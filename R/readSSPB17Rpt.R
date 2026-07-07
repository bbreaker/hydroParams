readSSPB17Rpr <- function(outFile) {
  
  if (!require("stringr", quietly = TRUE)) {
    warning("The 'stringr' package is needed. Please install it: install.packages('stringr')")
    return(NULL)
  }
  
  lines <- readLines(outFile, warn = FALSE)
  
  # Helper to find blocks of text based on start and end patterns
  find_block <- function(lines, start_pattern, end_pattern, header_offset = 0) {
    start_idx <- grep(start_pattern, lines)
    if (length(start_idx) == 0) return(NULL)
    start_idx <- start_idx[1] + header_offset
    end_idx <- grep(end_pattern, lines[start_idx:length(lines)])
    if (length(end_idx) == 0) {
      blank_lines <- which(lines[start_idx:length(lines)] == "")
      if (length(blank_lines) > 0) {
        end_idx <- start_idx + blank_lines[1] - 2
      } else {
        end_idx <- length(lines)
      }
    } else {
      end_idx <- start_idx + end_idx[1] - 2
    }
    if (start_idx > end_idx) return(NULL)
    return(lines[start_idx:end_idx])
  }
  
  # Helper to extract a numeric value from a named line in a block
  get_stat_value <- function(stat_lines, stat_name) {
    line <- stat_lines[grep(stat_name, stat_lines)]
    if (length(line) == 0) return(NA)
    if (length(unlist(stringr::str_split(line, "\\|"))) > 1) {
      line <- unlist(stringr::str_split(line, "\\|"))
      line <- line[grepl(stat_name, line)]
    }
    # Extract the last numeric value on the line
    val <- unlist(stringr::str_extract_all(line[1], "-?[0-9,]+\\.?[0-9]*"))
    val <- as.numeric(gsub(",", "", val))
    return(as.numeric(val))
  }
  
  # --- 1. Get 'info' table ---
  stats_block <- find_block(lines, "<< Systematic Statistics >>", "--- End of Analytical Frequency Curve ---")
  
  systematic_events <- get_stat_value(stats_block, "Systematic Events")
  low_outliers <- get_stat_value(stats_block, "Low Outliers")
  high_outliers <- get_stat_value(stats_block, "High Outliers")
  
  nPeaks <- systematic_events
  nPeaksUsed <- systematic_events - low_outliers - high_outliers
  
  # Determine the skew option by comparing adopted skew with others
  station_skew <- get_stat_value(stats_block, "Station Skew")
  regional_skew_val <- get_stat_value(stats_block, "Regional Skew")
  weighted_skew <- get_stat_value(stats_block, "Weighted Skew")
  adopted_skew <- get_stat_value(stats_block, "Adopted Skew")
  
  skewOption <- "UNKNOWN"
  if (!is.na(adopted_skew)) {
    # We round slightly to prevent floating point comparison errors
    if (isTRUE(all.equal(adopted_skew, station_skew))) {
      skewOption <- "STATION"
    } else if (isTRUE(all.equal(adopted_skew, regional_skew_val))) {
      skewOption <- "REGIONAL"
    } else if (isTRUE(all.equal(adopted_skew, weighted_skew))) {
      skewOption <- "WEIGHTED"
    }
  }
  
  info_df <- data.frame(
    nPeaks = nPeaks,
    nPeaksUsed = nPeaksUsed,
    skewOption = skewOption,
    stringsAsFactors = FALSE
  )
  
  # --- 2. Get 'parms' table (Parsed by trailing whitespace delimiters) ---
  parms_block <- find_block(lines, "Fitted log10 Moments", "-----------------", header_offset = 2)
  
  mu <- numeric(); sigma <- numeric(); gamma <- numeric()
  for (line in parms_block) {
    line <- trimws(line)
    if (line == "") next
    tokens <- strsplit(line, "\\s+")[[1]]
    n <- length(tokens)
    if (n >= 5) {
      # The last 4 tokens are Mean, Variance, Std Dev, Skew
      mu <- c(mu, as.numeric(tokens[n-3]))
      sigma <- c(sigma, as.numeric(tokens[n-1]))
      gamma <- c(gamma, as.numeric(tokens[n]))
    }
  }
  
  parms_df <- data.frame(
    type = c("STATION", "GENERAL_B17B", "GENERAL_EMA")[1:length(mu)], 
    mu = mu, 
    sigma = sigma, 
    gamma = gamma, 
    stringsAsFactors = FALSE
  )
  
  # --- 3. Get 'mse' table ---
  regional_skew_line <- lines[grep("^Regional Skew:", lines)]
  regional_skew <- as.numeric(trimws(strsplit(regional_skew_line[1], ":")[[1]][2]))
  
  regional_skew_mse_line <- lines[grep("^Regional Skew MSE:", lines)]
  mse_regional_gamma <- as.numeric(trimws(strsplit(regional_skew_mse_line[1], ":")[[1]][2]))
  
  gamma_est_ema <- parms_df$gamma[parms_df$type == "STATION"]
  if (length(gamma_est_ema) == 0) gamma_est_ema <- NA
  
  weighted_skew_line <- lines[grep("Weighted Skew", lines)]
  weighted_gamma <- as.numeric(stringr::str_extract(weighted_skew_line[1], "-?[0-9]+\\.[0-9]+"))
  
  mse_atsite_line <- lines[grep("EMA Estimate of MSE", lines)]
  mse_est_ema <- as.numeric(stringr::str_extract(mse_atsite_line[1], "[0-9]+\\.[0-9]+"))
  
  erl_line <- lines[grep("Equivalent Record Length \\[G at-site\\]", lines)]
  erl <- as.numeric(stringr::str_extract(erl_line[1], "[0-9]+\\.[0-9]+"))
  
  mse_df <- data.frame(
    estimate_type = c("gamma_Est_EMA", "regional_gamma", "MSE_regional_gamma", 
                      "weighted_gamma", "MSE_atSite_Systematic", "erl_gamma_atSite"),
    Value = c(gamma_est_ema, regional_skew, mse_regional_gamma, 
              weighted_gamma, mse_est_ema, erl),
    stringsAsFactors = FALSE
  )
  
  # --- 4. Get 'estimates' table ---
  estimates_block <- find_block(lines, "^\\|   Computed", "^Non-monotonically", header_offset = 3)
  
  rows_est <- lapply(estimates_block, function(line) {
    if (!grepl("\\|", line)) return(NULL)
    parts <- strsplit(line, "\\|")[[1]]
    
    # Needs at least 4 elements since splitting drops the empty right trailing pipe 
    if (length(parts) >= 4) {
      block1 <- strsplit(trimws(parts[2]), "\\s+")[[1]] # Computed, Variance, Expected
      block2 <- strsplit(trimws(parts[3]), "\\s+")[[1]] # AEP (Percent Chance)
      block3 <- strsplit(trimws(parts[4]), "\\s+")[[1]] # Conf Limits (lwr, upr)
      
      if (length(block1) >= 3 && length(block2) >= 1 && length(block3) >= 2) {
        emaCurve <- as.numeric(gsub(",", "", block1[1]))
        variance <- as.numeric(gsub(",", "", block1[2]))
        expected <- as.numeric(gsub(",", "", block1[3]))
        AEP <- as.numeric(gsub(",", "", block2[1]))
        lwr <- as.numeric(gsub(",", "", block3[1]))
        upr <- as.numeric(gsub(",", "", block3[2]))
        
        return(data.frame(AEP = AEP / 100, emaCurve = emaCurve, variance = variance, 
                          expected = expected, lwr = lwr, upr = upr, stringsAsFactors = FALSE))
      }
    }
    return(NULL)
  })
  
  estimates_df <- do.call(rbind, rows_est)
  if (is.null(estimates_df)) {
    estimates_df <- data.frame(AEP=numeric(), emaCurve=numeric(), variance=numeric(), expected=numeric(), lwr=numeric(), upr=numeric())
  }
  
  # --- 5. Get 'plotPositions' table ---
  pp_block <- find_block(lines, "Events Analyzed", "\\* Low outlier", header_offset = 3)
  
  rows_pp <- lapply(pp_block, function(line) {
    if (!grepl("\\|", line)) return(NULL)
    parts <- strsplit(line, "\\|")[[1]]
    
    # Needs at least 3 elements from splitting a line with two inner blocks
    if (length(parts) >= 3) {
      left_tokens <- strsplit(trimws(parts[2]), "\\s+")[[1]]
      right_tokens <- strsplit(trimws(parts[3]), "\\s+")[[1]]
      
      if (length(left_tokens) >= 4 && length(right_tokens) >= 4) {
        obsPkFlow <- as.numeric(gsub(",", "", left_tokens[4]))
        watYr <- as.integer(right_tokens[2])
        hspp <- as.numeric(right_tokens[4]) / 100
        
        return(data.frame(watYr = watYr, hspp = hspp, obsPkFlow = obsPkFlow, 
                          comment = "Peak Used", stringsAsFactors = FALSE))
      }
    }
    return(NULL)
  })
  
  plotpositions_df <- do.call(rbind, rows_pp)
  if (is.null(plotpositions_df)) {
    plotpositions_df <- data.frame(watYr=integer(), hspp=numeric(), obsPkFlow=numeric(), comment=character())
  }
  
  # --- Assemble the final list ---
  final_list <- list(
    info = info_df,
    parms = parms_df,
    mse = mse_df,
    estimates = estimates_df,
    plotPositions = plotpositions_df
  )
  
  return(final_list)
}

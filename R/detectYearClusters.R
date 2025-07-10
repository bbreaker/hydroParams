detectYearClusters <- function(dates, range_threshold = 2, min_group_size = 2) {
  # Sort input
  years_all <- dates
  years_unique <- sort(unique(years_all))
  n <- length(years_unique)
  
  # Initialize variables
  breaks <- c()
  i <- 1
  
  while (i <= n) {
    j <- i
    while (j < n && (max(years_unique[i:(j+1)]) - min(years_unique[i:(j+1)])) <= range_threshold) {
      j <- j + 1
    }
    if ((j - i + 1) >= min_group_size) {
      # Found a cluster
      breaks <- c(breaks, years_unique[i])
      i <- j + 1
    } else {
      # No cluster: treat year[i] as its own group
      breaks <- c(breaks, years_unique[i])
      i <- i + 1
    }
  }
  
  # Cut into groups
  cut_breaks <- c(-Inf, sort(unique(breaks)), Inf)
  labels <- paste("Group", seq_along(cut_breaks[-1]))
  group_labels <- cut(dates, breaks = cut_breaks, labels = labels, right = FALSE)
  return(group_labels)
}


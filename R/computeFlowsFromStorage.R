computeFlowsFromStorage <- function(storage, timestamps) {
  if (length(storage) != length(timestamps)) {
    stop("Storage and timestamps must have the same length.")
  }
  # Convert timestamps to POSIXct if not already
  timestamps <- as.POSIXct(timestamps)
  # Compute time differences in seconds
  time_diff_sec <- diff(as.numeric(timestamps))
  # Compute change in storage (ΔS)
  delta_storage_af <- diff(storage)
  # Conversion factor: 1 acre-foot = 43,560 cubic feet
  flows_cfs <- (delta_storage_af * 43560) / time_diff_sec
  # Return a data frame with timestamps and computed releases
  return(data.frame(
    index = timestamps[-1],  # Exclude first timestamp (no release computed for it)
    value = flows_cfs
  ))
}
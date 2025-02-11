# create a function to transform rainfall in to runoff using Clark Unit Hydrographs
# Requires hourly rainfall time-series
clarkUH <- function(rainfall, dt, Tc, R, area_sqmi) {
  # Constants
  cfs_per_inch_per_sqmi <- 640   # Conversion factor (1 in/hr over 1 sqmi = 640 cfs)
  # Time vector
  n <- length(rainfall)
  time <- seq(0, (n - 1) * dt, by = dt)
  # Clark Unit Hydrograph Components
  S_curve <- numeric(n) # S-hydrograph
  for (i in 1:n) {
    S_curve[i] <- 1 - exp(-time[i] / R) * (1 + time[i] / R)
  }
  # Unit hydrograph calculation (derivative of S curve)
  U <- numeric(n)
  for (i in 2:n) {
    U[i] <- max((S_curve[i] - S_curve[i - 1]) / dt, 0)
  }
  # Convolution of rainfall with unit hydrograph to get runoff hydrograph
  hydrograph <- numeric(n)
  for (i in 1:n) {
    for (j in 1:i) {
      hydrograph[i] <- hydrograph[i] + rainfall[j] * U[i - j + 1]
    }
  }
  # Convert to cfs using drainage area
  hydrograph_cfs <- hydrograph * cfs_per_inch_per_sqmi * area_sqmi
  # Return hydrograph time series
  data.frame(Time = time, Runoff_CFS = hydrograph_cfs)
}

# Apply the function
rainfall <- c(rep(0, 5), 0.05, 0.01, 0.2, 0.5, 1.2, 0.8, 0.6, 0.1, rep(0, 6)) # inches per hour 
dt <- 1  # Time step in hours
Tc <- 0.5  # Time of concentration in hours... this should be adjusted for site
R <- 0.25 # Storage coefficient in hours... this should be adjusted for site
area_sqmi <- 0.1  # Watershed area in square miles... this should be adjusted for site

# run the function created above
hydrograph <- clarkUH(rainfall, dt, Tc, R, area_sqmi)
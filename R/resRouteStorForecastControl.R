## 'inflow_cfs_series' 
##  - numeric vector of hourly inflows in cfs
## 'geometry_curve'
##  - data frame that contains 3 columns... 'elevation', 'storage', and 'discharge'
## 'initial_storage_af' 
##  - single numeric value in acre-feet that represents the storage value at the 
##    beginning of the simulation
## 'forecast_hours' 
##  - single numeric value in hours that represents the look-ahead time used to 
##    determine if the inflows will create elevations greater than 'elevation_max'
## 'elevation_max'   
##  - single numeric value in feet that represents the elevation threshold below from which 
##    the 'max_ramp_rate_cfs' applies to changes in releases
## 'max_ramp_rate' - 
##  - single numeric value in cfs that represents the maximum change in discharge that can 
##    occur per hour unless the 'max_discharge_cfs' is exceeded
## 'fixed_discharge_ts' 
##  - numeric vector of hourly discharge values that can be shorter than the
##    length of the 'inflow_series_cfs' vector
##  - if the vector is the same length as 'inflow_time_series', the 'fixed_discharge_ts' 
##    will be used for releases for the entire simulation
##  - if the vector is shorter than 'inflow_time_series', the specified discharge values
##    are used until they and and the rules take over
## 'ramp_rate_override_elevation' 
##  - single numeric value in feet that specifies an elevation above which the 'max_ramp_rate_cfs' 
##    is disregarded and the geometry curve is used to compute discharges, elevations, and etc 
## 'discharge_update_interval'
##  - single numeric value in hours representing time in between discharge changes in discharges
##  - intended to mimic rate at which gate changes can be made
## 'discharge_update_interval_override_elevation'
##  - single numeric value in feet that specifies an elevation at which the 'discharge_update_interval'
##    goes back to the time step of the inflows 

resRouteStorForecastControl <- function(inflow_cfs_series, 
                                        geometry_curve, 
                                        initial_storage_af, 
                                        forecast_hours = 24, 
                                        elevation_max = 1132.5, 
                                        elevation_target = 1130, 
                                        max_discharge_cfs = 43000, 
                                        max_ramp_rate_cfs = 30000, 
                                        fixed_discharge_ts = NULL, 
                                        ramp_rate_override_elevation = Inf, 
                                        discharge_update_interval = 1, 
                                        discharge_update_interval_override_elevation = NULL) {
  timestep_sec <- 3600
  ft3_to_af <- 1 / 43560
  n <- length(inflow_cfs_series)
  
  elevation_ft <- numeric(n)
  discharge_cfs <- numeric(n)
  storage_af <- numeric(n)
  
  storage_af[1] <- initial_storage_af
  elevation_ft[1] <- approx(geometry_curve$storage,
                            geometry_curve$elevation,
                            xout = initial_storage_af, rule = 2)$y
  base_discharge <- approx(geometry_curve$storage,
                           geometry_curve$discharge,
                           xout = initial_storage_af, rule = 2)$y
  discharge_cfs[1] <- if (!is.null(fixed_discharge_ts) && length(fixed_discharge_ts) >= 1) {
    fixed_discharge_ts[1]
  } else {
    base_discharge
  }
  
  for (t in 2:n) {
    override_update <- !is.null(discharge_update_interval_override_elevation) &&
      elevation_ft[t - 1] >= discharge_update_interval_override_elevation
    
    update_allowed <- ((t - 1) %% discharge_update_interval == 0) || override_update
    
    if (!is.null(fixed_discharge_ts) && t <= length(fixed_discharge_ts)) {
      discharge_cfs[t] <- fixed_discharge_ts[t]
    } else if (!update_allowed) {
      discharge_cfs[t] <- discharge_cfs[t - 1]
    } else {
      forecast_end <- min(t + forecast_hours - 1, n)
      inflow_forecast <- inflow_cfs_series[t:forecast_end]
      
      storage_test <- storage_af[t - 1]
      exceed_max <- FALSE
      for (h in 1:length(inflow_forecast)) {
        delta_storage <- inflow_forecast[h] * timestep_sec * ft3_to_af
        storage_test <- storage_test + delta_storage
        elev_test <- approx(geometry_curve$storage,
                            geometry_curve$elevation,
                            xout = storage_test, rule = 2)$y
        if (elev_test > elevation_max) {
          exceed_max <- TRUE
          break
        }
      }
      
      previous_discharge <- discharge_cfs[t - 1]
      previous_elevation <- elevation_ft[t - 1]
      proposed_discharge <- 0
      
      if (exceed_max) {
        proposed_discharge <- approx(geometry_curve$storage,
                                     geometry_curve$discharge,
                                     xout = storage_af[t - 1], rule = 2)$y
      } else {
        geom_discharge <- approx(geometry_curve$storage,
                                 geometry_curve$discharge,
                                 xout = storage_af[t - 1], rule = 2)$y
        max_possible_discharge <- min(geom_discharge, max_discharge_cfs)
        
        discharge_test_vals <- seq(max_possible_discharge, 0, by = -500)
        for (d in discharge_test_vals) {
          storage_sim <- storage_af[t - 1]
          under_target <- TRUE
          for (h in 1:length(inflow_forecast)) {
            net_inflow <- inflow_forecast[h] - d
            delta_storage <- net_inflow * timestep_sec * ft3_to_af
            storage_sim <- storage_sim + delta_storage
            elev_sim <- approx(geometry_curve$storage,
                               geometry_curve$elevation,
                               xout = storage_sim, rule = 2)$y
            if (elev_sim > elevation_target) {
              under_target <- FALSE
              break
            }
          }
          if (under_target) {
            proposed_discharge <- d
            break
          }
        }
      }
      
      if (previous_elevation > ramp_rate_override_elevation) {
        discharge_cfs[t] <- proposed_discharge
      } else {
        max_up <- previous_discharge + max_ramp_rate_cfs
        max_down <- previous_discharge - max_ramp_rate_cfs
        discharge_cfs[t] <- max(min(proposed_discharge, max_up), max_down)
      }
    }
    
    # Update storage and elevation
    net_inflow <- inflow_cfs_series[t - 1] - discharge_cfs[t]
    delta_storage <- net_inflow * timestep_sec * ft3_to_af
    storage_af[t] <- storage_af[t - 1] + delta_storage
    elevation_ft[t] <- approx(geometry_curve$storage,
                              geometry_curve$elevation,
                              xout = storage_af[t], rule = 2)$y
  }
  
  return(data.frame(hour = 1:n,
                    inflow_cfs = inflow_cfs_series,
                    discharge_cfs = discharge_cfs,
                    elevation_ft = elevation_ft))
}

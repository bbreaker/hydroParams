resRouteStorForecastControl_fixed <- function(inflow_cfs_series,
                                              geometry_curve,
                                              initial_storage_af,
                                              forecast_hours = 24,
                                              elevation_max = 1132.5,
                                              elevation_target = 1130,
                                              target_elevation = NULL,
                                              max_discharge_cfs = 43000,
                                              min_discharge_cfs = 0,
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
    max(min_discharge_cfs, fixed_discharge_ts[1])
  } else {
    max(min_discharge_cfs, base_discharge)
  }
  
  for (t in 2:n) {
    override_update <- !is.null(discharge_update_interval_override_elevation) &&
      elevation_ft[t - 1] >= discharge_update_interval_override_elevation
    update_allowed <- ((t - 1) %% discharge_update_interval == 0) || override_update
    
    # Store the previous discharge before it is updated
    previous_discharge <- discharge_cfs[t - 1]
    
    if (!is.null(fixed_discharge_ts) && t <= length(fixed_discharge_ts)) {
      discharge_cfs[t] <- max(min_discharge_cfs, fixed_discharge_ts[t])
    } else if (!update_allowed) {
      discharge_cfs[t] <- previous_discharge
    } else {
      forecast_end <- min(t + forecast_hours - 1, n)
      inflow_forecast <- inflow_cfs_series[t:forecast_end]
      
      storage_test <- storage_af[t - 1]
      exceed_max <- FALSE
      forecast_elevations <- numeric(length(inflow_forecast))
      
      for (h in 1:length(inflow_forecast)) {
        delta_storage <- inflow_forecast[h] * timestep_sec * ft3_to_af
        storage_test <- storage_test + delta_storage
        elev_test <- approx(geometry_curve$storage,
                            geometry_curve$elevation,
                            xout = storage_test, rule = 2)$y
        forecast_elevations[h] <- elev_test
        if (elev_test > elevation_max) {
          exceed_max <- TRUE
          break
        }
      }
      
      previous_elevation <- elevation_ft[t - 1]
      proposed_discharge <- 0
      
      if (exceed_max) {
        proposed_discharge <- approx(geometry_curve$storage,
                                     geometry_curve$discharge,
                                     xout = storage_af[t - 1], rule = 2)$y
      } else if (!is.null(target_elevation)) {
        geom_discharge <- approx(geometry_curve$storage,
                                 geometry_curve$discharge,
                                 xout = storage_af[t - 1], rule = 2)$y
        max_possible_discharge <- min(geom_discharge, max_discharge_cfs)
        
        # *** FIX #1 START: Ensure min_discharge_cfs is always tested ***
        # The original sequence could step over the specified minimum discharge.
        # This new approach creates the sequence and then adds min_discharge_cfs to it,
        # ensuring it's always a candidate for the best discharge.
        test_seq <- seq(max_possible_discharge, min_discharge_cfs, by = -500)
        discharge_test_vals <- unique(sort(c(test_seq, min_discharge_cfs), decreasing = TRUE))
        # *** FIX #1 END ***
        
        best_discharge <- NA
        min_diff <- Inf
        
        for (d in discharge_test_vals) {
          storage_sim <- storage_af[t - 1]
          valid <- TRUE
          
          for (h in 1:length(inflow_forecast)) {
            net_inflow <- inflow_forecast[h] - d
            delta_storage <- net_inflow * timestep_sec * ft3_to_af
            storage_sim <- storage_sim + delta_storage
            elev_sim <- approx(geometry_curve$storage,
                               geometry_curve$elevation,
                               xout = storage_sim, rule = 2)$y
            if (elev_sim > elevation_max) {
              valid <- FALSE
              break
            }
          }
          
          if (valid) {
            final_elev <- approx(geometry_curve$storage,
                                 geometry_curve$elevation,
                                 xout = storage_sim, rule = 2)$y
            diff <- abs(final_elev - target_elevation)
            if (diff < min_diff) {
              min_diff <- diff
              best_discharge <- d
            }
          }
        }
        
        if (!is.na(best_discharge)) {
          proposed_discharge <- best_discharge
        } else {
          proposed_discharge <- approx(geometry_curve$storage,
                                       geometry_curve$discharge,
                                       xout = storage_af[t - 1], rule = 2)$y
        }
      } else {
        proposed_discharge <- approx(geometry_curve$storage,
                                     geometry_curve$discharge,
                                     xout = storage_af[t - 1], rule = 2)$y
      }
      
      # Apply ramp rate
      if (previous_elevation > ramp_rate_override_elevation) {
        discharge_cfs[t] <- proposed_discharge
      } else {
        max_up <- previous_discharge + max_ramp_rate_cfs
        max_down <- previous_discharge - max_ramp_rate_cfs
        ramped_discharge <- max(min(proposed_discharge, max_up), max_down)
        discharge_cfs[t] <- max(min_discharge_cfs, ramped_discharge)
      }
    }
    
    # *** FIX #2 START: Corrected water balance calculation ***
    # The original method used I(t-1) and O(t), which is physically inconsistent.
    # This uses the average inflow and outflow over the time step [t-1, t],
    # which is the standard method for level-pool routing.
    avg_inflow <- (inflow_cfs_series[t - 1] + inflow_cfs_series[t]) / 2
    avg_outflow <- (previous_discharge + discharge_cfs[t]) / 2
    delta_storage <- (avg_inflow - avg_outflow) * timestep_sec * ft3_to_af
    storage_af[t] <- storage_af[t - 1] + delta_storage
    # *** FIX #2 END ***
    
    # Ensure storage doesn't go below zero
    if (storage_af[t] < 0) {
        storage_af[t] = 0
    }
    
    elevation_ft[t] <- approx(geometry_curve$storage,
                              geometry_curve$elevation,
                              xout = storage_af[t], rule = 2)$y
  }
  
  return(data.frame(hour = 1:n,
                    inflow_cfs = inflow_cfs_series,
                    discharge_cfs = discharge_cfs,
                    elevation_ft = elevation_ft,
                    storage_af = storage_af))
}

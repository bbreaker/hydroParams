fixPkFlowFiles <- function(pkFile) {
  
  pkFile$peak_dt <- if_else(stringr::str_sub(pkFile$peak_dt, start = 9, end = 10) == "00", 
                            paste0(stringr::str_sub(pkFile$peak_dt, 1, 8), "01"), pkFile$peak_dt) 
  
  pkFile$peak_dt <- if_else(stringr::str_sub(pkFile$peak_dt, start = 6, end = 7) == "00", 
                            paste0(stringr::str_sub(pkFile$peak_dt, 1, 5), "06", 
                                   stringr::str_sub(pkFile$peak_dt, 8, 10)), pkFile$peak_dt) 
  
  pkFile <- pkFile %>% 
    dplyr::mutate(peak_dt = as.POSIXct(peak_dt, format = "%Y-%m-%d")) %>% 
    dplyr::mutate(yr = as.numeric(format(peak_dt, '%Y')), 
                  mn = as.numeric(format(peak_dt, "%m"))) %>% 
    dplyr::mutate(watYr = as.character(as.numeric(yr) + 
                                         if_else(as.numeric(mn) < 10, 0, 1))) %>% 
    dplyr::group_by(watYr) %>% 
    dplyr::mutate(pkFlow = as.numeric(peak_va), 
                  gage_ht = as.numeric(gage_ht)) %>% 
    ungroup() %>% 
    dplyr::mutate(watYr = as.numeric(watYr)) %>% 
    dplyr::select(peak_dt, pkFlow, peak_cd, gage_ht, watYr) %>% 
    data.frame()
  
  return(pkFile)
  
}
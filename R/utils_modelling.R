
#' Filter the time-series to a time-frame defined by time_unit_extent
#' Fill the gaps in the time-series, assuming that at missing date, cases and deaths = 0 
#' Smoothing using moving average with a parameterable time-window (in days)
#' Model time-series using a linear regression

linear_model_cnt <- function(dta, iso_country = NULL, series, last_date, time_window = 12, ma_window = 3, min_sum = 30){
  
  dates_extent <- c(last_date - (time_window - 1), last_date)
  
  country_id <- dta %>% distinct(iso_a3, country)
  
  if (!is.null(iso_country)) {
    dta <- dta %>% 
      filter(iso_a3 %in% iso_country)
  } 
  
  # Fill gaps in time-series
  dta <- dta %>% 
    select(date, sym(series)) %>% 
    filter(between(date, dates_extent[1], dates_extent[2])) %>% 
    tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                    max(date, na.rm = TRUE), by = 1), 
                    fill = list(cases = NA_real_, deaths = NA_real_))
  
  if (dim(dta)[1] > ma_window & sum(dta[series], na.rm = TRUE) > min_sum) {
      
    dta$ma <- forecast::ma(dta[series], order = ma_window) %>% na_if(0)
    
    mdl <- lm(log(ma) ~ date, data = dta)
    
    # Create matrix frame of predictions
    mdl_preds <- matrix(data = NA, 
                        nrow = dim(dta)[1], 
                        ncol = 3, 
                        dimnames = list(c(1:dim(dta)[1]), c('fit', 'lwr', 'upr')))
    
    # Fill matrix of predictions
    preds <- exp(predict(mdl, interval = 'confidence'))
    
    matched_rows <- match(rownames(preds), rownames(mdl_preds))
    matched_cols <- match(colnames(preds), colnames(mdl_preds))
    mdl_preds[matched_rows, matched_cols] <- preds
    mdl_preds <- as_tibble(mdl_preds)
    
    mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                         lwr   = confint(mdl)[2,1], 
                         upr   = confint(mdl)[2,2])
    
  } else {
    mdl <- NA_character_
    
    mdl_preds  <- tibble(fit = rep(NA_real_, dim(dta)[1]), 
                         lwr = rep(NA_real_, dim(dta)[1]), 
                         upr = rep(NA_real_, dim(dta)[1]))
    
    mdl_coeffs <- tibble(coeff = NA_real_, 
                         lwr   = NA_real_, 
                         upr   = NA_real_)
  }
  
  preds <- cbind(dta, mdl_preds)
  
  return(list(country_id = country_id, 
              model = mdl, 
              preds = preds, 
              coeffs = mdl_coeffs, 
              par = list(model_name = 'linear regression model', 
                         time_unit_sourced = dates_extent, 
                         time_unit_modelled = time_window - (ma_window - 1), 
                         moving_average_extent = ma_window, 
                         minimum_observations_sum = min_sum)))
}





#' To calculate the doubling time based on the trends
doubling_time <- function(tbl_coeffs) {
  
  coeff <- tbl_coeffs$coeff
  lwr   <- tbl_coeffs$lwr
  upr   <- tbl_coeffs$upr
    
    if (!is.na(coeff) & all(lwr > 0, upr > 0)) {
      
      tbl_doubling_time <- tibble(est = log(2)/tbl_coeffs$coeff, 
                                  lwr = log(2)/tbl_coeffs$upr, 
                                  upr = log(2)/tbl_coeffs$lwr)
    } else {
      tbl_doubling_time <- tibble(est = NA_real_, 
                                  lwr = NA_real_, 
                                  upr = NA_real_)
    }
  
  return(tbl_doubling_time)
}





ts_coeff <- function(series, lst_dta, time_unit_extent = 5, ma_window = 3, min_sum = 30){
  
  lst_coeffs  <- list()
  
  for (j in names(lst_dta)) {
    
    dta <- lst_dta[[j]] %>% 
      select(date, continent, region, country, country_ecdc, iso_a3, series) %>% 
      tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                      max(date, na.rm = TRUE), by = 1), 
                      fill = list(series = NA_real_))
    
    seq_dates <- if(length(dta$date) < time_unit_extent) {
      min(dta$date) 
    } else {
      seq.Date(min(dta$date, na.rm = TRUE) + (time_unit_extent - 1) / 2, 
               max(dta$date, na.rm = TRUE) - (time_unit_extent - 1) / 2, 
               by = 1)
    }
    
    dta <- dta %>% 
      mutate(
        mov_av = as.double(forecast::ma(dta[series], order = ma_window)) %>% 
          na_if(., 0)) # Replace 0 values with NA
    
    tbl_coeffs <- tibble(date  = as.character(), 
                         coeff = numeric(), 
                         lwr   = numeric(), 
                         upr   = numeric())
    
    for (i in as.character(seq_dates)) {
      
      temp <- dta %>% 
        filter(between(date, 
                       as.Date(i) - (time_unit_extent - 1) / 2, 
                       as.Date(i) + (time_unit_extent - 1) / 2))
      
      if (sum(!is.na(temp['mov_av'])) > 2 & sum(temp['mov_av'], na.rm = TRUE) > min_sum) {
        mdl  <- lm(log(mov_av) ~ date, data = temp)
        mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                             lwr   = confint(mdl)[2,1], 
                             upr   = confint(mdl)[2,2])
      } else {
        mdl_coeffs <- tibble(coeff = NA_real_, 
                             lwr   = NA_real_, 
                             upr   = NA_real_)
      }
      
      tbl_coeffs <- tbl_coeffs %>% 
        add_row(date = i, mdl_coeffs)
      
    }
    
    dta_coeff <- left_join(dta,  tbl_coeffs %>% mutate(date = as.Date(date)))
    
    lst_coeffs[[j]] <- dta_coeff
    
  }
  return(lst_coeffs)
}

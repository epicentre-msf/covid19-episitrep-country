#' Import world shapefile as sf object
#' 
#' @param scale detail on map polygons, one of 'small', 'medium', 'large'
#' @param proj projection of map, one of 'robinson' (default) or 'mercator'
#' 
#' @export
get_world_sf <- function(scale = c('small', 'medium', 'large'), proj = c('robinson', 'mercator'), update_data = FALSE) {
  
  library(sf)
  library(dplyr)
  
  
  file_location <- file.path(path.data, paste0('sf_world','.RDS'))
  
  if (update_data | !file.exists(file_location)) {
  
  scale <- match.arg(scale, several.ok = FALSE)
  proj  <- match.arg(proj, several.ok = FALSE)
  
  world_map_raw <- rnaturalearth::ne_countries(scale = scale, type = "countries", returnclass = "sf")
  
  if (proj == "robinson") {
    world_map_raw <- world_map_raw %>% sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84") 
  }
  
  world_map_raw <- world_map_raw %>% 
    tibble::as_tibble() %>% # for nicer printing
    sf::st_as_sf() %>% 
    dplyr::select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
    dplyr::mutate(
      continent = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent")),
      region = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"))
    ) %>% 
    dplyr::filter(stringr::str_detect(country, "Antarctic", negate = TRUE)) %>% 
    cbind(sf::st_coordinates(suppressWarnings(sf::st_centroid(., of_largest_polygon = TRUE)))) %>% 
    dplyr::rename(lon = X, lat = Y)
  
  saveRDS(world_map_raw, file = file_location)
  
  } else {
    
    world_map_raw <- readRDS(file_location)
  }
  
  return(world_map_raw)
  
}


#' Import ECDC dataset
#' 
#' @export
get_ecdc_data <- function() {
  
  base_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  
  d <- readr::read_csv(base_url)
  
}



get_countries_list <- function(update_data = FALSE) {
  
  file_location <- file.path(path.data, paste0('df_countries','.RDS'))
  
  
  if (update_data | !file.exists(file_location)) {
    
    dta_ecdc <- get_ecdc_data() %>% prepare_ecdc_dta()
    
    countries <- dta_ecdc %>% filter(!is.na(iso_a3)) %>% distinct_at(vars(continent, region, iso_a3, country, pop = population_2019))
    
    saveRDS(countries, file = file_location)
    
  } else {
    
    countries <- readRDS(file_location)
  }
  
  return(countries)
}




get_msf_country_linelist <- function(iso_country, update_data = FALSE){
  
  # Note: 'path.sharepoint' is defined in setup.R
  path.sharepoint.country.data <- file.path(path.sharepoint, 'data', 'linelist', 'country',iso_country)
  
  file_local <- file.path(path.local.country.week.data, paste0('dta_MSF_', iso_country, '_linelist', '.RDS'))
  
  if (update_data | !file.exists(file_local)) {
    
    dta_path <- max(fs::dir_ls(path.sharepoint.country.data, regexp = "[.]rds$"))
    dta <- readRDS(dta_path)
    saveRDS(dta, file_local)
    
  } else {
    
    dta <- readRDS(file_local)
  }
  
  return(dta)
  
}



get_msf_country_aggregated <- function(iso_country, update_data = FALSE) {
  
  file_local <- file.path(path.local.country.week.data, paste0('dta_MSF_', iso_country, '_aggregated', '.RDS'))
  
  if (update_data | !file.exists(file_local)) {
    
    # Note: 'path.sharepoint' is defined in setup.R
    path.sharepoint.agg.data <- file.path(path.sharepoint, "coordination", "Surveillance focal points coordination", "Aggregated reporting", "Report_covid_aggregate_all_v2.xlsx")
    
    dta_names <- c("sheet", "oc", "country", "project", "date", "week", "suspected", "probable", "confirmed", "non_cases", "unknown")
    
    
    dta_aggregated <- excel_sheets(path.sharepoint.agg.data) %>% 
      setdiff(., c('Feuil1','Sheet1')) %>% 
      map_df(~{
        oc      <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "B1", col_names = FALSE) %>% pull()
        country <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "D1", col_names = FALSE) %>% pull()
        project <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "F1", col_names = FALSE) %>% pull()
        
        read_excel(path = path.sharepoint.agg.data, sheet = .x, skip = 5, col_names = FALSE) %>% 
          mutate(sheet = .x, oc = oc, country = country, project = project)
      }) %>% 
      select(sheet, oc, country, project, 1:7) %>% 
      purrr::set_names(dta_names)
    
    
    dta_expanded <- dta_aggregated %>% 
      filter(!project %in% c('Lesvos-Moria', 'Lesvos-Moria Ped')) %>% 
      select(-week) %>% 
      mutate(
        epi_week_consultation = make_epiweek_date(date) %>% as.Date(), 
        country = case_when(
          country == 'DRC' ~ 'Democratic Republic of the Congo', 
          country == "Côte d'Ivoire" ~ 'Côte d’Ivoire', 
          TRUE ~ country)) %>% 
      left_join(df_countries, by = 'country') %>% 
      filter(iso_a3 == iso_country) %>% 
      pivot_longer(cols = c('confirmed', 'probable', 'suspected', 'non_cases', 'unknown'), names_to = 'covid_status') %>% 
      filter(!is.na(value)) %>% 
      mutate(obs = purrr::map(value, ~rep_len(1, .x))) %>%
      unnest(cols = c(obs)) %>%
      select(-c(value, obs)) %>% 
      mutate(
        covid_status = factor(covid_status, levels = c('confirmed', 'probable', 'suspected', 'non_cases', 'unknown'), labels = c('Confirmed', 'Probable', 'Suspected', 'Not a case', 'Unknown')), 
        country = paste(country, '(*)'),
        site_name = paste(project, '(*)')) 
    
    saveRDS(dta_expanded, file_local)
    
  } else {
    
    dta_expanded <- readRDS(file_local)
  }
  
  return(dta_expanded)
  
}





#' Import FIND test dataset
#' 
#' @export
get_find_data <- function() {
  
  base_url <- "https://finddx.shinyapps.io/FIND_Cov_19_Tracker/_w_989488b3/downloads/cv_tests_download.csv"
  
  d <- readr::read_csv(base_url)
  
}

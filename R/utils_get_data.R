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


# Paul's dashboard import code ===========================================

get_msf_country_linelist <- function(path, iso_country, update_data = FALSE) {
  age_cut <- c(seq(0, 80, 10), Inf)
  age_labs <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  df_linelist <- readr::read_rds(path) %>% 
    filter(country == iso_country) %>% 
    mutate(
      continent = countrycode::countrycode(country, "iso3c", "continent"),
      region = countrycode::countrycode(country, "iso3c", "region"),
      country_lab = countrycode::countrycode(country, "iso3c", "country.name"),
      country_lab = dplyr::case_when(
        country_lab == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", 
        country_lab == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country_lab),
      age_group = cut(age_in_years, breaks = age_cut, labels = age_labs, right = FALSE, include.lowest = TRUE),
      age_group = forcats::fct_explicit_na(age_group, na_level = "Unknown"),
      patcourse_presHCF = lubridate::as_date(patcourse_presHCF),
      outcome_patcourse_presHCF = lubridate::as_date(outcome_patcourse_presHCF),
      epi_week_consultation = lubridate::floor_date(lubridate::as_date(MSF_date_consultation), "week", week_start = 1)
    ) %>% 
    mutate_at(vars(contains("date")), lubridate::as_date) %>% 
    tidyr::replace_na(list(
      patinfo_sex = "Unknown",
      MSF_covid_status = "Unknown",
      patcourse_admit = "Unknown",
      outcome_patcourse_status = "Unknown",
      MSF_visit_type = "Unknown",
      age_in_years = "Unknown"
    )) %>% 
    select(1:3, continent, region, country_lab, everything())
}


get_msf_country_aggregated <- function(path, iso_country, update_data = TRUE) {
  
  # iso code for Central African Republic is wrong in the agg data excel 
  # so need to switch to the wrong version to read data in correctly then correct later
  iso_country <- if_else(iso_country == "CAF", "RCA", iso_country)
  sheets <- excel_sheets(path)
  country_sheets <- sheets[stringr::str_detect(sheets, iso_country)]
  
  if (length(country_sheets) < 1) return(NULL)
  
  agg_data_names <- c("sheet", "OC", "country_lab", "project", "date", "week", "Suspected", "Probable", "Confirmed", "Not a case", "Unknown")
  
  df_weekly_aggregated <- country_sheets %>% 
    map_df(~{
      oc      <- read_excel(path = path, sheet = .x, range = "B1", col_names = FALSE) %>% pull()
      country <- read_excel(path = path, sheet = .x, range = "D1", col_names = FALSE) %>% pull()
      project <- read_excel(path = path, sheet = .x, range = "F1", col_names = FALSE) %>% pull()
      
      read_excel(path = path, sheet = .x, skip = 5, col_names = FALSE) %>% 
        mutate(sheet = .x, OC = oc, country_lab = country, project = project)
    }) %>% 
    select(sheet, OC, country_lab, project, 1:7) %>% 
    set_names(agg_data_names) %>% 
    tidyr::separate(sheet, into = c("country", "site_name"), sep = "-", extra = "merge") %>% 
    mutate(
      country = stringr::str_trim(country), 
      country = case_when(country == "RCA" ~ "CAF", TRUE ~ country)
    ) %>% 
    filter(country == iso_country) %>% 
    mutate(
      site_name = paste(stringr::str_trim(site_name), "(*)"), 
      continent = countrycode::countrycode(country, "iso3c", "continent"),
      region = countrycode::countrycode(country, "iso3c", "region23"),
      country_lab = countrycode::countrycode(country, "iso3c", "country.name"),
      country_lab = dplyr::case_when(
        country_lab == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", 
        country_lab == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country_lab),
      date = lubridate::as_date(date),
      Total = purrr::pmap_dbl(list(Suspected, Probable, Confirmed, `Not a case`, Unknown), sum, na.rm = TRUE)
    ) %>% 
    select(continent, region, country, country_lab, site_name, project, everything())
}


get_msf_combined_data <- function(df_linelist, df_aggregated) {
  
  df_combined <- df_linelist %>%
    select(continent, country, country_lab, OC, site_name, date = MSF_date_consultation, MSF_covid_status) %>% 
    mutate(
      date = lubridate::floor_date(lubridate::as_date(date), "week", week_start = 1) # floor date by week to monday
    ) %>% 
    group_by_all() %>% 
    summarise(n = n()) %>% 
    add_tally(wt = n, name = "Total") %>% 
    ungroup() %>% 
    tidyr::spread(MSF_covid_status, n, fill = 0) %>% 
    select(-Total, everything())
  
  if (!is.null(df_aggregated)) {
    df_combined <- df_combined %>% 
      bind_rows(
        df_weekly_aggregated %>% 
          filter(!project %in% c('Lesvos-Moria', 'Lesvos-Moria Ped')) %>% 
          select(-region, -project, -week) %>% 
          mutate(date = lubridate::as_date(date))
      )
  }
  df_combined  %>% arrange(continent, country_lab, site_name, date)
}


#' Import FIND test dataset
#' 
#' @export
get_find_data <- function() {
  
  base_url <- "https://finddx.shinyapps.io/FIND_Cov_19_Tracker/_w_989488b3/downloads/cv_tests_download.csv"
  
  d <- readr::read_csv(base_url)
  
}



# get_msf_country_linelist <- function(iso_country, update_data = FALSE){
#   
#   # Note: 'path.sharepoint' is defined in setup.R
#   path.sharepoint.country.data <- file.path(path.sharepoint, 'data', 'linelist', 'country',iso_country)
#   
#   file_local <- file.path(path.local.country.week.data, paste0('dta_MSF_', iso_country, '_linelist', '.RDS'))
#   
#   if (update_data | !file.exists(file_local)) {
#     
#     dta_path <- max(fs::dir_ls(path.sharepoint.country.data, regexp = "[.]rds$"))
#     dta <- readRDS(dta_path)
#     saveRDS(dta, file_local)
#     
#   } else {
#     
#     dta <- readRDS(file_local)
#   }
#   
#   return(dta)
#   
# }
# 
# 
# 
# get_msf_country_aggregated <- function(iso_country, update_data = FALSE) {
#   
#   file_local <- file.path(path.local.country.week.data, paste0('dta_MSF_', iso_country, '_aggregated', '.RDS'))
#   
#   if (update_data | !file.exists(file_local)) {
#     
#     # Note: 'path.sharepoint' is defined in setup.R
#     path.sharepoint.agg.data <- file.path(path.sharepoint, "coordination", "Surveillance focal points coordination", "Aggregated reporting", "Report_covid_aggregate_all_v2.xlsx")
#     
#     dta_names <- c("sheet", "oc", "country", "project", "date", "week", "suspected", "probable", "confirmed", "non_cases", "unknown")
#     
#     
#     dta_aggregated <- excel_sheets(path.sharepoint.agg.data) %>% 
#       setdiff(., c('Feuil1','Sheet1')) %>% 
#       map_df(~{
#         oc      <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "B1", col_names = FALSE) %>% pull()
#         country <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "D1", col_names = FALSE) %>% pull()
#         project <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "F1", col_names = FALSE) %>% pull()
#         
#         read_excel(path = path.sharepoint.agg.data, sheet = .x, skip = 5, col_names = FALSE) %>% 
#           mutate(sheet = .x, oc = oc, country = country, project = project)
#       }) %>% 
#       select(sheet, oc, country, project, 1:7) %>% 
#       purrr::set_names(dta_names)
#     
#     
#     dta_expanded <- dta_aggregated %>% 
#       filter(!project %in% c('Lesvos-Moria', 'Lesvos-Moria Ped')) %>% 
#       select(-week) %>% 
#       mutate(
#         epi_week_consultation = make_epiweek_date(date) %>% as.Date(), 
#         country = case_when(
#           country == 'DRC' ~ 'Democratic Republic of the Congo', 
#           country == "Côte d'Ivoire" ~ 'Côte d’Ivoire', 
#           TRUE ~ country)) %>% 
#       left_join(df_countries, by = 'country') %>% 
#       filter(iso_a3 == iso_country) %>% 
#       pivot_longer(cols = c('confirmed', 'probable', 'suspected', 'non_cases', 'unknown'), names_to = 'covid_status') %>% 
#       filter(!is.na(value)) %>% 
#       mutate(obs = purrr::map(value, ~rep_len(1, .x))) %>%
#       unnest(cols = c(obs)) %>%
#       select(-c(value, obs)) %>% 
#       mutate(
#         covid_status = factor(covid_status, levels = c('confirmed', 'probable', 'suspected', 'non_cases', 'unknown'), labels = c('Confirmed', 'Probable', 'Suspected', 'Not a case', 'Unknown')), 
#         country = paste(country, '(*)'),
#         site_name = paste(project, '(*)')) 
#     
#     saveRDS(dta_expanded, file_local)
#     
#   } else {
#     
#     dta_expanded <- readRDS(file_local)
#   }
#   
#   return(dta_expanded)
#   
# }

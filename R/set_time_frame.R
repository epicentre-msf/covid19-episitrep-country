

# Set the term of dates this is also used to separate output files

date_max_report <- get_max_date_report() 

week_report <- ISOweek(date_max_report) %>% gsub("W","w", .)

path.sharepoint.country.plot  <- file.path(path.sharepoint, 'template', 'sitreps', week_report, 'worldwide', 'graphs', 'country_trends')
path.sharepoint.ecdc.analysis <- file.path(path.sharepoint, 'template', 'sitreps', week_report, 'worldwide', 'data')


path.local.country        <- file.path(path.local, name_country) 
path.local.country.week   <- file.path(path.local.country, week_report)
path.local.country.week.data   <- file.path(path.local.country.week, 'data')
path.local.country.week.graphs <- file.path(path.local.country.week, 'graphs')
path.local.country.week.tables <- file.path(path.local.country.week, 'tables')


dir.create(path.local.country, showWarnings = FALSE, recursive = TRUE) 

dir.create(path.local.country.week, showWarnings = FALSE, recursive = TRUE) 
dir.create(path.local.country.week.data, showWarnings = FALSE, recursive = TRUE) 
dir.create(path.local.country.week.graphs , showWarnings = FALSE, recursive = TRUE) 
dir.create(path.local.country.week.tables , showWarnings = FALSE, recursive = TRUE) 


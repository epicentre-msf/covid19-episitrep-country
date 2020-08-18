# Parameters --------------------------------------------------------------

# automatize this later
country <- "AFG"
name_country <- "Afghanistan"


# Set environment ---------------------------------------------------------

# Set environment
source(here::here('R', 'setup.r'), encoding = 'UTF-8')
source(file.path(path.R, "utils_get_data.R")  , encoding = "UTF-8")
source(file.path(path.R, "utils_management.R"), encoding = "UTF-8")
source(file.path(path.R, "utils_vis.R")       , encoding = "UTF-8")
source(file.path(path.R, "utils_epicurve.R")  , encoding = "UTF-8")
source(file.path(path.R, "utils_modelling.R") , encoding = "UTF-8")
source(file.path(path.R, "set_time_frame.R") , encoding = "UTF-8")

# Knit Markdown ----------------------------------------------------------------

run_analyes_country <- TRUE


if (run_analyes_country) {
  
  rmarkdown::render(input = file.path(path.Rmd, "rmd_html_country_episitrep.Rmd"), 
                    output_file = glue("{week_report}_draft_EpiSitrep_country_{country}_Covid-19.html"), 
                    output_dir  = path.local.country.week) 
} else {
  # load(file.path(path.local.worldwide.data, paste0('episitrep_worldwide_analyses', '_', week_report, '.RData')))
}




# Build EpiSitrep docx ----------------------------------------------------
source(file.path(path.R, 'utils_officer.R')   , encoding = 'UTF-8')


my_doc <- read_docx(file.path(path.templates, 'template_country_episitrep.docx'))

source(file.path(path.R, 'utils_officer.R'), encoding = 'UTF-8')

#styles_info(my_doc)

# Doc title
source(file.path(path.R, 'docx_section0_heading.R'), encoding = 'UTF-8')
source(file.path(path.R, 'docx_section1_worldwide_analyses.R'), encoding = 'UTF-8')
source(file.path(path.R, 'docx_section2_MSF_level_analyses.R'), encoding = 'UTF-8')

# Save docx ---------------------------------------------------------------
print(my_doc, target = file.path(path.local.country.week, glue("draft_EpiSitrep_country_{country}_Covid-19_{week_report}.docx")))



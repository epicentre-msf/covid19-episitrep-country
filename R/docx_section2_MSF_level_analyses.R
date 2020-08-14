# --- --- --- --- --- --- --- --- --- 
# II. MSF level analyses
# --- --- --- --- --- --- --- --- ---



# Heading 1
my_doc <- add_heading1('Analysis at MSF level') 

my_doc <- add_end_section_continuous()




# Descriptive text of the section
my_doc %<>% 
  body_add_fpar(style = 'Description', 
                fpar(ftext("This section of the report analyses data reported by MSF in the reporting system created by Epicentre and distributed to MSF projects. Data can also be visualized and explored in the Epicentre Linelist Dashboard (see information besides).", prop = calibri_8)))


#TODO, on a second column:

#Epicentre dashboard for covid19 linelist data
#- Go to this url to sign up with your MSF email address - Then consult the dashboard here - Navigate and use filters to display Linelist data of your interest



# Overview ----------------------------------------------------------------
my_doc <- add_heading2("Overview of reported data") 
my_doc <- add_end_section_continuous()


# Projects reporting data
my_doc <- add_heading3("Projects reporting data")

## Some text


# Table
my_doc <- add_table(
  object_name = glue("gtbl_general_characteristics_by_site_{name_country}_{week_report}.png"), 
  path = path.local.country.week.tables,
  table_title = glue('Summary of Covid-19 infection-related status of patients consulted/admitted'), 
  width = 16 * cm_to_in, 
  height = 5.01 * cm_to_in)

my_doc <- add_end_section_continuous()





# Number patients over time ----------------------------------------------------------------

# Heading 3
my_doc <- add_heading3("Number of patients consulted or admitted in MSF Covid-19 facilities and their the evolution over time")
my_doc <- add_end_section_continuous()


# Figure number of patients over time
my_doc <- add_figure(
  object_name  = glue('hist_epicurve_consult_admit_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  figure_title = glue("TO WRITE"))


# Figure number of patients over time - per sites
my_doc <- add_figure(
  object_name  = glue('hist_epicurve_consult_admit_site_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  width = 9 * cm_to_in,
  height = 9 * cm_to_in,
  figure_title = glue("TO WRITE"))




# Origin of patients ------------------------------------------------------
my_doc <- add_heading3("Origin of patients")

my_doc <- add_par_normal("Origin of the patients are displayed in the map below (figure 3).")


#### TODO: map


my_doc <- add_end_section_continuous()



# Major care procedures ---------------------------------------------------
my_doc <- add_heading3("Major cares procedures in MSF facilities")


my_doc <- add_par_normal("Overall, X patients were admitted in MSF facilities since the beginning of the epidemic, which represents Y% of the total patients who accessed to an MSF facility in the country. TODOOOOOOO")



# Table
my_doc <- add_table(
  object_name = glue("gtbl_consultation_admission_{name_country}_{week_report}.png"), 
  path = path.local.country.week.tables,
  table_title = glue('TO DO'), 
  width = 14 * cm_to_in, 
  height = 20 * cm_to_in)


# Figure epicurve admission
my_doc <- add_figure(
  object_name  = glue('hist_epicurve_consult_admit_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  width = 12 * cm_to_in,
  height = 7.5 * cm_to_in,
  figure_title = glue("TO WRITE"))


# Figure epicurve admission - per site
my_doc <- add_figure(
  object_name  = glue('hist_epicurve_consult_admit_site_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  figure_title = glue("TO WRITE"),
  width = 11 * cm_to_in,
  height = 10 * cm_to_in)

my_doc <- add_end_section_continuous()




# Testes ------------------------------------------------------------------
my_doc <- add_heading3("Covid tests")

### TODO





# Patient characteristics -------------------------------------------------

# Heading 2
my_doc <- add_heading2("Characteristics of patients consulted/admitted in MSF Covid-19 facilities") 


my_doc <- add_table(
  object_name = glue("gtbl_general_characteristics_{name_country}_{week_report}.png"), 
  path = path.local.country.week.tables,
  table_title = glue('TO DO'), 
  width = 17 * cm_to_in, 
  height = 9 * cm_to_in)



my_doc <- add_table(
  object_name = glue("gtbl_general_characteristics_by_site_{name_country}_{week_report}.png"), 
  path = path.local.country.week.tables,
  table_title = glue('TO DO'), 
  width = 17 * cm_to_in, 
  height = 5.01 * cm_to_in)


# Histogram symptoms
my_doc <- add_figure(
  object_name  = glue('plot_symptoms_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  width = 16 * cm_to_in, 
  height = 13 * cm_to_in,
  figure_title = glue("TO WRITE"))


my_doc <- add_table(
  object_name = glue("gtbl_cfr_status_site_{name_country}_{week_report}.png"), 
  path = path.local.country.week.tables,
  table_title = glue('TO DO'), 
  width = 16 * cm_to_in, 
  height = 5.01 * cm_to_in)



my_doc <- add_end_section_continuous()



# Description Confirmed/suspect/probable patients -------------------------

# Heading
my_doc <- add_heading2("Description of Covid19 [confirmed/probable/suspect] patients")


# Pyramide age and sex
my_doc <- add_figure(
  object_name  = glue('pyramid_age_sex_confirmed_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  width = 17 * cm_to_in, 
  height = 13 * cm_to_in,
  figure_title = glue("TO WRITE"))


# Pyramide age and sex - per site
my_doc <- add_figure(
  object_name  = glue('pyramid_age_sex_confirmed_site_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  width = 17 * cm_to_in, 
  height = 20 * cm_to_in,
  figure_title = glue("TO WRITE"))



# Type exposure -----------------------------------------------------------

# Header
my_doc <- add_heading3("Type of exposure to the Covid-19 infection")

# Text
my_doc <- add_par_normal("Overall, X patients recorded a type of exposure among patients received in MSF facilities.")



# Underlying conditions ---------------------------------------------------
my_doc <- add_heading3("Underlying conditions")

#TODO

# Level of care -----------------------------------------------------------
my_doc <- add_heading3("Level of care received")


#TODO



# Patient outcomes --------------------------------------------------------
my_doc <- add_heading3("Patients’ outcomes")

#TODO

# Other operational indicators --------------------------------------------

my_doc <- add_heading3("Other operational indicators")


# Dealy from sysmptoms to consultation/admission
my_doc <- add_par_normal("Delay from symptoms to consultation/admission")

my_doc <- add_par_normal("The overall delay from symptoms to consultation/admission in MSF facilities was of X days.")

# Figure Delay admission
my_doc <- add_figure(
  object_name  = glue('boxplot_delay_admission_week_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  figure_title = glue("TO WRITE"))



# Length of stay
my_doc %<>% body_add_par(style = 'Description bold',
                         value = "Length of stay")

my_doc <- add_par_normal("The overall length of stay in MSF facilities was of X days")

# Figure length of stay
my_doc <- add_figure(
  object_name  = glue('boxplot_length_stay_week_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  figure_title = glue("TO WRITE"))


# Bed occupancy rates
my_doc %<>% body_add_par(style = 'Description bold',
                         value = "Bed occupancy rates")

#TODO
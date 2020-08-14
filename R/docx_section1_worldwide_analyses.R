# --- --- --- --- --- --- --- --- --- 
# II. External data
# --- --- --- --- --- --- --- --- ---


# Heading 1
my_doc <- add_heading1(heading_text = 'Epidemiological situation in Afghanistan')



# Case counts and trends ----------------------------------------

# Heading 2
my_doc <- add_heading2(heading_text = 'Evolution in cases and trends (external data)') 

my_doc <- add_end_section_continuous()



# Figure of cases and deaths, with trends
my_doc <- add_figure(
  object_name  = glue('multiplot_counts_trends_{name_country}_{week_report}.png'),
  path = path.local.country.week.graphs,
  width = 11 * cm_to_in,
  height = 7 * cm_to_in,
  figure_title = glue("Covid-19 cases and deaths and trend estimations in {name_country}"))


## - Table of cases and deaths, with trends
my_doc <- add_table(
  object_name = glue("gtbl_summary_country_{name_country}_{week_report}.png"), 
  path = path.local.country.week.tables,
  table_title = glue('Summary cases and deaths and evolution of the epidemic in {name_country} in the last 30 and 12 days'), 
  width = 16 * cm_to_in, 
  height = 4 * cm_to_in)


my_doc <- add_end_section_continuous()




# Tests and measures ------------------------------------------------------

## - Heading 2
my_doc <- add_heading2(heading_text = 'Country situation in surveillance, social measures and testing (external data)') 

my_doc <- add_end_section_continuous()


#TODO




my_doc <- add_end_section_continuous()

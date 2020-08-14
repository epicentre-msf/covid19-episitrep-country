# Title -------------------------------------------------------------------


my_doc %<>% 
  body_add_par(style = 'Title', 
               value = 'MSF EpiSitrep on Covid-19 epidemic',
               pos = "before") %>% 
  body_add_par(style = 'Subtitle', 
               value = glue("{week_report} {name_country}"))



# Introduction and disclaimers --------------------------------------------

my_doc %<>% 
  # First paragraph
  body_add_par(style = 'Description', 
               value = "This document presents tables, graphs and maps of the MSF Covid-19 linelists analyses for a single country (not much text). Most, but not all, of these analyses are presented and commented in the MSF Intersection Covid-19 EpiSitrep edited by Epicentre (for information on the EpiSitrep, please contact Anaïs Broban (") %>% 
  
  slip_in_text(style = 'Hyperlink', 
               str = 'Anais.BROBAN@epicentre.msf.org', 
               hyperlink = 'mailto:Anais.BROBAN@epicentre.msf.org') %>% 
  
  slip_in_text(style = 'Description char', 
               str = ').') %>% 
  
  # Second paragraph
  body_add_par(style = 'Description', 
               value = "The R scripts for both country and MSF linelist analyses are provided on the following Github repository (") %>% 
  
  slip_in_text(style = 'Hyperlink', 
               str = "https://github.com/epicentre-msf/covid19-episitrep-country", 
               hyperlink = "https://github.com/epicentre-msf/covid19-episitrep-country") %>%
  slip_in_text(style = 'Description char', 
               str =  ") for epidemiologists in MSF to use (R skills required). The scripts will be updated from week to week as analysis progresses as requested./n") %>% 
  
  
  # Line to separate
  body_add_par(style = 'Horizontal line', 
               value = '') %>% 
  body_end_section_continuous()



# Authors and publication ------------------------------------------

my_doc %<>%
  
  # Authors
  body_add_par(style = 'Description bold',
               value = "Authors") %>%
  
  body_add_par(style = 'Description bullet',
               value = "") %>%
  
  slip_in_text(style = 'Hyperlink',
               str = 'Francesco Grandesso',
               hyperlink = 'mailto:francesco.GRANDESSO@epicentre.msf.org') %>%
  
  body_add_par(style = 'Description bullet',
               value = "") %>% 
  slip_in_text(style = 'Hyperlink',
               str = 'Paul Campbell',
               hyperlink = 'mailto:paul.CAMPBELLepicentre.msf.org')



my_doc %<>%
  # Affiliation
  body_add_par(style = 'Description bold',
               value = "Affiliation") %>%
  slip_in_column_break(pos = 'before') %>% 
  body_add_par(style = 'Description',
               value = "Epicentre")


my_doc %<>%
  # Publication
  body_add_par(style = 'Description bold',
               value = "Publication date") %>%
  slip_in_column_break(pos = 'before') %>% 
  
  body_add_par(style = 'Description',
               value = Sys.Date())



my_doc <- add_end_section_3columns()


# Line
my_doc %<>% 
  body_add_par(style = 'Horizontal line', 
               value = '') %>% 
  body_end_section_continuous()



# Data sources and links --------------------------------------------------

my_doc %<>% 
  body_add_par(style = 'Description bold', 
               value = "Data sources") %>% 
  
  body_add_par(style = 'Description bullet', 
               value = "") %>% 
  
  slip_in_text(style = 'Hyperlink', 
               str = 'ECDC data', 
               hyperlink = 'https://opendata.ecdc.europa.eu/covid19/casedistribution/csv') %>% 
  
  slip_in_text(style = 'Description char', 
               str = glue(' last updated {format(max(dta_ecdc$date, na.rm = TRUE), "%d %b %Y")}')) %>% 
  
  body_add_par(style = 'Description bullet', 
               value = 'MSF linelists compiled by Epicentre') %>% 
  
  body_add_par(style = 'Description bullet', 
               value = 'MSF ') %>% 
  slip_in_text(style = 'Hyperlink', 
               str = 'GIS Unit', 
               hyperlink = 'https://mapcentre.msf.org') %>% 
  slip_in_text(style = 'Description char', 
               str = glue(' (baseline country maps)')) %>% 
  
  body_add_par(style = 'Description bullet', 
               value = 'FIND ') %>% 
  slip_in_text(style = 'Hyperlink', 
               str = 'Diagnostics resource centre Unit', 
               hyperlink = 'https://www.finddx.org/covid-19/') %>% 
  slip_in_text(style = 'Description char', 
               str = glue(' for data on Covid-19 tests'))


my_doc %<>%   
  body_add_par(style = 'Description bold', 
               value = 'Definitions and analysis methods') %>% 
  
  slip_in_column_break(pos = 'before') %>% 
  
  body_add_par(style = 'Description', 
               value = 'Definitions of increasing, declining and stable trends and the definition of doubling time, as well as detailed information on the analysis methods can be found ') %>% 
  slip_in_text(style = 'Hyperlink', 
               str = 'here', 
               hyperlink = 'https://msfintl-my.sharepoint.com/:u:/g/personal/francesco_grandesso_epicentre_msf_org/EZqExKcP8axMj06voaLlveABpiicCfzkk5OWB-EaJvo9Fw?e=lwU1eM')


my_doc %<>%  
  body_add_par(style = 'Description bold', 
               value = 'Useful links') %>% 
  
  slip_in_column_break(pos = 'before') %>% 
  
  body_add_par(style = 'Description', 
               value = "") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = 'MSF Covid-19 Information Hub', 
               hyperlink = 'https://msfintl.sharepoint.com/sites/msfintlcommunities/Covid-19/SitePages/Home.aspx') %>% 
  
  body_add_par(style = 'Description', 
               value = "") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = 'MSF activities on Covid-19', 
               hyperlink = 'https://msfintl.sharepoint.com/sites/msfintlcommunities/Covid-19/MSF%20Updates/COVID-19%20MSF%20Updates/') %>% 
  
  body_add_par(style = 'Description', 
               value = "") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = 'Literature review by INSERM', 
               hyperlink = 'https://reacting.inserm.fr/literature-review/') %>% 
  
  body_add_par(style = 'Description', 
               value = "") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = 'Epicentre Covid19 blog', 
               hyperlink = 'https://msfintl.sharepoint.com/sites/grp-epi-proj-ncov/SitePages/About.aspx')


my_doc <- add_end_section_3columns()


calibri_8 <- fp_text(font.family = "Calibri", 
                     font.size = 8)

calibri_8_bold <- update(calibri_8, bold = TRUE)

my_doc %<>% 
  body_add_fpar(style = 'Description', 
                fpar(ftext('IMPORTANT NOTE: ', 
                           prop = calibri_8_bold), 
                     ftext('Data and results presented here are possibly affected by bias related with factors such as the testing strategies used by each country and the performance of their surveillance systems. Results are to be interpreted in the light of this information.', 
                           prop = calibri_8))) %>% 
  body_add_par(style = 'Horizontal line', 
               value = '')


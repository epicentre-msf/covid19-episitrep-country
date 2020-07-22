
rm(list=ls())


# Load packages -----
library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(forcats)
library(janitor)
library(ISOweek)
library(glue)
library(ggplot2)
library(ggthemes)
library(scales)
library(gridExtra)
library(grid)
library(rgeos)
library(sf)
library(officer)
library(officedown)
library(gt)
library(webshot)
library(knitr)
library(english)
library(readxl)


# Create project paths -----
path.root  <- here::here()
path.R     <- file.path(path.root,'R')
path.Rmd   <- file.path(path.root,'Rmd')
path.data  <- file.path(path.root,'data')
path.templates <- file.path(path.root,'templates')
path.local     <- file.path(path.root,'local')

# Create local folder or all types of outputs -----
dir.create(path.local, showWarnings = FALSE, recursive = TRUE)


# Create the path to NCovEpi Sharepoint
OS <- Sys.info()[['sysname']]

sharepoint.parent.dir <- dplyr::case_when(
  OS == "Windows" ~ "D:", 
  OS == "Darwin" ~ "~")

path.sharepoint <- file.path(sharepoint.parent.dir, 'MSF', 'GRP-EPI-COVID-19 - NCoVEpi')
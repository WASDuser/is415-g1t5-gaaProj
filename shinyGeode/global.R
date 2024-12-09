library(shiny)
library(shinythemes)
library(DT) # For interactive table previews (if needed)
library(ggplot2) # For plotting
library(dplyr) # For dataset manipulation
library(sfdep)
library(spdep)
pacman::p_load(sfdep, spdep, tmap, sf, ClustGeo, ggpubr, cluster, factoextra, NbClust, heatmaply, corrplot, psych, tidyverse, GGally)

# Load datasets
# crime_merged_sf <- read_rds('data/crime_merged.rds')
# rate_crime_prep <- read_rds('data/rate_crime_prep.rds')

# To minimize dataset loading
# Lazy-load datasets
load_data <- function() {
  if (!exists("crime_merged_sf")) {
    assign("crime_merged_sf", read_rds('data/crime_merged.rds'), envir = .GlobalEnv)
  }
  if (!exists("rate_crime_prep")) {
    assign("rate_crime_prep", read_rds('data/rate_crime_prep.rds'), envir = .GlobalEnv)
  }
}
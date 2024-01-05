## ----------------------------------------------------
##
## Script name: General
##
## Project: Spatial data sandbox
##
## Purpose of script: Preliminary data cleaning and model building
##
## Author: Andrei Wong Espejo
##
## Date Created: 2024-01-05
##
## Date Updated: 
##
## Email: awonge01@student.bbk.ac.uk
## -----------------------------------------------------
##
## Notes: Using block level data from the 2017 Census
##   
##
## ---------------------------

## Load required packages ----

if (!require("pacman")) {
  
  install.packages("pacman")
}

pacman::p_load(here, dplyr, tidyverse, janitor, sf, Hmisc)

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
renv::snapshot()
##renv::update()
set.seed(4183) # See https://www.gigacalculator.com/calculators/random-number-generator.php
theme_set(theme_minimal())

## Runs the following --------

## Import and clean data -------------------------------------------------------
# read_sf() and st_read(). They are the same apart from converting character 
# data to factors, read_sf() uses stringsAsFactors = FALSE by default.

data <- st_read("data/LimaMet/EstratoLimaMetropolitanashp.shp")

# Simple feature collection with 106688 features and 196 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -77.2 ymin: -12.5 xmax: -76.67 ymax: -11.73
# Geodetic CRS:  WGS 84

st_layers("data/LimaMet/EstratoLimaMetropolitanashp.shp")

# Using variable POBRE_SUP:




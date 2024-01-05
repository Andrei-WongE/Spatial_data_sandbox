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

pacman::p_load(here, dplyr, tidyverse, janitor)

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
renv::snapshot()
##renv::update()

## Runs the following --------

## Import and clean data -------------------------------------------------------




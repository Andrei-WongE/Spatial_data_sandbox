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

if (!require("pacman","groundhog")) {
  
  install.packages(c("pacman","groundhog"))
}

library(pacman)
# library(checkpoint)

library(devtools)
library(xfun)
library(checkpoint)
library(remotes)

# checkpoint("2020-01-01")  # replace with desired date

# install.packages("groundhog")
library("groundhog")
groundhog.day="2020-05-12"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

#ONLY install once
# remotes::install_version("INLA"
#                          , version="20.05.12"
#                          , repos=c(getOption("repos")
#                          , INLA="https://inla.r-inla-download.org/R/testing")
#                          , dep=FALSE
#                          , quiet = FALSE
#                          , method="curl"
#                          # , mode="wb"
#                          , type="source"
#                         )
# 
# install_github("menglezhang/socialfrontiers@v0.2",
#                build_opts = c("--no-resave-data", 
#                               "--no-manual"), 
#                build_vignettes = FALSE)
library("socialFrontiers")
library("INLA")

pkgs = c("here", "dplyr", "tidyverse", "janitor", "sf"
       , "tmap", "devtools", "renv", "Hmisc")

groundhog.library(pkgs, groundhog.day)

#!!!POTENTIAL PROBLEMATIC DEPENDENCIES:
#Checkmate->htmlTable->Hmisc

## Program Set-up ------------------------------------------------------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
# renv::snapshot()
# renv::update()
set.seed(4183) # See https://www.gigacalculator.com/calculators/random-number-generator.php
theme_set(theme_minimal())

## Runs the following --------------------------------------------------------
# 1. Installs INLA and SF packages
# 2. Upload block level data from Lima, Census 2017.
# 3. Applies SF to data


## Import and clean data -------------------------------------------------------
# read_sf() and st_read(). They are the same apart from converting character 
# data to factors, read_sf() uses stringsAsFactors = FALSE by default.
# AND convert it to a CRS that is projected.
# Help picking an appropriate CRS:Using https://projectionwizard.org/

data <- st_read("data/LimaMet/EstratoLimaMetropolitanashp.shp") %>% 
  st_transform(crs = 32718) #18s zone (EPSG: 32718) 


# Simple feature collection with 106688 features and 196 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -77.2 ymin: -12.5 xmax: -76.67 ymax: -11.73
# Geodetic CRS:  WGS 84

st_layers("data/LimaMet/EstratoLimaMetropolitanashp.shp")

# Using variable POBRE_SUP:

describe(data$POBRE_SUP)
# n  missing distinct     Info 
# 106688        0       61    0.968 
# Mean      Gmd      .05      .10 
# 17.85    17.64     0.00     0.00 
# .25      .50      .75      .90 
# 0.00    16.38    31.13    41.09 
# .95 
# 41.09 
# 
# lowest : 0       2.36948 2.4496  6.24318 6.89766
# highest: 42.0976 44.916  46.2188 47.6525 57.7417

hist(data$POBRE_SUP, freq = FALSE)

plot(   st_geometry(data["POBRE_SUP"])
     # , col = sf.colors(  n = 5
     #                   , alpha = 1
     #                   , categorical = FALSE)
     #                  )
     , border = "grey"
     , axes = TRUE
     )

plot(  st_geometry(st_centroid(data))
     , pch = 3
     , col = 'red'
     , add = TRUE
     )

#  Filter to the district of SJL
SJL <- data %>%
       filter(IDDIST == 150132)


##Using frontier_detect to find frontiers-------------------------------------------------------

# Only method for finding frontiers is the localised binomial model
# used in Dean et al. The model needs total counts of an event occuring
# (e.g. number of non-UK-born residents) and the total number of trials
# (e.g. total number of residents). The name of the variables denoting the column 
# containing these counts must be entered as a string.

# set parameters
y <- 'POBRE_SUP' # Number of persons in poverty, official income definition
n.trials <- 'POP' #total population (per block)


# Now we run the frontier_detect routine. You can see the underlying code used 
# for the statistical model in using socialFrontiers:::binomial_localisedINLA.

#!!!POTENTIAL PROBLEMATIC ISSUE:
# #Put _SF_USE_S2=false in usethis::edit_r_environ()
# getOption("sf_use_s2", default = FALSE) #Turn off the s2 processing, otherwise getting:
# #Error in sf_use_s2() : could not find function "sf_use_s2"
# getOption("sf_use_s2") #Check if it is on or off

frontier_model <-
  frontier_detect(
    data = data,
    y = y, n.trials = n.trials)

#!!!ERROR:
# Error in nb2listw(neighbours, glist = glist, style = style, zero.policy = zero.policy) : 
#   Empty neighbour sets found
# In addition: Warning message:
#   In frontier_detect(data = data, y = y, n.trials = n.trials) :
#   106560 zone(s) have no neighbours!

class(frontier_model) # Outputs a frontier_model object

# The output saves as a 'frontier_model' object which can be used with other
# methods such as summary.

## Methods for use with the frontier_model object

### Summary

summary(frontier_model) ## This calls up summary.frontier_model


### graphing and gis methods

# We can extract the frontier (as well as non-frontier) borders as a sf object for
# further graphing or gis methods using frontier_as_sf. Note that this function
# throws up ignorable warnings that come from using sf::st_intersects.


suppressWarnings(borders_sf <-
                   frontier_as_sf(frontier_model, silent = T))

class(borders_sf) 

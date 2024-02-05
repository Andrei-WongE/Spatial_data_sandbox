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
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

# pkgs <- c("INLA")
# groundhog.library(pkgs
#                   , "2020-04-24"
#                   , tolerate.R.version='4.0.2'
#                   # , force.install=TRUE
#                   , ignore.deps=FALSE
#                   )


#install.packages("remotes")
#install.packages("Rcpp")
#remotes::install_github("rspatial/terra")
#install.packages("BiocManager")

remotes::install_version("INLA"
                         , version="20.05.12"
                         , repos=c(getOption("repos")
                         , INLA="https://inla.r-inla-download.org/R/testing")
                         , dep=FALSE
                         , quiet = FALSE
                         , method="curl"
                         # , mode="wb"
                         , type="source"
                        )

install_github("menglezhang/socialfrontiers@v0.2",
               build_opts = c("--no-resave-data", 
                              "--no-manual"), 
               build_vignettes = FALSE)

pacman::p_load(here, dplyr, tidyverse, janitor, sf, Hmisc
               , socialFrontiers, tmap, devtools)


## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
# renv::snapshot()
# renv::update()
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

plot(  st_geometry(data["POBRE_SUP"])
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
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 16 is degenerate (duplicate vertex)
# In addition: Warning message:
# st_centroid assumes attributes are constant over geometries 



##  Filter to the district of SJL
SJL <- data %>%
       filter(IDDIST == 150132)


#  Using frontier_detect to find frontiers

Only method for finding frontiers is the localised binomial model
used in Dean et al. The model needs total counts of an event occuring
(e.g. number of non-UK-born residents) and the total number of trials (e.g. 
                                                                       total number of residents). The name of the variables denoting the column containing
these counts must be entered as a string.

~
# set parameters}
y <- 'nonUK' # 'nonUK' # Number of foreign
n.trials <- 'totalPop' #total population (per zone?)


Now we run the frontier_detect routine. You can see the underlying code used 
for the statistical model in using socialFrontiers:::binomial_localisedINLA.


frontier_model <-
  frontier_detect(
    data = barnet,
    y = y, n.trials = n.trials)

class(frontier_model) # Outputs a frontier_model object


The output saves as a 'frontier_model' object which can be used with other
methods such as summary.

## Methods for use with the frontier_model object

### Summary

summary(frontier_model) ## This calls up summary.frontier_model


### graphing and gis methods

We can extract the frontier (as well as non-frontier) borders as a sf object for
further graphing or gis methods using frontier_as_sf. Note that this function
throws up ignorable warnings that come from using sf::st_intersects.


suppressWarnings(borders_sf <-
                   frontier_as_sf(frontier_model, silent = T))

class(borders_sf) 

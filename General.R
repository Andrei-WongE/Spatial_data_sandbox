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

# Contiguity neighbours for polygon support:

#Using spatial indices to check intersection of polygons is much faster than
#the legacy method in poly2nb
# poly2nb uses two heuristics, first to find candidate neighbours from intersecting
# polygons (st_intersects()), and second to use the symmetry of the relationship 
# to halve the number of remaining tests. This means that performance is linear 
# in n, but with overhead for identifying candidates, and back-filling symmetric 
# neighbours. Further, spdep::poly2nb() stops searching for queen contiguity as 
# soon as the first neighbour point is found within snap distance (if not identical,
# which is tested first); a second neighbour point indicates rook contiguities.

reps <- 10
eps <- sqrt(.Machine$double.eps)
system.time(for(i in 1:reps) neighb <- spdep::poly2nb( data
                                                     , queen=TRUE
                                                     , snap=eps
                                                     )
           )/reps
                                      
      # user  system elapsed 
      # 9.859   0.151  18.85
neighb

#  poly2nb function defaulting to the queen criterion
      # Neighbour list object:
      #   Number of regions: 106688 
      #   Number of nonzero links: 168  
      #   Percentage nonzero weights: 0.000001476 
      #   Average number of links: 0.001575 

spdep::is.symmetric.nb(neighb)
                       
# coords <- st_coordinates(st_centroid(st_geometry(data)))
coords <- st_coordinates(data)

plot(neighb, coords, col="grey", add = TRUE)

# Contiguity neighbours from invalid polygons:

# explore a further possible source of differences in neighbour object reproduction,
# using the original version of the tract boundaries used in ASDAR, but with some 
# invalid geometries as mentioned earlier

# Check if file with valid geometries in to ‘sf’ and ‘sp’ objects:
table(st_is_valid(data))
    
    # FALSE   TRUE 
    # 14      106674

# Using st_make_valid() to make the geometries valid:

data_clean <- st_make_valid(data)
table(st_is_valid(data_clean))
    
    # TRUE 
    # 106688

# Differences in geometry type

class(st_geometry(data))
    # [1] "sfc_MULTIPOLYGON"
    # [2] "sfc" 
class(st_geometry(data_clean))
    # [1] "sfc_GEOMETRY" "sfc"         

table(sapply(st_geometry(data), function(x) class(x)[[2]]))
    # MULTIPOLYGON      POLYGON 
    # 106674           14 

# This can be remedied using st_collection_extract() to get the polygon objects:

data_clean <- st_collection_extract(data_clean, "POLYGON")
table(sapply(st_geometry(data_clean), function(x) class(x)[[2]]))

    # MULTIPOLYGON 
    # 106688 

# However, in making the geometries valid, we change the geometries, so the new 
# sets of neighbours still differ from those made with the valid geometries in 
# the same ways as before imposing validity:

all.equal(data, data_clean, check.attributes=FALSE) # There are 569 differences

# set.ZeroPolicyOption(TRUE) #To avoid subsetting

#Run the model

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

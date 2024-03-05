## Load required packages ----

if (!require("pacman","groundhog", "here")) {
  
  install.packages(c("pacman","groundhog", "here"))
}

library("pacman")
library("here")

# install.packages("groundhog")
library("groundhog")
set.groundhog.folder(here("groundhog_library"))
groundhog.day = "2021-05-17" # To mantain compatibility with R-4.0.2

# To ensure reproducibility of your script, given timezone differences and
# delays in updating different CRAN mirrors, don't use a date more
# recent than two days ago: 2024-03-03.

#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("here", "dplyr", "tidyverse", "janitor", "sf"
         , "tmap", "devtools", "renv", "Hmisc", "ggplot2"
         , "xfun", "remotes", "sp", "spdep", "maptools"
         , "foreach", "doParallel", "parallel", "progress"
         , "doSNOW", "purrr", "patchwork")

groundhog.library(pkgs, groundhog.day, force.install=TRUE)

## Program Set-up ------------------------------------------------------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
# renv::snapshot()
# renv::update()
set.seed(4185) # See https://www.gigacalculator.com/calculators/random-number-generator.php
theme_set(theme_minimal())

## Runs the following --------------------------------------------------------
# 1. Upload block level data from Lima, Census 2017.
# 2. 


## Import and clean data -------------------------------------------------------
data <- st_read("data/Shape_Lima/ZONAS_CENSALES.shp") %>% 
  st_transform(crs = 24892) #PSAD56 / Peru central zone


data <- data[data$POB > 0,] #Remove rows with no population


# Checks if CRS is geographic or not
st_crs(data)$IsGeographic 
# [1] FALSE

# Finds out the CRS units 
st_crs(data)$units_gdal
# [1] "metre"

st_layers("data/LimaMet/EstratoLimaMetropolitanashp.shp")



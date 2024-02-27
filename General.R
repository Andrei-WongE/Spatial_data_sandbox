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

# install.packages("groundhog")
library("groundhog")
groundhog.day = "2020-05-12"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("here", "dplyr", "tidyverse", "janitor", "sf"
         , "tmap", "devtools", "renv", "Hmisc", "ggplot2"
         , "xfun", "remotes", "sp", "spdep", "maptools"
         , "foreach", "doParallel", "parallel", "progress"
         , "doSNOW", "purrr", "patchwork")

groundhog.library(pkgs, groundhog.day)

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
  # install.packages("bigDM")
library("socialFrontiers")
library("INLA")
library("bigDM")

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

# sf::sf_use_s2(FALSE) #planar geometry engine GEOS will be used by default 
# for all geometry operations, including geometry operations on unprojected data.

data <- st_read("data/LimaMet/EstratoLimaMetropolitanashp.shp") %>% 
  st_transform(crs = 24892) #PSAD56 / Peru central zone

  # Simple feature collection with 106688 features and 196 fields
  # Geometry type: MULTIPOLYGON
  # Dimension:     XY
  # Bounding box:  xmin: -77.2 ymin: -12.5 xmax: -76.67 ymax: -11.73
  # Geodetic CRS:  WGS 84

data <- data[data$POB > 0,] #Remove rows with no population

  # Simple feature collection with 90303 features and 196 fields
  # geometry type:  MULTIPOLYGON
  # dimension:      XY
  # bbox:           xmin: 590800 ymin: 709200 xmax: 647400 ymax: 793900
  # projected CRS:  PSAD56 / Peru central zone

# Checks if CRS is geographic or not
st_crs(data)$IsGeographic 
    # [1] FALSE

# Finds out the CRS units 
st_crs(data)$units_gdal
    # [1] "metre"

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

# Check and clean data----------------------------------------------------------
# Only method for finding frontiers is the localised binomial model
# used in Dean et al. The model needs total counts of an event occuring
# (e.g. number of non-UK-born residents) and the total number of trials
# (e.g. total number of residents). The name of the variables denoting the column 
# containing these counts must be entered as a string.

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

data <- SJL
rm(SJL)

# Create a neighbors list using the poly2nb function, to obtain contiguos polygons
# the snap function is needed, but as it is arbitrary, a set of distances will be used

# Parallel Processing set-up
(num_cores <- detectCores())  # Detect total number of available cores
num_cores <- 10     # Leave one core for the OS  
cl <- makeCluster(num_cores)  # Create a cluster with specified number of cores
registerDoParallel(cl)  # Register the cluster for parallel processing
registerDoSNOW(cl)

distance_vector <- 10:20 # Calculating from 10 to 20 meters as snap parameter

#Progress bar
pb <- progress_bar$new(
  format = "letter = :letter [:bar] :elapsed | eta: :eta",
  total = length(distance_vector),     
  width = 60)

progress_letter <- rep(LETTERS[1:11], 11)

# allowing progress bar to be used in foreach
progress <- function(n){
  pb$tick(tokens = list(letter = progress_letter[n]))
} 

opts <- list(progress = progress)


neighb <- foreach(i = 1:distance_vector
                  , .combine = "+" # Cbind the result of each iteration
                  , .inorder = TRUE
                  , .options.snow = opts # Allows for progress bar
                  , .packages = c("spdep", "doSNOW")
                  ) %dopar%  {
  
                    sink(paste0("Report_", i, ".txt")) # Open sink file for each iteration
                    
                    result <- spdep::poly2nb( data$geometry
                                            , queen = TRUE
                                            , row.names = data$id_mz
                                            , snap = i #meters
                                          )
                    print(result) 
                    
                    sink() # Close the sink
}
            
#Cluster of parallel processes made by the makeCluster function is stopped and shut down
stopCluster(cl)

  # Get a list of files that match the pattern
  files_to_delete <- list.files(  path = here()
                                , pattern = "Report_"
                                , recursive = TRUE
                                , full.names = TRUE
                                )

  #WARNING! The list.files() function returns the files in the order they are 
  #read by the file system, which might not be sequential
  
  files_to_delete <- files_to_delete[c(1, 3:10, 2)]
  
  # Read and parse the files
  data_db <- map_df(files_to_delete, ~read_lines(.x) %>% 
                 .[2:5] %>% # Extract the lines 2 to 5
                 str_split_fixed(":", 2) %>% 
                 as.data.frame(), .id = "id") %>% 
                 pivot_wider(names_from = V1, values_from = V2) %>% 
                 mutate_all(as.numeric)      
  
  # Delete the files
  file.remove(files_to_delete)
  
  #Plot the results
    p1 <- data_db %>%
          ggplot(aes(x = id, y = `Number of nonzero links` )) +
          geom_point() +
          scale_x_continuous(breaks = unique(data_db$id)) +
          geom_line( color="red") +
          scale_y_continuous(breaks = unique(data_db$`Number of nonzero links`))
        
    p2 <-data_db %>%
          ggplot(aes(x = id, y =`Percentage nonzero weights`)) +
          geom_bar(, stat = "identity"
                   , fill = "blue"
                   , alpha = 0.5
                   ) +
          scale_x_continuous(breaks = unique(data_db$id))+
          scale_y_continuous(breaks = unique(data_db$`Percentage nonzero weights`))
    
  
    p1 + p2
#
    neighb <- spdep::poly2nb( data$geometry
                            , queen = TRUE
                            , row.names = data$id_mz
                            , snap = 10
                            )
    
    neighb
    
# Convert the neighbors list to a binary adjacency matrix
adj_matrix <- nb2mat(neighb, style = "B" , zero.policy = TRUE)

#  poly2nb function defaulting to the queen criterion
    # Neighbour list object:
    #   Number of regions: 11439 
    # Number of nonzero links: 34008 
    # Percentage nonzero weights: 0.02599 
    # Average number of links: 2.973 
    # 1151 regions with no links:

spdep::is.symmetric.nb(neighb)
# [1] TRUE

table(st_is_valid(data))
 
  # FALSE  TRUE 
  # 1 13575 

data_clean <- st_make_valid(data)

table(st_is_valid(data_clean))
# TRUE 
# 13576 

# Identify if polygons share a border
touching <- st_touches(data_clean, sparse = TRUE) #sparse = FALSE to return a matrix
# apply(touching, 1, any)
lengths(touching) > 0

#RESULT: some polygons are touching other polygon, hence the no valid polygons
#SFA, page 26 clause c of what constitutes a valid polygon: "No two Rings in the 
#boundary cross and the Rings in the boundary of a Polygon may intersect at a
#Point but only as a tangent" I.e., any combination of polygons sharing a border is invalid.

st_relate(data_clean)[,1]
# F denotes no intersection.
# 2 denotes a spatial intersection of dimension 2 (i.e., an area).
# 1 denotes a spatial intersection of dimension 1 (i.e., a line).

# Function finds pattern: 2FFF1FFF2

# st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
# data_clean %>% mutate(NB_QUEEN = st_queen(.))
# 
# 
# st_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****")
# data_clean %>% mutate(NB_ROOK = st_rook(.))


# Group by link ID and union the polygons
# data_linked <- data_clean %>%
#                group_by(id_mz) %>%
#                summarise(geometry = st_union(geometry), do_union = FALSE)

# coords <- st_coordinates(st_centroid(st_geometry(data)))
# coords <- st_coordinates(data)
# 
# plot(neighb, coords, col="grey", add = FALSE)
#
    # Error in plot.nb(neighb, coords, col = "grey", add = FALSE) : 
    #   length(nb) == nrow(coords) is not TRUE

# Contiguity neighbours from invalid polygons:

# explore a further possible source of differences in neighbour object reproduction,
# using the original version of the tract boundaries used in ASDAR, but with some 
# invalid geometries as mentioned earlier

class(st_geometry(data))
    # [1] "sfc_MULTIPOLYGON"
    # [2] "sfc" 
class(st_geometry(data_clean))
    # [1] "sfc_GEOMETRY" "sfc"         

table(sapply(st_geometry(data_clean), function(x) class(x)[[2]]))
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

all.equal(data, data_clean, check.attributes=FALSE) # There are 28 differences

# Subdivide the MULTIPOLYGON object into single polygons
data_clean <- st_cast(data_clean, "POLYGON")




# Create neighbors--------------------------------------------------------------


neighb_clean_nb <- spdep::poly2nb(  data_clean
                                   , row.names = "id_mz"
                                   , queen = TRUE
                                   , useC = TRUE
                                   , snap = 10
                                )
neighb_clean_nb

    # Neighbour list object:
    #   Number of regions: 13579 
    # Number of nonzero links: 74 
    # Percentage nonzero weights: 0.00004013 
    # Average number of links: 0.00545 
    # 13509 regions with no links:

# Create a vector indicating whether each polygon has at least one neighbor
has_neighb_clean <- sapply(neighb_clean_nb, function(x) length(x) > 0)

# Subset the polygons that have at least one neighbor
data_clean <- data_clean[has_neighb_clean, ]




# Convert the neighbors list to a binary adjacency matrix
adj_matrix <- nb2mat(neighb_clean_nb, style = "B" , zero.policy = FALSE)


#Disjoint connected subgraphs

head(adj_matrix)

spdep::is.symmetric.nb(adj_matrix)


plot(st_geometry(data_clean_nb), border = "lightgray")
coords <- st_centroid(st_geometry(data_clean_nb), of_largest_polygon=TRUE)
plot.nb(neighb_clean_nb, coords, add = TRUE)

# Numbers of neighbours
spdep::card(neighb_clean_nb)
table(1:nrow(neighb_clean_nb), card(neighb_clean_nb))


# Compute the k-nearest neighbors
# 
# coords <- st_centroid(st_geometry(data_clean), of_largest_polygon = TRUE)
# data_clean.knn <- knearneigh(st_centroid(coords, k = 4))
# 
# plot(coords, border = "red")
# plot(knn2nb(data_clean.knn), coords, add = TRUE)
# title(main="K nearest neighbours, k = 4")
# 
# # Convert the k-nearest neighbors object to a neighbors list
# neighb_clean <- knn2nb(data_clean.knn)
# 
# spdep::is.symmetric.nb(neighb_clean)
# # [1] FALSE
# 
# # Convert the neighbors list to a neighbors list object
# lw = spdep::nb2listw(neighb_clean, style="B", zero.policy=TRUE) 
# #row standardised (sums over all links to n)
# 
# lw
#   
#   # Neighbour list object:
#   # Number of regions: 13579 
#   # Number of nonzero links: 13579 
#   # Percentage nonzero weights: 0.007364 
#   # Average number of links: 1 
#   # Non-symmetric neighbours list
# 
#   # Weights style: B 
#   # Weights constants summary:
#   #   n        nn    S0    S1    S2
#   # B 13579 184389241 13579 21377 63304
# 
# # Convert the neighbors list object to a matrix
# W = spdep::listw2mat(lw)

# write.table(W, "spatialW.csv", sep=",", col.names=F, row.names=F)
# data_clean@data$id <- seq(1:dim(data_clean@data)[1])

# # Merge disjoint connected subgraphs
# connected <- bigDM::connect_subgraphs( data_clean
#                                      , ID.area = "id_mz"
#                                      , nb = neighb_clean
#                                      , plot = TRUE
#                                      )
# 
# title(main="Connected neighbourhood graph")
# 
# n.comp.nb(connected$nb)$nc==1
# 
# par(op)

# # Estimate first order adjacency matrix
# neighb_clean_mat <- spdep::nb2mat(neighb_clean, style = "B", zero.policy=TRUE)
# neighb_clean_mat[1:10, 1:10]

# # Subsetting for only polygons with neighbours
data_clean_sub <- data_clean %>%
                   mutate(INTERSECT = st_intersects(.))

# # Check if the number of polygons has been reduced
# nrow(data) - nrow(data_clean_sub) #0

# # Create a neighbors list using the poly2nb function
# nb <- poly2nb(data_clean, queen = TRUE)
# 
# # Convert the neighbors list to a binary adjacency matrix
# adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)



# Create variables of interest--------------------------------------------------------------

data_clean <- data_clean %>% 
              mutate(POBRE_COUNT = (POBRE_SUP/100) * (POB))

#Run the model-----------------------------------------------------------------

# set parameters
y <- 'POBRE_COUNT' # Number of persons in poverty, official income definition
n.trials <- 'POP' #total population (per block)

frontier_model <- frontier_detect(  y = y
                                  , data = data_clean
                                  , n.trials = n.trials
                                  , W.nb = neighb_clean_nb #neighbourhood matrix indicating which elements of data are adjacent to each other.
                                  , zero.policy = TRUE
                                  , verbose = TRUE
                                  )

#!!!ERROR:
  # Error in nb2listw(neighbours, glist = glist, style = style, zero.policy = zero.policy) : 
  # Empty neighbour sets found
  # In addition: Warning message:
  #   In frontier_detect(data = data, y = y, n.trials = n.trials) :
  #   106560 zone(s) have no neighbours!

#!!!ERROR:
  # Error in inla.inlaprogram.has.crashed() : 
  # The inla-program exited with an error. Unless you interupted it yourself, please rerun with verbose=TRUE and check the output carefully.
  # If this does not help, please contact the developers at <help@r-inla.org>.
  # In addition: Warning message:
  # In if (class(X) == "try-error") stop("the covariate matrix contains inappropriate values.",  :
  # the condition has length > 1 and only the first element will be used

class(frontier_model) # Outputs a frontier_model object

# The output saves as a 'frontier_model' object which can be used with other
# methods such as summary.

## Methods for use with the frontier_model object

# Summary-----------------------------------------------------------------------

summary(frontier_model) ## This calls up summary.frontier_model


# graphing and gis methods -----------------------------------------------------

# We can extract the frontier (as well as non-frontier) borders as a sf object for
# further graphing or gis methods using frontier_as_sf. Note that this function
# throws up ignorable warnings that come from using sf::st_intersects.


suppressWarnings(borders_sf <-
                   frontier_as_sf(frontier_model, silent = T))

class(borders_sf) 

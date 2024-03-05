data(london)

##  Filter to the borough of Barnet
barnet <-
  london %>%
  filter(substr(LSOAname, 1, 6) %in% 'Barnet')


neighb <- spdep::poly2nb( barnet
                         , queen=TRUE
                       # , snap=eps
)


# user  system elapsed 
# 16.739   

coords <- st_coordinates(st_centroid(st_geometry(barnet)))


plot(neighb, coords, col="grey", add = FALSE)
plot(barnet)
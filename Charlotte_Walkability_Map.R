# Walking in Charlotte: 
# Using Sidewalk Network Analysis to Measure Residential Access to Amenities
# Cheyne Campbell

#---------------------------------------------------------------------------------
# UPLOAD LIBRARIES
#---------------------------------------------------------------------------------

library(sf)
library(sp)
library(ggplot2)
library(leaflet)
library(magrittr)
library(tmap)
library(classInt)
library(rgdal)
library(data.table)
library(GISTools)
library(igraph)
library(dplyr)
library(tidygraph)
library(nabor)
library(units)
library(raster)

#---------------------------------------------------------------------------------
# IMPORT DATA - all projections set to proj4string: +proj=longlat +datum=WGS84 +no_defs
#---------------------------------------------------------------------------------

# IMPORT CHARLOTTE MECKLENBERG NEIGHBORHOOD PROFILE AREAS (NPAs)
# DATA SOURCE: https://mcmap.org/qol/#5/
NPA <- st_read("/Users/cheynecampbell/Documents/UCL_SmartCitiesandUrbanAnalytis/UCL_GIS/Charlotte_Walkability/qol-data/NPA_2014_meck.shp")
NPA <- st_transform(NPA, 4326)

# filter based on which NPAs are part of Charlotte - NPAs 2 - 394
# DATA SOURCE: https://ui.uncc.edu/story/quality-life-dashboard-find-your-neighborhood
NPA_Charlotte <- NPA[NPA$NPA >= 2 & NPA$NPA <= 394,]

remove(NPA)

#------------------------------------------------------------------------------------------------------

# IMPORT TAX PARCEL WITH CAMA DATA 
# DATA SOURCE: http://maps.co.mecklenburg.nc.us/openmapping/data.html?search=par 
Tax_File <- "/Users/cheynecampbell/Documents/UCL_SmartCitiesandUrbanAnalytis/UCL_GIS/Charlotte_Walkability/parcel_taxdata/Parcel_TaxData.shp"
Tax <- st_read(Tax_File)[ ,c("objectid_1","units","cdebuildin")]
Tax_Charlotte <- st_transform(Tax, 4326)

# filter out tax parcels that fall outside the City of Charlotte
Tax_Charlotte <- Tax_Charlotte[NPA_Charlotte, ]

# convert cdebuildin column to numeric
Tax_Charlotte$cdebuildin <- as.numeric(Tax_Charlotte$cdebuildin)

# filter out parcels that are not residential based on descriptions from meta data
# and filter out parcels that have no living units
ResidentialTypes <- c(1,2,3,4,5,6,9,60,61,62,63,64,69,74)
Tax_Charlotte <- Tax_Charlotte[Tax_Charlotte$cdebuildin %in% ResidentialTypes, ]
Tax_Charlotte <- Tax_Charlotte[Tax_Charlotte$units != 0, ]

# find the centroid of each residential land parcel
Tax_Charlotte_SP <- as(Tax_Charlotte, "Spatial")
Tax_Centroids_SP <- gCentroid(Tax_Charlotte_SP, byid = TRUE)
Tax_Centroids <- st_as_sf(Tax_Centroids_SP)
Tax_Centroids <- st_transform(Tax_Centroids, 4326)

# bind centroid with land parcel info - adds land parcel info to centroid geometry
Tax_Centroids <- cbind(Tax_Centroids, Tax_Charlotte)

# filter centroids based on Charlotte NPAs
Tax_Centroids<- Tax_Centroids[NPA_Charlotte, ]

remove(Tax)
remove(Tax_Centroids_SP)
remove(Tax_Charlotte)
remove(Tax_Charlotte_SP)
  
#------------------------------------------------------------------------------------------------------
# IMPORT SIDEWALK DATA TO BE USED AS NETWORK
#------------------------------------------------------------------------------------------------------

# IMPORT CHARLOTTE SIDEWAlKS DATA
# DATA SOURCE: http://data.charlottenc.gov/datasets/sidewalks?geometry=-80.848%2C35.197%2C-80.815%2C35.209
Sidewalks <- st_read("/Users/cheynecampbell/Documents/UCL_SmartCitiesandUrbanAnalytis/UCL_GIS/Charlotte_Walkability/Sidewalks/Sidewalks.shp")
Sidewalks_Charlotte <- st_transform(Sidewalks, 4326)

remove(Sidewalks)

# remove empty geometries
Sidewalks_Charlotte = Sidewalks_Charlotte[!st_is_empty(Sidewalks_Charlotte),,drop = FALSE]

# MAKE NETWORK USING SIDEWALK DATA 
# SOURCE FOR METHOD: https://www.r-spatial.org/r/2019/09/26/spatial-networks.html

# convert multilinestring to linestring 
Sidewalks_Charlotte = st_cast(Sidewalks_Charlotte, "LINESTRING")

# add ID for sidewalk edges
Edges <- Sidewalks_Charlotte %>%
  mutate(edgeID = c(1:n()))

# locate the start and end points for each of the sidewalk edges
Nodes <- Edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1,n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

# give each node a unique index 
Nodes <- Nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  dplyr::select(-xy)

# assign each node to an edge (i.e. for each line, find the starting and ending node)
Source_Nodes <- Nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

Target_Nodes <- Nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

Edges = Edges %>%
  mutate(from = Source_Nodes, to = Target_Nodes)

# add attribute for length of each edge in meters
Edges$edge_length <- st_length(Edges)
# E(Graph)$edge_length

# remove the duplicate nodes since they have been added to the edges 
Nodes <- Nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  dplyr::select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(Edges))
  
# convert nodes and edges to a tibble graph object 
Graph = tbl_graph(nodes = Nodes, edges = as_tibble(Edges), directed = FALSE)

#------------------------------------------------------------------------------------------------------
# IMPORT AMENITY DATA
#------------------------------------------------------------------------------------------------------

# IMPORT CHARLOTTE GROCERY STORE DATA
# DATA SOURCE: http://data.charlottenc.gov/datasets/grocery-stores 
Grocery <- st_read("/Users/cheynecampbell/Documents/UCL_SmartCitiesandUrbanAnalytis/UCL_GIS/Charlotte_Walkability/Grocery_Stores/Grocery_Stores.shp")
GroceryWGS84 <- st_transform(Grocery, 4326)

# filter grocery store data based on NPA boundary - i.e. exclude grocery stores outside of the City of Charlotte
Groceries_Charlotte <- GroceryWGS84[NPA_Charlotte, ]

# export grocery store names to CSV to prevent overlap with restaurants later
GroceryNames <- subset(Grocery, select="Name")
write.csv(GroceryNames,"/Users/cheynecampbell/Documents/UCL_SmartCitiesandUrbanAnalytis/UCL_GIS/Charlotte_Walkability/GroceryNames.csv", row.names = FALSE)

remove(GroceryNames)
remove(Grocery)
remove(GroceryWGS84)

#------------------------------------------------------------------------------------------------------

# IMPORT CHARLOTTE RESTAURANT DATA
# DATA SOURCE: https://public.cdpehs.com/NCENVPBL/ESTABLISHMENT/ShowESTABLISHMENTTablePage.aspx?ESTTST_CTY=60
Restaurants_File <- "/Users/cheynecampbell/Documents/UCL_SmartCitiesandUrbanAnalytis/UCL_GIS/Charlotte_Walkability/restaurants_FINAL_filtered.csv"
Restaurants <- read.csv(Restaurants_File) [ ,c("Name","Formatted_Address","Lat","Lng")]

# convert to simple features and add projection
Restaurants_Spatial <- st_as_sf(Restaurants, coords = c("Lng", "Lat"), crs = 4326)

# filter restaurant data based on NPA boundary - i.e. exclude restaurants outside of the City of Charlotte 
Restaurants_Charlotte <- Restaurants_Spatial[NPA_Charlotte, ]

remove(Restaurants_File)
remove(Restaurants)
remove(Restaurants_Spatial)

#------------------------------------------------------------------------------------------------------

# IMPORT CHARLOTTE PARKS DATA
# DATA SOURCE: http://maps.co.mecklenburg.nc.us/openmapping/data.html
Parks <- st_read("/Users/cheynecampbell/Documents/UCL_SmartCitiesandUrbanAnalytis/UCL_GIS/Charlotte_Walkability/parkproperty/ParkProperty.shp")
ParksWGS84 <- st_transform(Parks, 4326)

# filter out admin, golf courses, and historic sites - focusing on outdoor / green parks
Parks_Filtered <- ParksWGS84[ParksWGS84$park_type != "ADMINISTRATION" &
                             ParksWGS84$park_type != "GOLF COURSE" &
                             ParksWGS84$park_type != "HISTORIC SITE",]

# filter parks based on NPA boundary -i.e. exclude parks that are entirely outside the City of Charlotte
Parks_Filtered <- Parks_Filtered[NPA_Charlotte, ]

# make a grid of points covering all of Charlotte
Grid <- NPA_Charlotte %>%
  st_make_grid(cellsize = 0.001, what = "centers") %>%
  st_intersection(NPA_Charlotte)

# convert to sf and project 
Grid <- as(Grid, "Spatial")
Grid <- st_as_sf(Grid)
Grid <- st_transform(Grid, 4326)

# filter points according to locations of parks
Park_Points <- st_intersection(Grid, Parks_Filtered)

remove(Parks)
remove(ParksWGS84)
remove(Parks_Filtered)
remove(Grid)

#------------------------------------------------------------------------------------------------------
# MAP RESIDENCES, AMENITIES, AND SIDEWALK NETWORK IN CITY OF CHARLOTTE
#------------------------------------------------------------------------------------------------------

AmenityMap <- ggplot() +
                # Charlotte boundary
                geom_sf(data = st_union(NPA_Charlotte$geom), lwd = 0) +
                # centroids of residential land parcels
                geom_sf(data = Tax_Centroids, aes(color = "Residence"), show.legend = "point", size = 0.2, stroke = 0, alpha = 1) + 
                # sidewalk edges and nodes
                geom_sf(data = Graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(fill = "Sidewalk"), show.legend = "line", size = 0.2) + 
                geom_sf(data = Graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), fill = "grey61", size = 0.4, stroke = 0, alpha = 1) + 
                # park points
                geom_sf(data = Park_Points, aes(color = "Park"), show.legend = "point", size = 0.4, stroke = 0, alpha = 1) +
                # restaurants
                geom_sf(data = Restaurants_Charlotte, aes(color = "Restaurant"), show.legend = "point", size = 0.4, stroke = 0, alpha = 1) +
                # grocery stores
                geom_sf(data = Groceries_Charlotte, aes(color = "Grocery Store"), show.legend = "point", size = 0.8, stroke = 0, alpha = 1) +
                # legend for sidewalk line
                scale_color_manual(values = c("Sidewalk" = "grey61"),
                                   guide = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
                # legend for points 
                scale_color_manual(values = c("Residence" = "deepskyblue1", "Park" = "palegreen3", "Restaurant" = "indianred1", "Grocery Store" = "gold1"),
                                   guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "blank", "blank"),
                                                                            shape = c(16, 16, 16, 16),
                                                                            size = c(2, 2, 2, 2)))) +
                theme_light()

AmenityMap <- AmenityMap + labs(title="Charlotte, North Carolina",
                  subtitle = "Residences, Amenities, and Sidewalk Network",
                  x = "Longitude", 
                  y = "Latitude",
                  caption = "Sources: Open Mapping - Mecklenburg County GIS (residences), \nCharlotte Open Data Portal (grocery stores, parks, and sidewalks), \nand North Carolina Public Health Inspections (restaurants).") +
             theme(plot.title = element_text(color="grey22", size=16, face="bold"),
                   plot.subtitle = element_text(color="grey22", size=12, face = "bold"),
                   axis.title.x = element_text(color="grey40", size=10, face="plain"),
                   axis.title.y = element_text(color="grey40", size=10, face="plain"),
                   plot.caption = element_text(color="grey40", size = 10, hjust = 0, face = "italic"),
                   legend.title = element_blank())

AmenityMap

#---------------------------------------------------------------------------------
# NETWORK ANALYSIS
# DETERMINE THE NUMBER OF AMENITIES IN EACH CATEGORY WITHIN 805 METERS (0.5 MILES) OF RESIDENTIAL PARCEL
#---------------------------------------------------------------------------------

# analyze network - find shortest path between nearest node (NN) to each residence and NN to each amenity
# find total distance by summing distance to residence NN, path distance, and distance to amenity NN 
# for each residence, find total number of amenities in each category where total distance is <= 805 meters

#---------------------------------------------------------------------------------
# 1. find nearest nodes (and distances to NNs) for residences and amenities
#---------------------------------------------------------------------------------

# get.nodes
# INPUT:   Amenities (simple features)
# OUTPUT:  Amenities_CompleteInfo (simple features with added attributes)
# PURPOSE: for each amenity, finds nearest node and distance to nearest node in meters
get.nodes <- function (Amenities) {
  
  # coordinates of nodes in the network
  Node_Coords <- Nodes %>% st_coordinates()
  
  # convert simple features to 2 column matrix 
  Amenities_Coords <- Amenities %>% st_coordinates() %>% matrix(ncol = 2)
  
  # find nearest node id for amenity location and save as 1 column matrix
  knn_Nodes <- nabor::knn(data = Node_Coords, query = Amenities_Coords, k = 1)
  NodeID <- matrix(knn_Nodes$nn.idx, ncol = 1)
  colnames(NodeID) <- c("nodeID")
  
  # convert node ids to dataframe and add row ID column for reordering
  NodeID_DF <- data.frame(NodeID)
  NodeID_DF$rowID <- 1:nrow(NodeID_DF) 
  
  # merge node ids with node geometries, reorder, and convert to matrix
  NodeID_DF     <- merge(NodeID_DF, Nodes, by = "nodeID", sort = FALSE)
  NodeID_Info   <- NodeID_DF[order(NodeID_DF$rowID), ] %>% sf::st_as_sf(crs = 4326)
  NodeID_Matrix <- NodeID_Info %>% st_coordinates() %>% matrix(ncol = 2)
  
  # find distances between tax centroid and matrix in meters; save in 1 column matrix
  Dist_To_Node <- raster::pointDistance(Amenities_Coords, NodeID_Matrix, lonlat = TRUE, allpairs = FALSE) %>% matrix(ncol = 1)
  colnames(Dist_To_Node) <- c("Dist_To_Node")
  
  # combine information with original simple features; note:
  # geometry           = centroid coordinates
  # geometry.1 (or .2) = node coordinates
  CompleteInfo <- cbind(Amenities, NodeID_Info, Dist_To_Node)
  return(CompleteInfo)
  
}

Residence_GetNodes  <- get.nodes(Tax_Centroids)
Grocery_GetNodes    <- get.nodes(Groceries_Charlotte)
Restaurant_GetNodes <- get.nodes(Restaurants_Charlotte)
Park_GetNodes       <- get.nodes(Park_Points)

#---------------------------------------------------------------------------------
# 2. create distance matrix for each unique residence node and each unique amenity node
#---------------------------------------------------------------------------------

# distance matrix
# column names - amenity nodes 
# row names - residence nodes 
# excludes amenities where there is no path to any residence NN

# groceries
Grocery_Distances <- igraph::distances(Graph,
                             unique(Residence_GetNodes$nodeID),
                             unique(Grocery_GetNodes$nodeID),
                             weights = Graph %>% activate(edges) %>% pull(edge_length))
colnames(Grocery_Distances) <- c(unique(Grocery_GetNodes$nodeID))
rownames(Grocery_Distances) <- c(unique(Residence_GetNodes$nodeID))
Grocery_Distances <- Grocery_Distances[, colSums(is.infinite(Grocery_Distances)) != nrow(Grocery_Distances)]

# restaurants 
Restaurant_Distances <- igraph::distances(Graph,
                                unique(Residence_GetNodes$nodeID),
                                unique(Restaurant_GetNodes$nodeID),
                                weights = Graph %>% activate(edges) %>% pull(edge_length))
colnames(Restaurant_Distances) <- c(unique(Restaurant_GetNodes$nodeID))
rownames(Restaurant_Distances) <- c(unique(Residence_GetNodes$nodeID))
Restaurant_Distances <- Restaurant_Distances[, colSums(is.infinite(Restaurant_Distances)) != nrow(Restaurant_Distances)]

# parks
Park_Distances <- igraph::distances(Graph,
                          unique(Residence_GetNodes$nodeID),
                          unique(Park_GetNodes$nodeID),
                          weights = Graph %>% activate(edges) %>% pull(edge_length))
colnames(Park_Distances) <- c(unique(Park_GetNodes$nodeID))
rownames(Park_Distances) <- c(unique(Residence_GetNodes$nodeID))
Park_Distances <- Park_Distances[, colSums(is.infinite(Park_Distances)) != nrow(Park_Distances)]

#---------------------------------------------------------------------------------
# 3. find total number of each amenity type within walking distance for each residence
#---------------------------------------------------------------------------------

# set up count columns for amenities
Residence_GetNodes$groceryCount <- NA
Residence_GetNodes$restaurantCount <- NA
Residence_GetNodes$parkCount <- NA

# calculate number of GROCERY STORES within 805 meters on sidewalk
for (row in 1:nrow(Residence_GetNodes)) {
  # if residence is more than 805 m from sidewalk, set groceryCount to 0
  resDistance <- as.numeric(Residence_GetNodes[[row,"Dist_To_Node"]])
  if (resDistance > 805) {
    Residence_GetNodes[row,"groceryCount"] <- 0
    next
  }
  else {
    # find nearest node ID for residence, and extract corresponding row from distance matrix
    nodeID <- toString(Residence_GetNodes[[row,"nodeID"]])
    AllGroceries <- Grocery_Distances[nodeID,,drop = FALSE]
    # set up count to keep track of grocery stores that are within range 
    count <- 0 
    # iterate over columns in extracted row
    for (col in 1:ncol(AllGroceries)) {
      # extract distance value
      pathDistance <- AllGroceries[[1,col]]
      # if distance is infinite, move to next distance
      if (is.infinite(pathDistance)) {
        next
      }
      else {
        pathDistance <- as.numeric(pathDistance)
        # find ID of nearest node for that grocery store (column name) and distance to grocery
        groceryID <- as.numeric(colnames(Grocery_Distances)[col])
        groceryOptions <- Grocery_GetNodes[Grocery_GetNodes$nodeID == groceryID, "Dist_To_Node"]
        for (row2 in 1:nrow(groceryOptions)) {
          # find total distances
          groceryDistance <- as.numeric(groceryOptions[[row2,"Dist_To_Node"]])
          totalDistance <- resDistance + pathDistance + groceryDistance
          if (totalDistance <= 805)
            count <- count + 1
        }
      }
    }
  }
  # add count for grocery stores to residence data 
  Residence_GetNodes[row,"groceryCount"] <- count
}

# calculate number of RESTAURANTS within 805 meters on sidewalk
for (row in 1:nrow(Residence_GetNodes)) {
  # if residence is more than 805 m from sidewalk, set restaurantCount to 0
  resDistance <- as.numeric(Residence_GetNodes[[row,"Dist_To_Node"]])
  if (resDistance > 805) {
    Residence_GetNodes[row,"restaurantCount"] <- 0
    next
  }
  else {
    # find nearest node ID for residence, and extract corresponding row from distance matrix
    nodeID <- toString(Residence_GetNodes[[row,"nodeID"]])
    AllRestaurants <- Restaurant_Distances[nodeID,,drop = FALSE]
    # set up count to keep track of restaurants that are within range 
    count <- 0 
    # iterate over columns in extracted row
    for (col in 1:ncol(AllRestaurants)) {
      # extract distance value
      pathDistance <- AllRestaurants[[1,col]]
      # if distance is infinite, move to next distance
      if (is.infinite(pathDistance)) {
        next
      }
      else {
        pathDistance <- as.numeric(pathDistance)
        # find ID of nearest node for that restaurant (column name) and distance to restaurant
        restaurantID <- as.numeric(colnames(Restaurant_Distances)[col])
        restaurantOptions <- Restaurant_GetNodes[Restaurant_GetNodes$nodeID == restaurantID, "Dist_To_Node"]
        for (row2 in 1:nrow(restaurantOptions)) {
          # find total distances
          restaurantDistance <- as.numeric(restaurantOptions[[row2,"Dist_To_Node"]])
          totalDistance <- resDistance + pathDistance + restaurantDistance
          if (totalDistance <= 805)
            count <- count + 1
        }
      }
    }
  }
  # add count for restaurants to residence data 
  Residence_GetNodes[row,"restaurantCount"] <- count
}

# calculate number of PARKS within 805 meters on sidewalk
for (row in 1:nrow(Residence_GetNodes)) {
  # if residence is more than 805 m from sidewalk, set parkCount to 0
  resDistance <- as.numeric(Residence_GetNodes[[row,"Dist_To_Node"]])
  if (resDistance > 805) {
    Residence_GetNodes[row,"parkCount"] <- 0
    next
  }
  else {
    # find nearest node ID for residence, and extract corresponding row from distance matrix
    nodeID <- toString(Residence_GetNodes[[row,"nodeID"]])
    AllParks <- Park_Distances[nodeID,,drop = FALSE]
    # set up count to keep track of parks that are within range and which parks have been counted
    count <- 0 
    parks_counted <- vector()
    # iterate over columns in extracted row
    for (col in 1:ncol(AllParks)) {
      # extract distance value
      pathDistance <- AllParks[[1,col]]
      # if distance is infinite, move to next distance
      if (is.infinite(pathDistance)) {
        next
      }
      else {
        pathDistance <- as.numeric(pathDistance)
        # find ID of nearest node for that park (column name) and distance to park
        parkID <- as.numeric(colnames(Park_Distances)[col])
        parkOptions <- Park_GetNodes[Park_GetNodes$nodeID == parkID, c("Dist_To_Node","park_id")]
        for (row2 in 1:nrow(parkOptions)) {
          # find total distances
          parkCheckDupes <- toString(parkOptions[[row2,"park_id"]])
          parkDistance <- as.numeric(parkOptions[[row2,"Dist_To_Node"]])
          totalDistance <- resDistance + pathDistance + parkDistance
          if (totalDistance <= 805 & !(parkCheckDupes %in% parks_counted)) {
            count <- count + 1
            parks_counted <- append(parks_counted, parkCheckDupes)
          }
        }
      }
    }
  }
  # add count for parks to residence data 
  Residence_GetNodes[row,"parkCount"] <- count
}

#---------------------------------------------------------------------------------
# 4. some summary statistics
#---------------------------------------------------------------------------------

# finding: only 10 / 241,840 units are within walking distance of at least 1 grocery, restaurant, and park
walkableUnits <- Residence_GetNodes[Residence_GetNodes$groceryCount >= 1 & Residence_GetNodes$restaurantCount >= 1 & Residence_GetNodes$parkCount >= 1,]
totalUnits <- sum(Residence_GetNodes$units)

# descriptive statistics for amenities
table(Residence_GetNodes$groceryCount) # 0-1
table(Residence_GetNodes$parkCount) # 0-2
table(Residence_GetNodes$restaurantCount) # 0-8

# finding: 97% of housing units in Charlotte are single unit
table(Residence_GetNodes$units)
percentSingleUnit <- 234689 / sum(Residence_GetNodes$units)

#---------------------------------------------------------------------------------
# 5. set up additional attributes for mapping 
#---------------------------------------------------------------------------------

# set up categorical variable for walkability 
# if you can reach a park, grocery store, and restaurant = good 
# if you can reach one or two amenity types = medium
# if you cannot reach any amenity = bad
Residence_GetNodes$walkability <- NA
Residence_GetNodes[Residence_GetNodes$parkCount >= 1 &
                   Residence_GetNodes$restaurantCount >= 1 &
                   Residence_GetNodes$groceryCount >= 1, "walkability"] <- "Good"
Residence_GetNodes[Residence_GetNodes$parkCount == 0 &
                   Residence_GetNodes$restaurantCount == 0 &
                   Residence_GetNodes$groceryCount == 0, "walkability"] <- "Bad"
Residence_GetNodes[is.na(Residence_GetNodes$walkability), "walkability"] <- "Medium"

# frequency table for categorical variable 
table(Residence_GetNodes$walkability)

# add attribute for wether or not walkability is good - used to determine size in visual
Residence_GetNodes$walkabilityBinary <- NA
Residence_GetNodes[Residence_GetNodes$walkability == "Good", "walkabilityBinary"] <- 1
Residence_GetNodes[!(Residence_GetNodes$walkability == "Good"), "walkabilityBinary"] <- 0

#---------------------------------------------------------------------------------
# MAP WALKABILITY OF RESIDENCES 
#---------------------------------------------------------------------------------

CountsMap <- ggplot() +
  # Charlotte boundary
  geom_sf(data = st_union(NPA_Charlotte$geom), lwd = 0) +
  # centroids of residential land parcels
  geom_sf(data = Residence_GetNodes, aes(color = walkability, size = walkabilityBinary), show.legend = "point", stroke = 0, alpha = 1) + 
  # sidewalk edges and nodes
  geom_sf(data = Graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(fill = "Sidewalk"), show.legend = "line", size = 0.2) + 
  geom_sf(data = Graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), fill = "grey61", size = 0.4, stroke = 0, alpha = 1) + 
   # legend for sidewalk line
  scale_color_manual(values = c("Sidewalk" = "grey61"),
                     guide = guide_legend(override.aes = list(linetype = "solid", shape = NA, size = c(2)))) +
  # legend for points 
  scale_color_manual(values = c("Medium" = "deepskyblue1", "Good" = "palegreen3", "Bad" = "indianred1"),
                     guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "blank"),
                                                              shape = c(16, 16, 16),
                                                              size = c(2, 2, 2)))) +
  scale_size(range = c(0.2,2), guide = FALSE) +
  theme_light()

CountsMap <- CountsMap + labs(title="Charlotte, North Carolina",
                                subtitle = "Walkability of Residences",
                                x = "Longitude", 
                                y = "Latitude",
                                caption = "Sources: See 'Charlotte, North Carolina: Residences, Amenities, and Sidewalk Network'.\nGood - At least one grocery store, restaurant, and park can be accessed within 0.5 miles on sidewalk network.\nMedium - One or two of the three amenity types can be accessed within 0.5 miles on sidewalk network.\nBad - No grocery stores, restaurants, or parks can be accessed within 0.5 miles on sidewalk network.") +  
  theme(plot.title = element_text(color="grey22", size=16, face="bold"),
        plot.subtitle = element_text(color="grey22", size=12, face = "bold"),
        axis.title.x = element_text(color="grey40", size=10, face="plain"),
        axis.title.y = element_text(color="grey40", size=10, face="plain"),
        plot.caption = element_text(color="grey40", size = 10, hjust = 0, face = "italic"),
        legend.title = element_blank())

CountsMap

#---------------------------------------------------------------------------------
# MAP NEIGHBORHOOD PROFILE AREA - MOUNTAIN ISLAND - CONTAINING WALKBLE RESIDENCES
#---------------------------------------------------------------------------------

# select neighborhood profile areas that intersect with walkable residences
# NPA 266 corresponds with Mountain Island, Charlotte, North Carolina
walkableResidences <- Residence_GetNodes[Residence_GetNodes$walkabilityBinary == 1,]
walkableNPA <- st_intersects(walkableResidences, NPA_Charlotte, sparse = FALSE) # row 261
walkableNPA <- NPA_Charlotte[261,]

# set map range based on NPA 266 (row 261)
mapRange <- c(range(st_coordinates(walkableNPA)[,1]),range(st_coordinates(walkableNPA)[,2]))

# make map
mountainIslandMap <- ggplot() +
  # Charlotte boundary
  geom_sf(data = NPA_Charlotte$geometry, lwd = 0) +
  geom_sf(data = NPA_Charlotte[261, "geometry"], stroke = 0.25) +
  # centroids of residential land parcels
  geom_sf(data = Residence_GetNodes, aes(color = walkability), show.legend = "point", stroke = 0, alpha = 1, size = 0.75) + 
  # sidewalk edges and nodes
  geom_sf(data = Graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(fill = "Sidewalk"), show.legend = "line", size = 0.2) + 
  geom_sf(data = Graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), fill = "grey61", size = 0.4, stroke = 0, alpha = 1) + 
  # legend for sidewalk line
  scale_color_manual(values = c("Sidewalk" = "grey61"),
                     guide = guide_legend(override.aes = list(linetype = "solid", shape = NA, size = c(2)))) +
  # legend for points 
  scale_color_manual(values = c("Medium" = "deepskyblue1", "Good" = "palegreen3", "Bad" = "indianred1"),
                     guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "blank"),
                                                              shape = c(16, 16, 16),
                                                              size = c(2, 2, 2)))) +
  # set coordinate bounding box
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
  theme_light()

mountainIslandMap <- mountainIslandMap + labs(title="Mountain Island, Charlotte, North Carolina",
                              subtitle = "Walkability of Residences",
                              x = "Longitude", 
                              y = "Latitude",
                              caption = "Sources: See 'Charlotte, North Carolina: Residences, Amenities, and Sidewalk Network'.\nGood - At least one grocery store, restaurant, and park can be accessed within 0.5 miles on sidewalk network.\nMedium - One or two of the three amenity types can be accessed within 0.5 miles on sidewalk network.\nBad - No grocery stores, restaurants, or parks can be accessed within 0.5 miles on sidewalk network.") +  
  theme(plot.title = element_text(color="grey22", size=16, face="bold"),
        plot.subtitle = element_text(color="grey22", size=12, face = "bold"),
        axis.title.x = element_text(color="grey40", size=10, face="plain"),
        axis.title.y = element_text(color="grey40", size=10, face="plain"),
        plot.caption = element_text(color="grey40", size = 10, hjust = 0, face = "italic"),
        legend.title = element_blank())

mountainIslandMap


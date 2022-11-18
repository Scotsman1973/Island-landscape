library(dplyr) # used to manipulate csv files/data frames
library(readr) # used to open csv
library(sp) # used to manipulate spatialpointdataframe
library(rgdal) # used for coordinate transformation
library(spatstat)
library(maptools)
library(rgeos)
library(tibble)
library(plyr)
library(geosphere)
library(ggplot2)

# delete files and directories created when the program was previously run
file2delete <- "prehistoric_data"
folder_one2delete <- "Sites"
folder_two2delete <- "Shapefiles"

setwd('C:/Mythings/PhD/Data/')

if (file.exists (file2delete)) {
  unlink (file2delete)
}
if (file.exists(folder_one2delete)) {
  unlink (folder_one2delete, recursive = TRUE)
}
if (file.exists(folder_two2delete)) {
  unlink (folder_two2delete, recursive = TRUE)
}

# create directories
dir.create("Sites")
dir.create("Shapefiles")

#for use with tesselation
Outline <- readOGR(dsn = "C:/Mythings/PhD/Data/Contours/Unst_contours", layer = "Outline")
Outline = spTransform(Outline, CRS("EPSG:27700"))
Outlinewindow <- as.owin(Outline)


# open canmore csv data
canmore <- read_csv('C:/Mythings/PhD/Data/canmore_download.csv')

# create an array of terms to search using
ages <- c('Prehistoric', 'Mesolithic', 'Neolithic', 'Bronze Age', 'Iron Age')

# create an array of terms to search using
site_type <- c('Broch', 'Bank', 'Building', 'Burnt Mound', 'Cairnfield', 'Clearence Cairn',
               'Chambered Cairn', 'Kerb Cairn', 'Burial Cairn', 'Dun', 'Dyke', 'Enclosure', 'Field Boundary',
               'Field System', 'House', 'Hut Circle', 'Lithic Working Site', 'Quarry', 'Souterrain',
               'Standing Stone', 'Stone Heap', 'Stone Row', 'Settlement (Prehistoric)', 'Settlement (Neolithic)',
               'Settlement (Bronze Age)', 'Settlement (Iron Age)', 'Stone Setting', 'Long Cairn', 'Wall',
               'Ritual Building')

# create an empty data frame
prehistoric_data <- data.frame()
site_data <- data.frame()
points_data_frame <- data.frame()
sites_saved <- NULL
areas_sites <- NULL
points_list <- NULL
soils_sites <- NULL

# set the variable for csv data location for shape file
setwd('C:/Mythings/PhD/Data/Shapefiles/')

prehistoric_data_func <- function(ages, canmore){
  for(loopItem in ages) # loopItem is each string in turn
  {
    # this line returns True or False for each row
    # having searched for loopItem in canmore$'SITE TYPE' using grepl
    temp_data <- with(canmore, grepl(loopItem, canmore$`SITE TYPE`))
    
    prehistoric_data <- rbind(prehistoric_data, canmore[temp_data, ])
    print(prehistoric_data)
    
  }
  return(prehistoric_data)
  
}

new_df_different_coords_func <- function(ID, Site_name, Easting, Northing){
  
  coords <- cbind(Easting = as.numeric(as.character(Easting)), 
                  Northing = as.numeric(as.character(Northing)))
  # make sure that the coordinate reference system is properly set, put in a spatialpointdataframe
  spdfcoords <- SpatialPointsDataFrame(coords, data = data.frame(ID, Site_name), 
                                       proj4string = CRS("EPSG:27700"))
  # change the coordinate reference system
  spdfcoords = spTransform(spdfcoords, CRS("EPSG:27700"))
  # change the spatialpointdataframe back to a dataframe
  spdfcoords <- as.data.frame(spdfcoords)
  # rename columns appropriately
  colnames(spdfcoords)[3] <- "Easting"
  colnames(spdfcoords)[4] <- "Northing"
  
  return(spdfcoords)
}

# function that takes each csv file, makes sure it has the correct crs, and saves it as a shp
csv_to_shapefile_function <- function(df, file_name)
{
  spdf <- SpatialPointsDataFrame(df[11:12], data = df[1:10], proj4string = CRS("EPSG:27700"))
  
  work_dir <- getwd()
  # shapefile save command
  writeOGR(spdf, work_dir, file_name, driver="ESRI Shapefile")
  return(spdf)
}

site_data_func <- function(site_type, prehistoric_data)
{
  for(loopItem in site_type) # loopItem is each string in turn
  {
    # this line returns True or False for each row
    # having searched for loopItem in canmore$'SITE TYPE' using grepl
    temp_local_data <- data.frame()
    temp_local_data <- with(prehistoric_data, grepl(loopItem, prehistoric_data$`SITE TYPE`))
    site_data <- rbind(site_data, prehistoric_data[temp_local_data, ])
    
    if (nrow(site_data) > 0)
    {
      # Shorten column names (below 10 characters)
      
      colnames(site_data)[1] <- "ID"
      colnames(site_data)[2] <- "SITE NO."
      colnames(site_data)[5] <- "x"
      colnames(site_data)[6] <- "y"
      site_Description <- gsub(" ", "", loopItem)
      site_data$`SITE TYPE` <- site_Description # standardise the site descriptions
      # calls the coordinate transformation function
      dfcoords <- new_df_different_coords_func(site_data$`ID`, site_data$`SITE NAME`, 
                                               site_data$x, site_data$y)
      
      # attach the Long and Lat columns as additional columns on the dataframe
      site_data <- cbind(site_data, Easting = dfcoords$Easting, Northing = dfcoords$Northing)
      
      # change the column attributes so that the coordinates are treated as numbers
      site_data[1] <- lapply(site_data[1], as.character)
      site_data[5:6] <- lapply(site_data[5:6], as.numeric) #coordinate columns
      site_data[11:12] <- lapply(site_data[11:12], as.double)# coordinate columns
      
      spdf <- csv_to_shapefile_function(site_data, site_data$`SITE TYPE`) # call function and send data
      
      
      # create a file, save the data, and save the filename for later use
      destination_file <- paste('C:/Mythings/PhD/Data/Sites/', site_Description, '.csv', sep= '')
      write_csv(site_data, destination_file)
      sites_saved <- c(sites_saved, site_Description)
      # empty the data frame ready for the next iteration
      site_data <- data.frame()
    }
  }
  #return a list of files created by this function
  return(sites_saved)
}


csv_to_globalenv_func <- function()
{
  
  files <- list.files(path = 'C:/Mythings/PhD/Data/Sites/', pattern = '.csv')
  
  dflist <- lapply(files, function(files)
  {
    setwd('C:/Mythings/PhD/Data/Sites/')
    df <- read.csv(files, header = TRUE)
    
    return(df)
  }
  )
  
  dflist <- setNames(dflist, gsub(".csv", "", files))
  
  list2env(dflist, envir = .GlobalEnv)
  return(dflist)
}

# return data into the empty data frame
prehistoric_data <- prehistoric_data_func(ages, canmore)
# writes the total canmore data reduced to sites with prehistoric component to a csv file
write_csv(prehistoric_data, 'C:/Mythings/PhD/Data/prehistoric_data.csv')

sites_saved <- site_data_func(site_type, prehistoric_data)

site_list <- csv_to_globalenv_func()

#creates a point pattern of brochs, performs a voronoi tessalation and saves it as a shape file
Brochpoints <- ppp(x = Broch[,5], y = Broch[,6],
                   window = Outlinewindow, check = T)

Brochpoints$window <- Outlinewindow
Brochvor <- dirichlet(Brochpoints)

Brochvorsp <- as(Brochvor, "SpatialPolygons")
Brochvorspdf <- SpatialPolygonsDataFrame(Brochvorsp, Broch)
proj4string(Brochvorspdf) = CRS("EPSG:27700")

writeOGR(Brochvorspdf, dsn = "C:/Mythings/PhD/Data/Shapefiles", "Brochvor", driver="ESRI Shapefile")
##############

indiv_tiles <- Brochvor$tiles


# Return a list of the tiles and the tile area
for(number in 1:length(Brochvorsp)) {
  print(paste(Broch$SITE.NAME[number], "is",Brochvorsp@polygons[[number]]@area /1000000, "square kilometers"))
  
}

broch_df_list <-list()

for (number in 1:length(indiv_tiles)) {
  broch_df_list[[number]] <- data.frame()
}



splitpoint_func <- function(site_type2split)
{
  type_points <- ppp(x = site_type2split[,5], y = site_type2split[,6],
                     window = Outlinewindow, check = T)
  split_type_points <- split.ppp(type_points, f=as.tess(indiv_tiles))
  return(split_type_points)
}

for (number in 1:length(site_list)) {
  split_points <- splitpoint_func(site_list[[number]])
  
  points_list <- c(points_list, split_points)
  
}

#loop through each site
i=1

for (site_type_number in 1:length(sites_saved)) {
  #then loop through tiles
  for (tile_number in 1:length(indiv_tiles)) {
    
    points_list[[i]]$markformat <- sites_saved[site_type_number]
    i=i+1
  }
  
}


points_list_rows_func <- function(points_list, site_type_number, tile_number)
{
  df_no <- tile_number + (site_type_number - 1)*15
  points_rows <- cbind(as.data.frame(points_list[[df_no]]))
  if (nrow(as.data.frame(points_list[[df_no]])) > 0)
  {
    points_rows <- cbind(points_rows, points_list[[df_no]]$markformat)
  }
  return(points_rows)
}


for (tile_number in 1:length(indiv_tiles)) {
  for (site_type_number in 1:length(sites_saved)) {
    
    points_rows <- points_list_rows_func(points_list, site_type_number, tile_number)
    
    broch_df_list[[tile_number]] <- rbind.data.frame(broch_df_list[[tile_number]], points_rows, make.row.names = FALSE)
    
  }
}

#rename the columns in each dataframe
for (tile_number in 1:length(indiv_tiles)) {
  
  colnames(broch_df_list[[tile_number]]) <- c("x", "y", "SiteType")
}

for (number in 1:length(broch_df_list)) {
  #add the broch name to the dataframe
  broch_df_list[[number]] <- cbind(broch_df_list[[number]], Broch = Broch$SITE.NAME[number])
  
  #turn the dataframe into a spatialpointdataframe
  broch_df_list[[number]] <- SpatialPointsDataFrame(cbind(
    Longitude = as.double(as.character(broch_df_list[[number]]$x)),
    Latitude = as.double(as.character(broch_df_list[[number]]$y))),
    data = data.frame(broch_df_list[[number]]),
    proj4string = CRS("EPSG:27700"))
  #reproject
  broch_df_list[[number]] = spTransform(broch_df_list[[number]],
                                        CRS("EPSG:4326"))
  #make back into a dataframe
  broch_df_list[[number]] <- as.data.frame(broch_df_list[[number]])
  
  #create dataframe of all long/lat, needs to be a dataframe
  monument_coords <- data.frame(broch_df_list[[number]]$Longitude,
                                broch_df_list[[number]]$Latitude)
  
  #measure distance between one point and all others in the dataframe
  Distance_from_Broch <- distVincentySphere(c(
    broch_df_list[[number]]$Longitude[1], 
    broch_df_list[[number]]$Latitude[1]), monument_coords)
  
  #add the distances as another column in the dataframe
  broch_df_list[[number]] <- cbind (broch_df_list[[number]], Dist_from_Broch = Distance_from_Broch)
  
  #add the distances as another column in the dataframe
  broch_poly_area <- format(round(Brochvorsp@polygons[[number]]@area/1000000, 2), nsmall = 2)
  broch_df_list[[number]] <- cbind (broch_df_list[[number]], Polygon_area = broch_poly_area)
}




for (number in 1:length(broch_df_list)) {
  
  save_title <- paste(broch_df_list[[number]]$Broch[1], ".png", sep = "")
  Broch_Title <- paste(broch_df_list[[number]]$Broch[1], "is", broch_df_list[[number]]$Polygon_area[1], "Square kilometres")
  Polygon_Title <- paste("Archaeological sites within the Voroni polygon of", broch_df_list[[number]]$Broch[1])
  Broch_plot <- ggplot(data = broch_df_list[[number]], aes(y = broch_df_list[[number]]$Dist_from_Broch, x= broch_df_list[[number]]$SiteType, col = broch_df_list[[number]]$SiteType)) + 
    geom_point() + 
    labs(y = "Distance from Broch (metres)", x = Broch_Title, col = "Sites", title = Polygon_Title) + 
    theme(axis.text.x = element_text(size  = 10, angle = 45))
  ggsave(save_title, plot = Broch_plot, width = 11.69, height = 8.27, units = "in")
  areas_sites <- rbind.fill(areas_sites, broch_df_list[[number]])
}

Areas_Brochs_plot <- ggplot(data = areas_sites, aes(y = Dist_from_Broch, x= SiteType, col = SiteType)) + 
  geom_point() + 
  #geom_rect(data = areas_sites, aes(fill = areas_sites$Broch_Landform), xmin = -Inf, xmax = Inf, ymin = - Inf, ymax = -Inf, alpha = 0.3) +
  facet_wrap(~areas_sites$Broch) + 
  labs(y = "Distance from Broch (metres)", x = "", col = "Sites") + coord_flip()
ggsave("Brochs_plot.png", plot = Areas_Brochs_plot, width = 11.69, height = 8.27, units = "in")



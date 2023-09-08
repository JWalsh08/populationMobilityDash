library(sf)
library(ggplot2)
library(scales)
library(crayon)
library(tidyverse)
library(dplyr)
library(plotly)
library(leaflet)
library(rgdal)
library(shiny)
library(RColorBrewer)
library(xts)
library(leaflet.extras)
#POI graphing Data Process
################

#Read in the shapefile as an "sf" object
shapefile = read_sf(dsn ="tl_2021_51_tract", layer= "tl_2021_51_tract")
Montgomery <- readOGR('cb 2018 51 tract 500k.geojson', verbose = FALSE)
#Choose HealthPOIs_Montgomery_VA.csv
health_POIs = read.csv('HealthPOIs_Montgomery_VA.csv')
#montgomery_health_POIs1 = subset(health_POIs, city == "Blacksburg" | city == "Christiansburg")
montgomery_health_POIs2 <- health_POIs %>% distinct(street_address, .keep_all = TRUE)
#Coordinates of hospitals. I've made up 3 here, you can see if you run the second line below this:

y <- montgomery_health_POIs2$latitude
x <- montgomery_health_POIs2$longitude
business <- montgomery_health_POIs2$location_name
coordinates = data.frame(x, y, business)
coordinates

#Plot the coordinates on the map of VA
ggplot() + geom_sf(data = shapefile) + geom_point(data = coordinates, mapping=aes(x=x,y=y),colour="red")

#Convert the coordinates to an "sf" object, so that we can overlay
coordinates_sf = st_as_sf(coordinates, coords = c("x","y"))
#set the projection of the new coordinate object
st_crs(coordinates_sf) <- 4269
#Plot to make sure it works
ggplot() + geom_sf(data = shapefile) + geom_sf(data = coordinates_sf, colour="red")


#Count up the number of hospitals in each census tract
shapefile$pt_count <- lengths(st_intersects(shapefile, coordinates_sf))

#Subset montgomery county and plot only that county
shapefile_montgomery = subset(shapefile, is.element(COUNTYFP,c(121)))
ggplot() + geom_sf(data = shapefile_montgomery, mapping=aes(fill=pt_count)) + 
  geom_sf(data = coordinates_sf, colour="red") + geom_sf_label(data = shapefile_montgomery, aes(label = GEOID))


Montgomery$popup <- paste(Montgomery$GEOID)
coordinates$popup <- paste("<strong>", coordinates$business, "</strong", "/br>")



                            

##########################
#GEOID movement Data process

#Read in data
all_data = read.csv("VA_all.csv")
#Tell R the "date" variable is actually a date
all_data$date2 = as.Date(all_data$date, format = "%Y-%m-%d")

shapefile$county_fips = paste0(shapefile$STATEFP,shapefile$COUNTYFP)
#FIPS codes within Montgomery county and with census data
montgomery_selection = c("51121020500", "51121020201","51121020100", 
                         "51121020600", "51121020202","51121021000", 
                         "51121020900", "51121021400") 

#Construct baseline values for each census tract
GEOID_baselines = data.frame(GEOID = unique(all_data$GEOID))
GEOID_baselines$baseline = 0

for (i in 1:nrow(GEOID_baselines)){
  print(i)
  GEOID_sub = GEOID_baselines$GEOID[i]
  sub_dat = subset(all_data, GEOID == GEOID_sub & date2 > as.Date("2020-02-01") & date2 > as.Date("2020-03-01"))
  
  GEOID_baselines$baseline[i] = mean(sub_dat$flow_in)
}

#Merge baseline with dataset
all_data_with_baseline = merge(all_data, GEOID_baselines, by = "GEOID")
#calculate the % of movement each day, compared to the baseline for that census tract
all_data_with_baseline$adjusted_flow_in = all_data_with_baseline$flow_in/all_data_with_baseline$baseline
#Remove census tracts with no data
all_data_with_baseline = subset(all_data_with_baseline,!is.na(adjusted_flow_in))

all_data_with_baseline$ct_status = "None"
#Subset out hospitals

all_data_with_baseline$ct_status[is.element(all_data_with_baseline$GEOID, montgomery_selection)] = "Montgomery"

#Create a dataset with the 10% quantile of movement, mean, and 90th for the census tracts marked "Hospital"
#First, we'll subset ONLY data where the census tract has hospitals
all_data_with_baseline_mont = subset(all_data_with_baseline, ct_status == "Montgomery")

montgomery_dataset = data.frame(GEOID = all_data_with_baseline_mont$GEOID, date = unique(all_data_with_baseline_mont$date2), 
  mean = 0,
  high = 0,
  low = 0)
for (i in 1:nrow(montgomery_dataset)){
  subset_data = subset(all_data_with_baseline_mont, date2 == montgomery_dataset$date[i])
  montgomery_dataset$mean[i] = mean(subset_data$adjusted_flow_in)
  montgomery_dataset$high[i] = quantile(subset_data$adjusted_flow_in,.9)
  montgomery_dataset$low[i] = quantile(subset_data$adjusted_flow_in,.1)
}

#Create a second one with all census tracts
general_dataset = data.frame(date = unique(all_data_with_baseline$date2), 
                            mean = 0,
                            high = 0,
                            low = 0)
for (i in 1:nrow(general_dataset)){
  subset_data = subset(all_data_with_baseline, date2 == general_dataset$date[i])
  general_dataset$mean[i] = mean(subset_data$adjusted_flow_in)
  general_dataset$high[i] = quantile(subset_data$adjusted_flow_in,.9)
  general_dataset$low[i] = quantile(subset_data$adjusted_flow_in,.1)
}

montgomery_dataset$rank = "mont"
general_dataset$rank = "all"



##############################
#GEOID 2-D Plots




ggplot(all_data_with_baseline_mont, aes(x=date2, y=adjusted_flow_in, group=GEOID)) + 
  geom_line(aes(color=GEOID))+
  scale_y_continuous(name="Relative mobility")+
  scale_x_date(limits=c(as.Date("2019-01-01"),as.Date("2020-01-17")),date_labels="%b %d %y",name="") + theme_bw(base_size=14)


colors = rainbow(length(montgomery_selection))
theme_update(plot.title = element_text(hjust = 0.5))
for (i in 1:length(montgomery_selection)){
  sub_dat2 = subset(all_data_with_baseline_mont, GEOID == montgomery_selection[i])
  print(ggplot(sub_dat2, aes(x=date2, y = adjusted_flow_in))+
    geom_line(color=colors[i])+
    ggtitle(montgomery_selection[i])+
    scale_y_continuous(n.breaks = 10, limit = c(0, 5), name="Relative mobility")+
    scale_fill_brewer(palette="Set1")+scale_x_date(date_breaks = "3 month", date_minor_breaks = "1 month", limits=c(as.Date("2019-01-01"),as.Date("2021-04-15")),date_labels="%b %y",name="") + theme_bw(base_size=14)+
    theme(plot.title = element_text(hjust = 0.5)))
}

##############
#Leaflet Maps

icons <- awesomeIcons(
  icon = "ios-medkit",
  iconColor = 'red',
  library = 'ion',
  markerColor = "blue"
)

#Health POI plot
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = Montgomery,
              stroke = TRUE,
              weight = 0.5,
              color = "#37B1ED",
              opacity = 1,
              fillColor = "#37B1ED",
              fillOpacity = 0.25,
              popup = ~popup,
              highlightOptions = highlightOptions(color = "#E2068A", weight = 1.5,
                                                  bringToFront = TRUE, fillOpacity = 0.5),) %>%
  addAwesomeMarkers(data = coordinates, 
                    ~x, ~y, icon = icons,
                    clusterOptions = markerClusterOptions(),
                    popup = ~popup) %>%
  setView(lng = -80.412,
          lat = 37.187,
          zoom = 10)
m

#Choropleth plots
#################
#Plotting one variable, testing purposes
testset <- health_POIs[1,]
testset


choro <- leaflet() %>%
  addBootstrapDependency() %>%
  setView(testset$latitude, testset$longitude, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addGeoJSONChoropleth(
    readr::read_file('cb 2018 51 tract 500k.geojson', valueProperty = )
  )
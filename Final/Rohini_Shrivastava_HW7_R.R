########################################
#Course: IST687
#Assignment: HW #7
#Name: Rohini Shrivastava
#Date: 8/18/20
#Notes: Rev 1.0
########################################
install.packages("C:\\Users\\shriv\\Downloads\\zipcode_1.0.tar.gz", repos = NULL, type = "source")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("maps")
install.packages("mapproj")

library(ggplot2)
library(ggmap)
library(zipcode)
library(mapproj)

#STEP 1

df <- read.csv("C:\\Users\\shriv\\Downloads\\MedianZIP_2_2_2\\MedianZIP-3.csv")
#df

colnames(df) <- c("zip","median","mean","population")
#df
data(zipcode)
df$zip <- clean.zipcodes(df$zip)

merge_zip <- merge(df, zipcode, by="zip")
merge_zip <- merge_zip[which(merge_zip$state != "AK" & merge_zip$state != "HI"),]
merge_zip

#########################################
#STEP 2

med_state <- aggregate(as.numeric(median) ~ state, merge_zip, mean) 
pop_state <- aggregate(as.numeric(population) ~ state, merge_zip, mean)
med_pop <- merge(med_state, pop_state, by="state")
colnames(med_pop) <- c("state abb","median","population")
#med_pop
states <- data.frame(state.name, state.abb)
states$state.name <-tolower(states$state.name)
colnames(states)<- c("state name", "state abb")
med_pop_state <- merge(med_pop, states, by="state abb")
#

colnames(med_pop_state) <- c("state abb","median","population", "states")
med_pop_state

us <- map_data("state")
us
map.med <- ggplot(med_pop_state, aes(map_id = states))
map.med <- map.med + geom_map(map = us, aes(fill=median))
map.med <- map.med + expand_limits(x=us$long, y=us$lat)
map.med <- map.med + coord_map() + ggtitle("State Median Income")
map.med


map.pop <- ggplot(med_pop_state, aes(map_id = states))
map.pop <- map.pop + geom_map(map = us, aes(fill=population))
map.pop <- map.pop + expand_limits(x=us$long, y=us$lat)
map.pop <- map.pop + coord_map() + ggtitle("State Median Population")
map.pop

#############################################
#STEP 3


states_1 <- data.frame(state.name, state.abb)
states_1$state.name <-tolower(states_1$state.name)
colnames(states_1)<- c("states", "state")
merge_zip_1 <- merge(merge_zip, states_1, by="state")
merge_zip_1$median <- as.numeric(merge_zip_1$median)
str(merge_zip_1)
map.state <- ggplot(merge_zip_1, aes(map_id=states))

map.state <- map.state + geom_map(map = us, aes(fill="black"))
map.state <- map.state + expand_limits(x=us$long, y=us$lat)
map.state <- map.state + geom_point(data=merge_zip_1, aes(x=longitude, y=latitude, color=median))
map.state <- map.state + coord_map() + ggtitle("Zip Median Income")

map.state
#, map_id = states
############################################
#STEP 4

map.density <- ggplot(merge_zip_1, aes(map_id=states))
map.density <- map.density + geom_map(map = us, aes(fill=median))
map.density <- map.density + expand_limits(x=us$long, y=us$lat)
map.density <- map.density + stat_density2d(data=merge_zip_1, aes(x=longitude, y=latitude, fill= ..level.., 
                                            alpha = 0.25), bins=10, size =0.25, geom= "polygon", inherit.aes = FALSE)
map.density <- map.density + geom_point(data=merge_zip_1, aes(x=longitude, y=latitude, color=median))
map.density <- map.density + coord_map()
map.density

#############################################
#zoomGeo <- geocode("New York, ny")

nyx = as.numeric("-74.00594")
nyy = as.numeric("40.71278")

zoomamount <- 3

xlim <- c(nyx-zoomamount, nyx+zoomamount)
ylim <- c(nyy-zoomamount, nyy+zoomamount)


#map.ny <- map.ny + geom_point(data=merge_zip_1, aes(x=longitude, y=latitude, color=median))
#map.ny <- map.ny + stat_density2d(data=merge_zip_1, aes(x=longitude, y=latitude), size =0.25)


zoom.comp <- merge_zip_1
zoom.comp <- zoom.comp[zoom.comp$longitude > xlim[1], ]
zoom.comp <- zoom.comp[zoom.comp$longitude < xlim[2], ]
zoom.comp <- zoom.comp[zoom.comp$latitude > ylim[1], ]
zoom.comp <- zoom.comp[zoom.comp$latitude < ylim[2], ]

zoom.comp

#median per zipcode
map.ny <- ggplot(merge_zip_1, aes(map_id=states))
map.ny <- map.ny + geom_map(map = us, aes(fill=median))
map.ny <- map.ny + expand_limits(x=xlim, y=ylim)
map.ny <- map.ny + geom_point(data=zoom.comp, aes(x=longitude, y=latitude, color=median))
map.ny <- map.ny + coord_map()
map.ny

#density per zipcode
dens.ny <- ggplot(merge_zip_1, aes(map_id=states))
dens.ny <- dens.ny + geom_map(map = us, aes(fill=median))
dens.ny <- dens.ny + expand_limits(x=xlim, y=ylim)
dens.ny <- dens.ny + geom_point(data=zoom.comp, aes(x=longitude, y=latitude, color=median))
dens.ny <- dens.ny + stat_density2d(data=zoom.comp, aes(x=longitude, y=latitude, fill= ..level.., 
                          alpha = 0.25), bins=5, size =0.25, geom= "polygon", inherit.aes = FALSE)
dens.ny <- dens.ny + coord_map()
dens.ny

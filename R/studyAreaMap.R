# Makes maps of study area
# Sean Hendryx


library(ggmap)
library(dplyr)
library(maps)
library(mapdata)
library(rgdal)
library(ggsn)
library(sf)


#Get basemap using ggmap:
#az = c(left = -115.54533055555555, top = 37.506458333333335, right = -107.12299722222221, bottom = 30.185680555555557)
#map = get_stamenmap(az, zoom = 7, maptype = "terrain-background")
#ggmap(map)

#Let's also try just getting az shapefile:
states <- map_data("state")
azDf <- subset(states, region == "arizona")
azBase = ggplot(data = azDf, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.2) + 
  geom_polygon(color = "black", fill = "gray") + theme_classic()
  #, fill = "gray"
azBase

phoenix = c(33.45, -112.06666666666666)
tucson = c(32.22166666666667, -110.9263888888889)
study = c(31.821379, -110.866136)

mapLabs = data.frame(
  long = c(phoenix[2], tucson[2], study[2]),
  lat = c(phoenix[1], tucson[1], study[1]),
  name = c("Phoenix", "Tucson", "Study Area"),
  stringsAsFactors = FALSE
)

rownames(mapLabs) = mapLabs$name

azBase = azBase + 
  geom_point(data = mapLabs, aes(x = long, y = lat), color = "black", size = 2, inherit.aes = FALSE) + 
  geom_text(data = mapLabs, aes(x = long, y = lat, label = rownames(mapLabs)), size = 7, hjust = 0, nudge_x = 0.05, inherit.aes = FALSE)

#dev.off()
#dev.new()
azBase

# Now make map of study area:
shapefile = readOGR("/Users/seanmhendryx/Google Drive/Thesis/Data/RectangularStudyArea.shp")

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(shapefile)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
SAmap <- ggplot() +
  geom_path(data = shapefile_df, 
          aes(x = long, y = lat, group = group),
          color = 'gray', fill = 'white', size = .2)


dsn = "/Users/seanmhendryx/Google Drive/Thesis/Data/RectangularStudyArea.shp"

#dsn <- system.file('extdata', package = 'ggsn')

# Map in geographic coordinates
shape = st_read(dsn = dsn, quiet = TRUE)

blankSA = SAmap + blank() + north(shape) +
  scalebar(data = shapefile_df, dist = 0.005, dd2km = TRUE, model = 'WGS84',  st.size = 0) #+ opts(legend.position = c(0, 1), legend.justification = c(0, 1))#+ theme(legend.position = c(0, 1)) #+  theme(legend.justification = "top") 


saLoc = c(lon = -110.866136, lat =31.821379)
sa = c(top = 31.821706,  left = -110.866841, bottom  =  31.820827, right = -110.865761)

saMap = get_map(saLoc, zoom = 19, maptype = 'satellite')
limited = ggmap(saMap)  +
  geom_polygon(aes(x = long, y = lat, group = group), data = shapefile_df,
  colour = 'white', fill = NA, size = .3) +
  scale_x_continuous(limits = c(sa['left'], sa['right']), expand = c(0, 0)) +
  scale_y_continuous(limits = c(sa['bottom'], sa['top']), expand = c(0, 0))

dev.new()
limited

limited = limited + blank()
dev.new()
limited






# North arrow and scale bar using ggsn
# http://oswaldosantos.github.io/ggsn/
mapSN = limited + blank() 
dev.new()
mapSN


mapSN = mapSN + north(shape) +
  scalebar(data = shape, dist = 0.005, dd2km = TRUE, model = 'WGS84',  st.size = 0) #+ opts(legend.position = c(0, 1), legend.justification = c(0, 1))#+ theme(legend.position = c(0, 1)) #+  theme(legend.justification = "top") 

mapSN









mapSN = limited +
  blank() +
  north(shapefile_df) +
  scalebar(shapefile_df, dist = 5, dd2km = TRUE, model = 'WGS84')

dev.new()
mapSN


# reproducing example
library(ggsn); library(sf); library(ggplot2)
dsn = "/Users/seanmhendryx/Google Drive/Thesis/Data/RectangularStudyArea.shp"

#dsn <- system.file('extdata', package = 'ggsn')

# Map in geographic coordinates
shape <- st_read(dsn = dsn, quiet = TRUE)

ggm1 <- ggplot(map) +
    geom_sf() 

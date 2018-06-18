# Interactive-map-practice
Trying out different types of flexdashboard maps with tree canopy data, https://rmarkdown.rstudio.com/flexdashboard/examples.html

---
title: "Waste Lands - America's forgotten nuclear legacy"
author: Philipp Ottolinger
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

```{r setup, include=FALSE}
library(flexdashboard)
library(shiny)
library(jsonlite)
library(maptools)
library(tidyr)
library(dplyr)
library(purrr)
library(leaflet)
library(plotly)
library(RPostgreSQL)
library(sf)
library(ggplot2)
library(dplyr)

setwd("C:/Users/gloria.dasanker/Desktop/Readings/GIS/R/MLT_Analytics_samplework/MLT_Analytics_samplework")

###pw <- scan("~/.pgpass2", what="")

###conn = dbConnect(PostgreSQL(), dbname='nycgis', user='postgres', host='mlt_host', password=pw) #[PW not needed b/c that's stored in local config files]

# fgdb <- "../../media/sf_Treglia_Data/Dropbox/TNC/Trees/MLT_Analytics/NYC_20Urban_20Tree_20Canopy_20Assessment_20Metrics_202010.gdb"


# The input file geodatabase

# List all feature classes in a file geodatabase
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)
# 
# # Read the feature class
# fc <- readOGR(dsn=fgdb,layer="NYC_Community_Districts_Version10C")
# 
# 
# neighbs <- st_read(dsn=fgdb,layer="NYC_Neighborhoods_Singlepart_Version10C")
# head(nycbs)
# fc_list
# 
# tc_neighbs <- st_read(dsn=fgdb, layer="TreeCanopy_Metrics_NYC_Neighborhoods_Singlepart_Version10C", geometry_column = NULL)
# # Determine the FC extent, projection, and attribute information
# summary(fc)

## at scale of tabulation area
neighbdata <- st_read("neighborhoodData.geojson")
st_crs(neighbdata) <- 2263 ## telling it what the projection is


## pull out columns
neighbdata.red <- neighbdata[,c("NEIGH_CODE", "Alt_Name", "TreeCanopy.Metrics.NYC.Neighborhoods.Singlepart.Version10C_TC_E_P", "TreeCanopy.Metrics.NYC.Neighborhoods.Singlepart.Version10C_TC_P_P", "TreeCanopy.Metrics.NYC.Neighborhoods.Singlepart.Version10C_TC_Pi_P", "TreeCanopy.Metrics.NYC.Neighborhoods.Singlepart.Version10C_TC_Pv_P")]

#NOTE - Potential Canopy Should be Existing Canopy + P_P + Pi_P + Pv_P

## renaming columns
## potential total canopy is existing canopy + land that could be canopy
names(neighbdata.red)[3:6] <- c("ExistingPctCanopy", "PotentialPctCanopy_Ttl", "PotentialPctCanopy_Imperv", "PotentialPctCanopy_Veg")
head(neighbdata.red)
neighbdata.red[][[3]] <- round(neighbdata.red[][[3]], 1)
neighbdata.red[][[4]] <- round(neighbdata.red[][[4]], 1)
neighbdata.red[][[5]] <- round(neighbdata.red[][[5]], 1)
neighbdata.red[][[6]] <- round(neighbdata.red[][[6]], 1)


```

Page 1
================================

<!-- each page is a differeny flexdashboard ui, but can the flexdashboard type change? --> 

Column {data-width=650}
-----------------------------------------------------------------------


### Panel A

```{r}

library(plotly)
library(leaflet)
library(crosstalk)
library(rgdal)
library(sp)
library(DT)

## javascript library(D3)
## plotly is based on d3 and is used for visualization
p <- plot_ly(neighbdata.red, x = ~ExistingPctCanopy, y = ~PotentialPctCanopy_Ttl) %>% add_markers(alpha = 0.5,text = ~paste(Alt_Name)) 


#%>%

               
               #hoverinfo = 'text',
               #text = ~paste(boroughs$boroname)) %>%
  #highlight("plotly_selected", dynamic = TRUE)


## transforms data
## creates the leaflet map
## customization

## interactive map!
map <- st_transform(neighbdata.red,crs = 4326) %>% leaflet() %>% 
   addTiles() %>% addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
     opacity = 1.0, fillOpacity = 0.5,
     fillColor = ~colorQuantile("Greens", ExistingPctCanopy)(ExistingPctCanopy), highlightOptions = highlightOptions(color = "white", weight = 2,
       bringToFront = TRUE))


###
## creating data table
sd_neigh <- SharedData$new(neighbdata.red)

neigh_centroids <- st_transform(st_centroid(neighbdata.red),crs = 4326)

sd_neigh <- SharedData$new(neigh_centroids)

test <- bscols(widths=c(7, 6, 6), device="md",
               list(filter_slider("ExistingPctCanopy", "Existing Percent Canopy", sd_neigh,
                                  ~ExistingPctCanopy, step=10, round=1, sep=""),
                    filter_slider("PotentialPctCanopy_Ttl", "Potential Percent Canopy", sd_neigh,
                                  ~PotentialPctCanopy_Ttl, step=10, round=1)),
              leaflet(sd_neigh) %>% 
              addProviderTiles(providers$CartoDB.Positron) %>%
                  addPolygons(data=st_transform(neighbdata.red, crs=4326),color = "#444444", weight = 1, 
                            smoothFactor = 0.5,
                            opacity = 1.0, 
                            fillOpacity = 0.5,
                            fillColor = ~colorQuantile("Greens", ExistingPctCanopy)(ExistingPctCanopy), 
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = FALSE), group="Existing Percent Canopy") %>% 
                addPolygons(data=st_transform(neighbdata.red, crs=4326),color = "#444444", weight = 1, 
                            smoothFactor = 0.5,
                            opacity = 1.0, 
                            fillOpacity = 0.5,
                            fillColor = ~colorQuantile("Greens", 
                                                       PotentialPctCanopy_Ttl)(PotentialPctCanopy_Ttl), 
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = FALSE), group="Potential Percent Canopy") %>% 
              addCircleMarkers(radius=5, stroke=TRUE, color="yellow", weight=1,fillColor = "#03F",
                               opacity=1,fillOpacity=1, group="pts",
                               popup=~as.character(Alt_Name)) %>%
              addLayersControl(baseGroups = c("Existing Percent Canopy", "Potential Percent Canopy"),overlayGroups="Neighborhoods",
                              options = layersControlOptions(collapsed = FALSE)),
              plot_ly(sd_neigh, x = ~ExistingPctCanopy, y = ~PotentialPctCanopy_Ttl) %>% 
                  add_markers(alpha = 0.5,text = ~Alt_Name) %>% 
                  layout(xaxis = list(title="Existing % Canopy (2010)"), 
                         yaxis = list(title="Potential % Canopy")) %>%
                  highlight("plotly_hover", off='plotly_deselect', 
                            opacityDim=getOption("opacityDim",  0.2)))

test
```


Column {data-width=350}
-----------------------------------------------------------------------
### Panel B

You can also embed plots, for example:

```{r, echo=FALSE}
plot(neighbdata.red["ExistingPctCanopy"],)
```

Page 2
================================

Column { vertical_layout: scroll}
-----------------------------------------------------------------------

### Panel A 

```{r}

plot(neighbdata[10],) ## to plot first column of data
plot(neighbdata[12],)

```

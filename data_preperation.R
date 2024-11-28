# Load Libraries
library(dplyr)
library(sp)
library(sf)
library(mclust)
library(geojsonio)
library(rgdal)

# Load Albuquerque Dorling cartogram
github.url <- "https://github.com/DS4PS/cpp-529-fall-2020/raw/main/data/phx_dorling.geojson"
abq <- geojson_read(x = github.url, what = "sp")

# Reproject the map
abq <- spTransform(abq, CRS("+init=epsg:3395"))

# Convert to sf format
abq_sf <- st_as_sf(abq)

# Select relevant variables
keep.these <- c("pnhwht12", "pnhblk12", "phisp12", "pntv12", "pfb12", "polang12", 
                "phs12", "pcol12", "punemp12", "pflabf12", "pprof12", "pmanuf12", 
                "pvet12", "psemp12", "hinc12", "incpc12", "ppov12", "pown12", 
                "pvac12", "pmulti12", "mrent12", "mhmval12", "p30old12", "p10yrs12", 
                "p18und12", "p60up12", "p75up12", "pmar12", "pwds12", "pfhh12")

# Standardize the data
clustering_data <- abq_sf %>%
  st_drop_geometry() %>%
  select(all_of(keep.these)) %>%
  na.omit() %>%
  apply(2, scale)

# Perform clustering
set.seed(1234)
clustering_model <- Mclust(clustering_data, verbose = TRUE)

# Save model to avoid rerun
saveRDS(clustering_model, "clustering_model.rds")

# Add cluster classifications
abq_sf$cluster <- as.factor(clustering_model$classification)

# Add descriptive cluster labels
abq_sf$cluster_label <- NA
abq_sf$cluster_label[abq_sf$cluster == "1"] <- "Affluent, Predominantly White Neighborhoods"
abq_sf$cluster_label[abq_sf$cluster == "2"] <- "Diverse, Low-Income Communities"
abq_sf$cluster_label[abq_sf$cluster == "3"] <- "Working-Class Neighborhoods"
abq_sf$cluster_label[abq_sf$cluster == "4"] <- "Established, Wealthy Suburban Areas"
abq_sf$cluster_label[abq_sf$cluster == "5"] <- "Suburban Mixed Communities"
abq_sf$cluster_label[abq_sf$cluster == "6"] <- "Middle-Income Suburbs"
abq_sf$cluster_label[abq_sf$cluster == "7"] <- "Hispanic, Low-Income Communities"
abq_sf$cluster_label[abq_sf$cluster == "8"] <- "Middle-Class, Diverse Communities"
abq_sf$cluster_label <- as.factor(abq_sf$cluster_label)

# Save the prepared data as GeoJSON

dir.create("C:/Final Dashboard/New folder/lab", recursive = TRUE)

library(geojsonio)
geojson_write(abq_sf, file = "processed_abq_sf.geojson", geometry = "polygon")

geojson_write(abq_sf, file = "C:/Final Dashboard/New folder/lab/processed_abq_sf.geojson", geometry = "polygon")

file.exists("processed_abq_sf.geojson")

#Test

file.exists("C:/Final Dashboard/New folder/lab/processed_abq_sf.geojson")

# Load from the working directory
loaded_abq_sf <- geojson_read("processed_abq_sf.geojson", what = "sp")
plot(loaded_abq_sf)

# Load from the specified directory
loaded_abq_sf <- geojson_read("C:/Final Dashboard/New folder/lab/processed_abq_sf.geojson", what = "sp")
plot(loaded_abq_sf)

# Check
str(abq_sf)

# Check

str(loaded_abq_sf)
head(loaded_abq_sf@data)  # View the attribute table



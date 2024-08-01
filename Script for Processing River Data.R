## Packages

# Required packages
req_packages = c("tidyverse", "sf")

# Install any required packages
inst_packages <- req_packages %in% installed.packages()
if(length(req_packages[!inst_packages]) > 0) install.packages(req_packages[!inst_packages])

# Load packages
lapply(req_packages, require, character.only=TRUE)


## Read Data

# Read EC Data
id <- readRDS("~/Documents/id.rds") %>%
  filter(land == TRUE)

# Read Shapefile
ec_shp <- st_read("~/Documents/east-coast-flood/east-coast-flood.shp")
rivers_shp <- st_read("~/Documents/river-regions/RiverRegionWebM.shp")


## Tidying

# Filter for SEC & NEC
rivers_shp <- rivers_shp %>%
  filter(division == "South East Coast (NSW)" | division == "North East Coast") %>%
  select(division, rivregname, rivregnum, geometry) %>%
  st_transform(geometry, crs = 4283)

# Convert to SF
rivers_sf <- st_sf(rivers_shp, crs = 4283) %>%
  mutate(river_id = seq(1, nrow(rivers_shp), 1))

coords_sf <- id %>%
  select(lat, lon) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4283)

## Linking IDs and Rivers

# Find which coords are associated with river region
contain <- st_within(coords_sf, rivers_sf, sparse = FALSE) %>%
  t() %>%
  as_tibble()

true_rows <- apply(contain, 2, function(col) which(col == TRUE))

# Bind id with rivers
id <- id %>%
  mutate(river_id = as.numeric(true_rows)) %>%
  na.omit()

rivers <- merge(id, rivers_sf, by = "river_id")

# Map back to original columns in id dataframe
# ID gives point reference with AGCD data
v <- paste0("V", seq(1, 11773, 1))
positions <- match(v, rivers$ref)
map_to_id <- paste0("V", seq(1, 11773, 1)) %>%
  as_tibble() %>%
  mutate(ref = value,
         river = rivers[positions, 11]) %>%
  select(-value)

id <- readRDS("~/Documents/id.rds")

grid_id <- id %>%
  as_tibble() %>%
  mutate(river = map_to_id$river) %>%
  drop_na(river) %>%
  select(-land)

saveRDS(grid_id, "grid_id.rds")


## River ID Dataframe

# Extract Unique Rivers
river_nam <- unique(rivers$rivregname)

# Filter for rivers in SEC/NEC w/ gridpoints
rivers_sf <- rivers_sf %>%
  mutate(sample = ifelse(rivregname %in% river_nam, TRUE, FALSE)) %>%
  filter(sample == TRUE) %>%
  select(-c(sample, river_id))


saveRDS(rivers_sf, "river_id.rds")


## Precipitation

# Read data
precip <- readRDS("~/Documents/CLEX Summer Research/East Coast/Precipitation Data/precipitation.rds")

# Add river to id
id <- id %>%
  as_tibble() %>%
  mutate(river = map_to_id$river) %>%
  select(-land)

# Find IDs associated with rivers to extract precipitation data
true_rows <- is.na(id$river) %>%
  as_tibble()

true_rows <- which(true_rows == FALSE)

river_precip <- data.frame(matrix(NA, nrow = nrow(precip), ncol = length(true_rows)))
for(i in 1:length(true_rows)) {
  river_precip[,i] <- precip[,true_rows[i]]
}

# Rename columns in precipitation dataframe to link with gridpoint IDs
colnames <- id %>%
  drop_na(river) %>%
  select(ref)

names(river_precip) <- colnames$ref

# Convert to long dataframe
river_precip <- river_precip %>%
  as_tibble() %>%
  mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), 1)) %>%
  pivot_longer(cols = -date,
               names_to = "id", 
               values_to = "mm")

# Add river to long data
positions <- match(river_precip$id, rivers$ref)

river_precip <- river_precip %>%
  mutate(river = rivers[positions, 11]) %>%
  relocate(river, .before = mm)

saveRDS(river_precip, "river_precip.rds")


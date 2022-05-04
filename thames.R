library(readxl)
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(leaflet.extras2)
library(viridis)

# Stoke st george coordinates are incorrect
# What proportion of sites have nothing that exceeds a limit?
# Rank companies in terms of the median values we obtain - summed ranking? Then deep dive - is that due to a sampling issue or do some
# areas have greater risks

determinands <- read_csv("determinands.csv")

thames <- read_excel("thames.xlsx")
southwest <- read_excel("southwest.xlsx")
severn <- read_excel("severn.xlsx")
wessex <- read_excel("wessex.xlsx")
southern <- read_excel("southern.xlsx")
yorkshire <- read_excel("yorkshire.xlsx")
united <- read_excel("united.xlsx")
mystery1 <- read_excel("682e02.xlsx")
mystery2 <- read_excel("fe8323.xlsx")

all_data <- bind_rows(thames,
                    southwest,
                  severn,
                    wessex,
                    southern, 
                  yorkshire,
                  united,
                    mystery1,
                  mystery2, .id = "company")


df <- all_data %>%
  separate(SampleDateTime, into = c("Date", "Time"), sep = " ") %>%
  mutate(Date = as.Date(Date, format = '%d/%m/%Y')) %>%
  rename(treatment_plant = TreatmentPlant, sample_location = SampleLocationName, value = SampleValue, determinand = NameDeterminandName, units = UnitsName) %>%
  left_join(determinands)


# Using scaled median values ---------------

scaled_medians <-
  df %>%
  filter(treatment_plant != 'Stoke St George') %>%
  group_by(company, treatment_plant, Latitude, Longitude, group, determinand)%>%
  summarise(median = median(value)) %>% # Sum across all time
  ungroup() %>%
  group_by(company, determinand) %>% # Keep scaling within companies to account for variation in how they measure determinands in samples
  mutate(scaled_median = scale(median)) %>%
  filter(scaled_median > 3)

head(scaled_medians)

medians_sf <- st_as_sf(scaled_medians, coords =c('Longitude', 'Latitude'), crs = 4326) 

factpal <- colorFactor(palette = viridis(5), scaled_medians$group)

leaflet(medians_sf) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius = ~ scaled_median, 
                   stroke = FALSE, 
                   fillOpacity = 0.5, 
                   label = ~ determinand,
                   popup = ~ treatment_plant,
                   color = ~ factpal(group),
                   group = "Determinands") %>%
  addLegend("bottomright", pal = factpal, values = ~ group,
            title = "Category",
            opacity = 1) %>%
              # Layers control
              addLayersControl(
                overlayGroups = c("Determinands"),
                options = layersControlOptions(collapsed = FALSE)
              )
  

groups = as.character(unique(scaled_medians$group))

map <- leaflet(medians_sf) %>% 
  addProviderTiles(providers$CartoDB.Positron)
  

for(g in groups){
  
  d <- scaled_medians[scaled_medians$group == g, ]
  
  map <- map %>% addCircleMarkers(data = d,
                                  radius = ~ scaled_median, 
                                  stroke = FALSE, 
                                  fillOpacity = 0.5, 
                                  color = ~ factpal(group),
                                  label = ~ determinand,
                                  popup = ~ treatment_plant,
                                 group = g)
  
}
map %>% addLayersControl(overlayGroups = groups)

# ------ Raw medians

raw_medians <-
  df %>%
  filter(treatment_plant != 'Stoke St George') %>%
  group_by(company, treatment_plant, Latitude, Longitude, group, determinand)%>%
  summarise(median = median(value))  # Sum across all time

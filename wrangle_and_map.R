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

units_lookup <- df %>% select(determinand, units) %>% unique() %>% filter(units != 'Not Set') %>%
  mutate(unit_short = ifelse(units == 'micrograms per litre', 'µg/L', 'mg/L'))



# Using scaled median values ---------------

scaled_medians <-
  df %>%
  filter(treatment_plant != 'Stoke St George') %>%
  group_by(company, treatment_plant, Latitude, Longitude, group, determinand)%>%
  summarise(median = median(value)) %>% # Sum across all time
  ungroup() %>%
  group_by(company, determinand) %>% # Keep scaling within companies to account for variation in how they measure determinands in samples
  mutate(scaled_median = scale(median)) %>%
  filter(scaled_median > 2) %>%
  left_join(units_lookup) %>%
  mutate(determinand_value = paste(determinand,":", median, unit_short))

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
  

groups <- as.character(unique(scaled_medians$group)) %>% sort()

map <- leaflet(medians_sf) %>% 
  addProviderTiles(providers$CartoDB.Positron)
  

for(g in groups){
  
  d <- scaled_medians[scaled_medians$group == g, ]
  
  map <- map %>% addCircleMarkers(data = d,
                                  radius = ~ scaled_median, 
                                  stroke = FALSE, 
                                  fillOpacity = 0.5, 
                                  color = ~ factpal(group),
                                  label = ~ determinand_value,
                                  popup = ~ treatment_plant,
                                 group = g)
  
}
map %>% addLayersControl(overlayGroups = groups, options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(pal = factpal, values = ~ group, opacity = 0.8, title = "")

# ------ Raw medians

raw_medians <-
  df %>%
  filter(treatment_plant != 'Stoke St George') %>%
  group_by(company, treatment_plant, Latitude, Longitude, group, determinand)%>%
  summarise(median = median(value))  # Sum across all time

# --------- Pivot table

df %>% group_by(group, determinand) %>% summarise(total_readings = n())

df %>% group_by(sample_location) %>% summarise(total_readings = n())

df %>% group_by(treatment_plant, Date) %>% summarise(n = n()) %>%
  group_by(treatment_plant) %>% summarise(n_samples = sum(n), n_dates_sampled = n(), min_year = min(lubridate::year(Date)), max_year = max(lubridate::year(Date)))

df %>% 
  group_by(treatment_plant, Date) %>%
  summarise(n = n()) %>%
  filter((lubridate::year(Date) =< 2020) && (lubridate::year(Date) => 2015)) #%>%
  #ggplot(aes(x = lubridate::year(Date), y = treatment_plant, fill = n)) +
 # geom_tile() +
 # ylab("")

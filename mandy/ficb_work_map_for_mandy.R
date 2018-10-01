# Packages ----------------------------------------------------------------

library(tidyverse)
library(googlesheets)
library(maps)
library(readxl)
library(janitor)
library(ggalt)
library(ggrepel)
library(extrafont)
library(ggimage)


# Theme -------------------------------------------------------------------

# tfff.dark.green <- "#265142"
tfff.dark.green <- "#4D7064" # Changed to lighter shade
tfff.light.green <- "#B5CC8E"
tfff.orange <- "#e65100"
tfff.yellow <- "#FBC02D"
tfff.blue <- "#283593"
tfff.red <- "#B71C1C"
tfff.brown <- "#51261C"
tfff.dark.gray <- "#545454"
tfff.medium.gray <- "#a8a8a8"
tfff.light.gray <- "#eeeeee"

font_import(pattern = "Calibri")


# Basemap function -----------------------------------------------



tfff_counties_map <- map_data("county") %>%
     filter(region == "oregon" | subregion == "siskiyou") %>%
     mutate(subregion = case_when(
          subregion == "hood river" ~ "hood\nriver",
          TRUE ~ subregion
     ))

dk_tfff_counties_basemap <- function() {
     
     
     
     ggplot() +
          geom_polygon(data = tfff_counties_map, 
                       aes(x = long, 
                           y = lat, 
                           group = group),
                       fill = tfff.light.green,
                       color = "white") +
          coord_map() +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          theme_void()
}


# Add county names --------------------------------------------------------


county.names.lon <- tfff_counties_map %>%
     group_by(subregion) %>%
     summarize(avg = mean(range(long)))

county.names.lat <- tfff_counties_map %>%
     group_by(subregion) %>%
     summarize(avg = mean(range(lat)))

county.names <- left_join(county.names.lat, county.names.lon, by = "subregion") %>%
     set_names(c('county', 'lat', 'lon')) %>%
     mutate(county = str_to_title(county)) %>%
     mutate(lat = case_when (
          county == 'Washington' ~ lat + .08,
          county == 'Wasco' ~ lat - .1,
          county == 'Umatilla' ~ lat + .1,
          county == 'Union' ~ lat - .1,
          county == 'Gilliam' ~ lat - .15,
          county == 'Lake' ~ lat + .2,
          county == 'Multnomah' ~ lat - .035,
          county == 'Sherman' ~ lat + .145,
          county == 'Hood\nRiver' ~ lat + .08,
          TRUE ~ lat)
     ) %>%
     mutate(lon = case_when (
          county == 'Deschutes' ~ lon - .45,
          county == 'Klamath' ~ lon - .2,
          county == 'Gilliam' ~ lon - .05,
          county == 'Polk' ~ lon - .05,
          county == 'Lake' ~ lon - .2,
          county == 'Curry' ~ lon - .1,
          county == 'Wasco' ~ lon - .15,
          county == 'Marion' ~ lon - .35,
          county == 'Tillamook' ~ lon - .15,
          county == 'Jefferson' ~ lon - .15,
          county == 'Wheeler' ~ lon + .05,
          county == 'Josephine' ~ lon + .1,
          county == 'Benton' ~ lon + .02,
          county == 'Yamhill' ~ lon + .06,
          county == 'Lane' ~ lon + .15,
          county == 'Hood\nRiver' ~ lon + .025,
          # county == 'Douglas' ~ lon + .1,
          TRUE ~ lon)
     )


dk_add_county_names <- function() {
     geom_text(data = county.names,
               aes(x = lon, 
                   y = lat, 
                   label = county), 
               size=2.9,
               lineheight = .75,
               family = "Calibri",
               color = tfff.dark.gray) 
     
}


# Get data ----------------------------------------------------------------

# gs_title("FICB map data") %>%
#      gs_download(to = "data/ficb_map_data.xlsx",
#                  overwrite = TRUE)

work_data <- read_excel("data/ficb_map_data.xlsx",
                        sheet = "Communities") %>%
     clean_names() %>%
     mutate(county = str_to_lower(county)) %>%
     mutate(type_of_work = str_to_title(type_of_work)) %>%
     mutate(region = str_replace(region, "Jeffersion", "Jefferson")) %>%
     mutate(maturity_of_relationship = factor(maturity_of_relationship, levels = c("Level One: Developing",
                                                                                   "Level Two: Defined",
                                                                                   "Level Three: Deepening"))) %>%
     arrange(maturity_of_relationship, region, -latitude) %>%
     mutate(n = row_number()) %>%
     mutate(label = paste(n,
                          ") ", 
                          community,
                          sep = ""))


# Save work_data to label in Word doc

write.csv(work_data, "data/work_data.csv")     

# TFFF offices


tfff_offices <- read_excel("data/ficb_map_data.xlsx",
                           sheet = "TFFF offices") %>% 
     mutate(image = "images/tfff-icon-white-border.png")

# Field coordinators

field_coordinators <- read_excel("data/ficb_map_data.xlsx",
                                 sheet = "Field coordinators") %>% 
     clean_names() 


field_coordinators_hometowns <- field_coordinators %>%
     select(name:lon, -c(full_address, hometown_city, hometown_state)) %>% 
     arrange(name) %>% 
     mutate(n = row_number()) %>%
     filter(name != "Max Gimbel" & name != "Roque Barros")

field_coordinators_counties <- field_coordinators %>%
     select(name, baker:siskiyou) %>%
     gather(key = "county", value = "work", -name) %>%
     filter(!is.na(work))

# Regions

region <- c("Siuslaw",
            "Applegate Valley",
            "Illinois Valley",
            "McKenzie River",
            "Crook/Jefferson",
            "Rural Klamath")

lat <- c(43.8,
         42.45,
         42.25,
         43.95,
         44.7,
         42.45)


lon <- c(-123.75,
         -122.75,
         -124.1,
         -122.25,
         -120.7,
         -121.4)


regions <- tibble(region, lat, lon) %>%
     mutate(region = str_wrap(region, 10))





# Work map ----------------------------------------------------------------

dk_tfff_counties_basemap() +
     geom_point(data = field_coordinators_hometowns,
                aes(x = lon,
                    y = lat),
                fill = "transparent",
                color = "black",
                shape = 21,
                stroke = 1,
                size = 4) +
     geom_text(data = field_coordinators_hometowns,
               aes(x = lon,
                   y = lat,
                   label = n),
               # nudge_x = .15,
               family = "Calibri",
               fontface = "bold",
               size = 3,
               color = "black") +
     geom_image(data = tfff_offices, 
                aes(x = lon,
                    y = lat,
                    image = image),
                size = .02) +
     # geom_text(data = tfff_offices, 
     #           aes(x = lon,
     #               y = lat,
     #               label = office),
     #           color = tfff.dark.green,
     #           alpha = .75,
     #           size = 2.5,
     #           nudge_x = case_when(
     #                tfff_offices$office == "Eugene" ~ .28,
     #                tfff_offices$office == "Roseburg" ~ .32
     #           )) +
     dk_add_county_names()
   




ggsave("FICB-work-map-v2.png",
       height = 9,
       width = 9,
       dpi = 600)







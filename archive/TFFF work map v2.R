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

tfff.dark.green <- "#265142"
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

# Get data ----------------------------------------------------------------

# gs_title("FICB map data") %>%
#      gs_download(to = "data/ficb_map_data.xlsx",
#                  overwrite = TRUE)

work_data <- read_excel("data/ficb_map_data.xlsx",
                        sheet = "Communities") %>%
     clean_names() %>%
     mutate(county = str_to_lower(county)) %>%
     mutate(type_of_work = str_to_title(type_of_work)) %>%
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


tfff_offices <- read_excel("data/ficb_map_data.xlsx",
                           sheet = "TFFF offices") %>% 
     mutate(type = "TFFF office") 

field_coordinators <- read_excel("data/ficb_map_data.xlsx",
                                 sheet = "Field coordinators") %>% 
     clean_names() 




# Regions -----------------------------------------------------------------

region <- c("Siuslaw",
            "Applegate Valley",
            "Illinois Valley",
            "McKenzie River",
            "Crook/Jefferson",
            "Rural Klamath")

lat <- c(44.32,
         42.45,
         42.35,
         44.4,
         44.7,
         42.3)


lon <- c(-124.014892,
         -122.6,
         -124.1,
         -122.25,
         -120.7,
         -120.7)


regions <- tibble(region, lat, lon) %>%
     mutate(region = str_wrap(region, 10))




# Work map ----------------------------------------------------------------



dk_tfff_counties_basemap() +
     geom_text(data = regions,
               aes(x = lon, 
                   y = lat,
                   label = region,
                   lineheight =.75,
                   fontface = "bold",
                   family = "Calibri"),
               color = tfff.brown,
               alpha = 0.4) +
     geom_encircle(data = work_data, 
                   size = 2,
                   aes(x = longitude,
                       y = latitude),
                   group = work_data$region,
                   expand = .05,
                   linetype = "dotted",
                   alpha = 0.4,
                   color = tfff.brown) +
     geom_image(data = tfff_offices, 
                aes(x = lon,
                    y = lat,
                    image = image),
                size = .02) +
     geom_text(data = tfff_offices, 
                     aes(x = lon,
                         y = lat,
                         label = office),
                     color = tfff.light.green,
                     nudge_x = case_when(
                          tfff_offices$office == "Eugene" ~ .36,
                          tfff_offices$office == "Roseburg" ~ .4
                     )) +
     geom_text_repel(data = work_data,
                     size = 3,
                     aes(x = longitude,
                         y = latitude,
                         label = n),
                     force = .75,
                     box.padding = .3,
                     fontface = "bold",
                     family = "Calibri",
                     color = case_when(
                          work_data$maturity_of_relationship == "Level One: Developing" ~ tfff.blue,
                          work_data$maturity_of_relationship == "Level Two: Defined" ~ tfff.orange,
                          work_data$maturity_of_relationship == "Level Three: Deepening" ~ tfff.dark.green
                     )) +
     geom_point(data = work_data, 
                aes(x = longitude,
                    y = latitude),
          color = case_when(
               work_data$maturity_of_relationship == "Level One: Developing" ~ tfff.blue,
               work_data$maturity_of_relationship == "Level Two: Defined" ~ tfff.orange,
               work_data$maturity_of_relationship == "Level Three: Deepening" ~ tfff.dark.green),
          size = case_when(
               work_data$maturity_of_relationship == "Level One: Developing" ~ 1,
               work_data$maturity_of_relationship == "Level Two: Defined" ~ 2.5,
               work_data$maturity_of_relationship == "Level Three: Deepening" ~ 4),
          shape = case_when(
               work_data$maturity_of_relationship == "Level One: Developing" ~ 21,
               work_data$maturity_of_relationship == "Level Two: Defined" ~ 22,
               work_data$maturity_of_relationship == "Level Three: Deepening" ~ 23),
          fill = case_when(
               work_data$maturity_of_relationship == "Level One: Developing" ~ tfff.blue,
               work_data$maturity_of_relationship == "Level Two: Defined" ~ tfff.orange,
               work_data$maturity_of_relationship == "Level Three: Deepening" ~ tfff.dark.green),
          shape = 21,
          alpha = 0.8) 



ggsave("plots/FICB-work-map.png",
       height = 9,
       width = 9,
       dpi = 600)


# Save as SVG in order to grab points for legend

ggsave("plots/FICB-work-map.pdf")
       

# Counties where FICB works now


# Field coordinators ------------------------------------------------------

field_coordinators_hometowns <- field_coordinators %>%
     select(name:lon, -c(full_address, hometown_city, hometown_state)) %>% 
     arrange(name) %>% 
     mutate(n = row_number()) 

field_coordinators_counties <- field_coordinators %>%
     select(name, baker:siskiyou) %>%
     gather(key = "county", value = "work", -name) %>%
     filter(!is.na(work))




# County map --------------------------------------------------------------



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
                       fill = tfff.light.gray,
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


# Field coordinators by county --------------------------------------------

field_coordinators_county_fills <- field_coordinators_counties %>% 
     left_join(tfff_counties_map, by = c("county" = "subregion"))


dk_tfff_counties_basemap() +
     geom_polygon(data = field_coordinators_county_fills, aes(x = long, y = lat, 
                                                              group = group),
                  fill = tfff.light.green,
                  color = "white") +
     geom_point(data = field_coordinators_hometowns,
                aes(x = lon,
                    y = lat),
                color = tfff.dark.green) +
     facet_wrap(~name, ncol = 2) +
     scale_x_continuous(expand = c(.1, .1)) +
     scale_y_continuous(expand = c(.1, .1)) +
     theme(strip.text = element_text(family = "Calibri"))


ggsave("plots/field-coordinators-by-county.png",
       height = 10,
       width = 7.5,
       dpi = 600)


# Counties where work happens vs counties where it doesn't ----------------

counties_where_work <- work_data %>%
     select(county) %>% 
     distinct(county, .keep_all = T) %>%
     left_join(tfff_counties_map, by = c("county" = "subregion"))

dk_tfff_counties_basemap() +
     geom_polygon(data = counties_where_work, 
                  aes(x = long, y = lat, 
                      group = group),
                  fill = tfff.light.green,
                  color = "white") +
     geom_point(data = field_coordinators_hometowns,
                aes(x = lon,
                    y = lat),
                color = tfff.dark.green) +
     geom_text_repel(data = field_coordinators_hometowns,
                     aes(x = lon,
                         y = lat,
                         label = n,
                         family = "Calibri"),
                     color = tfff.dark.green,
                     # point.padding = 0,
                     # box.padding = .5,
                     segment.alpha = 0,
                     direction = "x") +
     dk_add_county_names()



ggsave("plots/counties-where-work.png",
       height = 7,
       width = 7,
       dpi = 600)

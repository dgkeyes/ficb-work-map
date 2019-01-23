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




# Counties where work happens vs counties where it doesn't ----------------

counties_where_work <- work_data %>%
     select(county) %>% 
     distinct(county, .keep_all = T) %>%
     # Add \n in hood river so it matches for join
     mutate(county = str_replace(county, "hood river", "hood\nriver")) %>% 
     left_join(tfff_counties_map, by = c("county" = "subregion"))

# Work map ----------------------------------------------------------------

dk_tfff_counties_basemap() +
     geom_polygon(data = counties_where_work, 
                  aes(x = long, y = lat, 
                      group = group),
                  fill = tfff.light.green,
                  color = "white") +
     geom_text(data = regions,
               aes(x = lon, 
                   y = lat,
                   label = region,
                   lineheight =.75,
                   fontface = "bold",
                   family = "Calibri"),
               color = tfff.brown,
               alpha = 0.5) +
     geom_encircle(data = work_data, 
                   size = 2,
                   aes(x = longitude,
                       y = latitude),
                   group = work_data$region,
                   expand = .035,
                   linetype = "dotted",
                   alpha = 0.5,
                   color = tfff.brown) +
     geom_point(data = field_coordinators_hometowns,
                aes(x = lon,
                    y = lat),
                fill = "transparent",
                color = "black",
                shape = 21,
                stroke = 1,
                size = 3) +
     geom_text(data = field_coordinators_hometowns,
               aes(x = lon,
                   y = lat,
                   label = n),
               nudge_x = .15,
               family = "Calibri",
               fontface = "bold",
               size = 3,
               color = "black") +
     geom_image(data = tfff_offices, 
                aes(x = lon,
                    y = lat,
                    image = image),
                size = .02) +
     geom_text(data = tfff_offices, 
               aes(x = lon,
                   y = lat,
                   label = office),
               color = tfff.dark.green,
               alpha = .75,
               size = 2.5,
               nudge_x = case_when(
                    tfff_offices$office == "Eugene" ~ .26,
                    tfff_offices$office == "Roseburg" ~ .3
               )) +
     geom_text_repel(data = work_data,
                     size = 3,
                     aes(x = longitude,
                         y = latitude,
                         label = n),
                     force = .8,
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
                     work_data$maturity_of_relationship == "Level Two: Defined" ~ 2,
                     work_data$maturity_of_relationship == "Level Three: Deepening" ~ 3),
                shape = case_when(
                     work_data$maturity_of_relationship == "Level One: Developing" ~ 22,
                     work_data$maturity_of_relationship == "Level Two: Defined" ~ 23,
                     work_data$maturity_of_relationship == "Level Three: Deepening" ~ 24),
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


# Save as PDF in order to grab points for legend

ggsave("plots/FICB-work-map.pdf")
ggsave("plots/FICB-work-map.svg",
       height = 9,
       width = 9)


# Counties where FICB works now







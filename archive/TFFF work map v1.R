# Packages ----------------------------------------------------------------

library(tidyverse)
library(googlesheets)
library(maps)
library(readxl)
library(janitor)
library(ggalt)
library(ggrepel)
library(extrafont)


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
     arrange(maturity_of_relationship, longitude, -latitude) %>%
     mutate(n = row_number()) %>%
     mutate(latino_leadership = if_else(str_detect(type_of_work, "Latino Leadership"),
                                        "Y",
                                        "N")) %>% 
     mutate(community_networking = if_else(str_detect(type_of_work, "Community Networking"),
                                        "Y",
                                        "N")) %>%
     mutate(visioning = if_else(str_detect(type_of_work, "Visioning"),
                                           "Y",
                                           "N")) %>% 
     mutate(mobilizing_regional = if_else(str_detect(type_of_work, "Mobilizing (Regional)"),
                                           "Y",
                                           "N")) %>% 
     mutate(mobilizing_local = if_else(str_detect(type_of_work, "Mobilizing (Local)"),
                                           "Y",
                                           "N")) %>% 
     mutate(project_development = if_else(str_detect(type_of_work, "Project Development"),
                                           "Y",
                                           "N")) %>% 
     mutate(listening = if_else(str_detect(type_of_work, "Listening"),
                                           "Y",
                                           "N"))


write.csv(work_data, "work_data.csv")     



# Regions -----------------------------------------------------------------

region <- c("Siuslaw",
            "Applegate Valley",
            "Illinois Valley",
            "McKenzie River",
            "Crook/Jefferson",
            "Rural Klamath",
            "Klamath River")

lat <- c(44.325,
         42.45,
         42.35,
         44.45,
         44.7,
         42.3,
         41.3)


lon <- c(-124.014892,
         -122.7,
         -124.1,
         -122.3,
         -120.55,
         -120.85,
         -121.8)


regions <- tibble(region, lat, lon) %>%
     mutate(region = str_wrap(region, 10))

# Make main map ----------------------------------------------------------------


bg_map <- map_data("county", c("oregon", "california")) %>%
     filter(region == "oregon" | subregion == "siskiyou")

# Map notes
# Community = encirlced
# Depth of work = shape
# Type of work = color

ggplot(data = work_data, 
        aes(x = longitude,
            y = latitude)) +
     geom_polygon(data = bg_map, 
                  aes(x = long, 
                      y = lat, 
                      group = group),
                  fill = tfff.light.gray,
                  color = tfff.light.gray,
                  alpha = 0.5) +
     geom_encircle(data = work_data, 
                   size = 30,
                   aes(x = longitude,
                       y = latitude),
                   group = work_data$region,
                   expand = 0,
                   alpha = 0.5,
                   color = tfff.brown) +
     geom_text(data = regions,
               aes(x = lon, 
                   y = lat,
                   label = region,
                   lineheight =.75,
                   fontface = "bold",
                   family = "Calibri"),
               color = tfff.brown,
               alpha = 0.5) +
     geom_text_repel(data = work_data, 
               size = 3,
                aes(x = longitude,
                    y = latitude,
                    label = n),
               force = .01,
               box.padding = .01,
               fontface = "bold",
               family = "Calibri",
                color = case_when(
                     work_data$maturity_of_relationship == "Level One: Developing" ~ tfff.blue,
                     work_data$maturity_of_relationship == "Level Two: Defined" ~ tfff.orange,
                     work_data$maturity_of_relationship == "Level Three: Deepening" ~ tfff.dark.green
                )) +
     coord_map() +
     theme_void() +
     scale_x_continuous(expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0)) 

ggsave("FICB-map.png",
       height = 7,
       width = 10)


# ggsave("FICB-map.svg",
#        height = 10,
#        width = 16)

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
     mutate(n = row_number())


tfff_offices <- read_excel("data/ficb_map_data.xlsx",
                           sheet = "TFFF offices") %>% 
     mutate(type = "TFFF office") %>%
     select(office, lat, lon, type, region) %>%
     set_names(c("name", "lat", "lon", "type")) %>% 
     mutate(region = NA)

field_coordinators_hometowns <- read_excel("data/ficb_map_data.xlsx",
                                           sheet = "Field coordinators") %>% 
     # Make main map v2 ----------------------------------------------------------------


bg_map <- map_data("county", c("oregon", "california")) %>%
     filter(region == "oregon" | subregion == "siskiyou")

# ggplot() +
#      geom_polygon(data = bg_map, 
#                   aes(x = long, 
#                       y = lat, 
#                       group = group),
#                   fill = tfff.light.gray,
#                   color = tfff.light.gray,
#                   alpha = 0.5) +
dk_oregon_basemap() +     
     geom_point(data = work_data, 
                aes(x = lon,
                    y = lat),
                shape = 21,
                alpha = 0.8) +
     coord_map() +
     theme_void() +
     scale_x_continuous(expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0)) +
     scale_color_manual(values = c(tfff.blue, tfff.orange, tfff.dark.green)) 


# TK ----------------------------------------------------------------------



clean_names() %>% 
     select(name:lon, -c(full_address, hometown_city, hometown_state)) %>%
     mutate(type = "field coordinator") %>% 
     mutate(region = NA)

all_points <- work_data %>%
     select(community:longitude, region, -full_address, -state) %>%
     set_names(c("name", "lat", "lon", "region")) %>%
     mutate(type = "community") %>%
     bind_rows(field_coordinators_hometowns) %>% 
     bind_rows(tfff_offices)




# Messing with map --------------------------------------------------------

ggplot(data = work_data, 
       aes(x = longitude,
           y = latitude)) +
     geom_polygon(data = bg_map, 
                  aes(x = long, 
                      y = lat, 
                      group = group),
                  fill = tfff.light.gray,
                  color = "white") +
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
                   expand = .035,
                   linetype = "dotted",
                   alpha = 0.4,
                   color = tfff.brown) +
     geom_image(data = tfff_offices, 
                aes(x = lon,
                    y = lat,
                    image = image),
                size = .02) +
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
     geom_point(
          color = case_when(
               work_data$maturity_of_relationship == "Level One: Developing" ~ tfff.blue,
               work_data$maturity_of_relationship == "Level Two: Defined" ~ tfff.orange,
               work_data$maturity_of_relationship == "Level Three: Deepening" ~ tfff.dark.green),
          size = case_when(
               work_data$maturity_of_relationship == "Level One: Developing" ~ 1,
               work_data$maturity_of_relationship == "Level Two: Defined" ~ 2.5,
               work_data$maturity_of_relationship == "Level Three: Deepening" ~ 4),
          fill = case_when(
               work_data$maturity_of_relationship == "Level One: Developing" ~ tfff.blue,
               work_data$maturity_of_relationship == "Level Two: Defined" ~ tfff.orange,
               work_data$maturity_of_relationship == "Level Three: Deepening" ~ tfff.dark.green),
          shape = 21,
          alpha = 0.8) +
     coord_map() +
     theme_void() +
     scale_x_continuous(expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0)) +
     scale_color_manual(values = c(tfff.blue, tfff.orange, tfff.dark.green)) 

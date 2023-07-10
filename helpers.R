library(tidyverse)
library(janitor)
library(sf)
library(patchwork)
library(rcartocolor)
library(tmap)
library(shiny)
library(bslib)
library(rsconnect)


names_raw <- read_delim("resources/ts-x-01.04.00.15.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(), 
                        trim_ws = TRUE) %>%
  clean_names()

data <- names_raw %>%
  mutate(pct_gde = as.numeric(pct_gde))

list_names <- data %>%
  select(lastname) %>%
  unique() %>%
  pull()



map <- st_read("resources/2023_GEOM_TK/06_POKU/Gesamtfläche_gf/K4_voge20230101_gf/K4voge20230101gf_ch2007Poly.shp") %>%
  clean_names()

map_name <- function(data, name){
  selected_name <- data %>%
    filter(lastname == name)
  
  join_map <- map %>%
    left_join(selected_name, by = c("id" ="gdenr"))
  
  join_map %>%
    ggplot() +
    geom_sf(aes(fill = value),color="grey95") +
    scale_fill_carto_c(name = "Occurence par commune",
                       palette = "Sunset", na.value = "grey95") +
    labs(title = paste0("<b> Nombre de </b>", name, "<b> par commune </b>")) +
    theme_void() +
    theme(plot.title = ggtext::element_markdown(hjust=0.5),
          legend.position = "bottom")
  
  
}

top_10 <- function(data,name) {
  
  data %>%
    filter(lastname == name) %>%
    arrange(desc(value)) %>%
    head(10) %>%
    ggplot() +
    geom_col(aes(fct_rev(fct_infreq(gdename,value)),value), fill="#ce6693") +
    coord_flip() +
    labs(#title = paste0("Nombre de ", name, " par commune"),
         x = "",
         y = "Occurence par commune") +
    theme_minimal()
  
}
  
  

#map_val + top_10 +
 # plot_layout(design = layout)

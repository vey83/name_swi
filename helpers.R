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



layout <- c(
  area(t = 1, l = 1, b = 8, r = 8),
  area(t = 8, l = 8, b = 9, r = 9))

map_name <- function(data, name){
  selected_name <- data %>%
    filter(lastname == name)
  
  join_map <- map %>%
    left_join(selected_name, by = c("id" ="gdenr"))
  
  
  quantile_vec <- join_map %>%
    pull(pct_gde) %>%
    quantile(probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  join_map <- join_map %>%
    mutate(quantiles = cut(pct_gde, breaks = quantile_vec, include.lowest = TRUE))
  
  labels <- tibble(
    lab1 = quantile_vec,
    lab2 = c(quantile_vec[2:length(quantile_vec)], NA)
  ) %>%
    slice(1:n() - 1) %>% 
    mutate(labs = paste(lab1, lab2, sep = " -"))
  
  join_map <- join_map %>%
    mutate(quantiles = cut(pct_gde,
                           breaks = quantile_vec,
                           labels = labels$labs,
                           include.lowest = TRUE
    ))
  
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

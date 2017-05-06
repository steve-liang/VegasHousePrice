library(ggplot2)
library(dplyr)
library(ggmap)
library(plotly)


data <- readr::read_csv("housing.csv")
lv_map <- get_map("Las Vegas", maptype = "roadmap", source = "google", zoom = 10)

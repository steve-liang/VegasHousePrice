library(ggmap)
library(Imap)
str(mydata)

# id seems to be irrelevant
mydata <- select(mydata, -id)

# convert chr to factor for modeling purposes
mydata <- chr.to.factor(mydata)

d <- mydata

# We need zip info to answer some questions, so a zip field is necessary
d <- d %>% rowwise() %>% mutate(zip = as.factor(tail(unlist(str_split(address, " ")),1)))

# Price in focus
summary(d$price)

dist.to.strip <- function(geo){
  STRIP <- "-115.172816,36.114646"
  lont1 <- as.numeric(str_split(geo, ",")[[1]][1])
  lati1 <- as.numeric(str_split(geo, ",")[[1]][2])
  strip.lon <- as.numeric(str_split(STRIP, ",")[[1]][1])
  strip.lat <- as.numeric(str_split(STRIP, ",")[[1]][2])
  
  gdist(lont1, lati1, strip.lon, strip.lat, units = "miles")
}

# New features (distance to strip, price per sqft, split GEOCODE to lon/lat)
d <- d %>% rowwise() %>% mutate(distance = dist.to.strip(GEOCODE))
d <- d %>% mutate(pps = price / size_sqft)
d <- d %>% mutate(lon = as.numeric(str_split(GEOCODE, ",")[[1]][1]), 
                  lat = as.numeric(str_split(GEOCODE, ",")[[1]][2]))


# File is ready, save as a csv for Shiny
readr::write_csv(d, "housing.csv")

# zip with high price sqft

ggplot(d) + geom_boxplot(aes(x = reorder(zip, -pps), y = pps)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1))


lv_map <- get_map("Las Vegas", maptype = "terrain", source = "google", zoom = 12)
ggmap(lv_map) %+% d + 
       aes(x = lon,
           y = lat,
           z = pps) + 
  stat_summary2d(fun = median, binwidth = c(.05, .05)) + 
  scale_colour_gradient(low = "green", high = "red") +
  coord_map()

# Top 10 zip

top10 <- d %>% group_by(zip) %>% summarise(med = median(price)) %>% arrange(desc(med)) %>% top_n(10)
ggplot(top10) + geom_bar(aes(reorder(zip, -med), med, fill = as.factor(zip)), stat = "identity") +
  labs(title = "Top 10 Zip Code By Median Home Price", x = "Zip Code", y = "Median Price") + 
  scale_y_continuous(labels = scales::dollar, breaks = seq(0,1000000,50000)) + 
  theme(legend.position="none")



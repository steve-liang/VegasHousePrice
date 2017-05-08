library(ggmap)
library(Imap)
str(mydata)

options("scipen"=100, "digits"=4)

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
  
  Imap::gdist(lont1, lati1, strip.lon, strip.lat, units = "miles")
}

# New features (distance to strip, price per sqft, split GEOCODE to lon/lat)
d <- d %>% rowwise() %>% mutate(distance = dist.to.strip(GEOCODE))
d <- d %>% mutate(pps = price / size_sqft)
d <- d %>% mutate(lon = as.numeric(str_split(GEOCODE, ",")[[1]][1]), 
                  lat = as.numeric(str_split(GEOCODE, ",")[[1]][2]))


# File is ready, save as a csv for Shiny
readr::write_csv(d, "housing.csv")

# Clear outliers
d <- d[d$pps < median(d$pps) + 20*sd(d$pps),]
d <- d[d$lot_size < median(na.omit(d$lot_size)) + 10*sd(na.omit(d$lot_size))  | is.nan(d$lot_size),]

# add bathrooms field 
d <- d %>% mutate(bathrooms = full_bathrooms + 0.5 * half_bathrooms)


features <- select(d, bedrooms, bathrooms, size_sqft, lot_size, type, distance, pps)

# Need to impute lot_size and distance
colSums(is.na(features))

# use rpart to predict lot_size and impute missing values
predictLot<- rpart(lot_size~bedrooms+bathrooms+type+pps+size_sqft,data=features[!is.na(features$lot_size),],method="anova")
rpart.plot(predictLot)
features$lot_size[is.na(features$lot_size)] <- predict(predictLot,data= features[is.na(features$lot_size)])

# simply use mean to impute distance
features$distance[is.na(features$distance)] <- mean(na.omit(features$distance))

# standardization and create bath/bedroom ratio
features <- features %>% mutate(bb.ratio = bathrooms/bedrooms, bedrooms = bedrooms/size_sqft, bathrooms = bathrooms/size_sqft)

features$type = as.factor(features$type)
###########################################
#
# Split data for Cross Validation
############################################

## 80% of the sample size
smp_size <- floor(0.8 * nrow(features))

## set the seed to make your partition reproductible
set.seed(123)
train_index <- sample(seq_len(nrow(features)), size = smp_size)

train_set <- features[train_index, ]
test_set <- features[-train_index, ]


############################################

library(randomForest)

# Use 10-fold CV to tune mtry
control <- trainControl(method="cv", number=10, search="grid")
tunegrid <- expand.grid(.mtry=c(1:5))
rf_gridsearch <- train(pps~., data = train_set, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


# Build a RF Model with mtry = 5
rf <-  randomForest(data = train_set, pps~., 
                    ntree = 1000, mtry = 3, 
                    importance = T, keep.forest = T, 
                    na.action = na.omit)

# RMSE 66.36

varImpPlot(rf, type = 2)

# Validate with new data set
pred.rf <- predict(rf, test_set, predict.all=TRUE)
cat("R2:" , 1 - (sum((test_set$pps-pred.rf$aggregate)^2)/sum((test_set$pps-mean(test_set$pps))^2)))
cat("RMSE: ", sqrt(mean((test_set$pps-pred.rf$aggregate)^2))) 


# Use simple decision tree for interpretable modeling
treeModel<- rpart(pps~bedrooms+distance+size_sqft,data=features, method = "anova")
rpart.plot(treeModel)


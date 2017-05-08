source('~/GitHub/dsPack.R')
library(stringr)

# read in data files
data <- readr::read_csv('realtor_extract.csv')
geocode <- readr::read_csv('census_geocoder_results.csv')
dim(data)
str(data)

# convert all to upper characters for easy comparison
geocode <- geocode %>% mutate(ADDRESS = str_to_upper(ADDRESS))

# NA Check
NA.percentage(geocode, 0)

# No NA Found in ADDRESS, MATCH_IND, but 13% in other columns

# But does it mean the ADDRESS(es) are clean? e.g unique
length(unique(geocode$ADDRESS)) == length(geocode$ADDRESS)

# There are duplicates
dup_addr <- geocode %>% group_by(ADDRESS) %>% mutate(n=n()) %>% arrange(desc(n))

# Select only distinct row by ADDRESS
geocode <- geocode %>% distinct(ADDRESS, .keep_all= T)

# Reverify uniqueness of ADDRESS now
length(unique(geocode$ADDRESS)) == length(geocode$ADDRESS)  # TRUE

# Other than NAs, is GEOCODE unique too?
length(unique(na.omit(geocode$GEOCODE))) == length(na.omit(geocode$GEOCODE)) # FALSE

dup_geocode <- na.omit(geocode) %>% group_by(GEOCODE) %>% mutate(n = n()) %>% arrange(desc(n))

# Appears that multi-unit (apartment) buildings share same address and geocode, fair enough, and should be kept as-is
# Thus far, ADDRESSes are unique, GEOCODE can have duplicates and contains NA, both are fine. 
# Lastly, a safety check is verify if MATCH_ADDRESS and GEOCODE have the same length, so that they are 1:1

# Unique now
length(unique(geocode$MATCH_ADDRESS)) == length(unique(geocode$GEOCODE)) # FALSE 3824:3825

# could that be NA?
length(unique(na.omit(geocode$MATCH_ADDRESS))) == length(unique(na.omit(geocode$GEOCODE))) # FALSE

# more investigation
p <- select(geocode, MATCH_ADDRESS, GEOCODE)
a <- na.omit(p) %>% group_by(MATCH_ADDRESS) %>% arrange(MATCH_ADDRESS, GEOCODE) %>% mutate(n=n())
b <- na.omit(p) %>% group_by(GEOCODE) %>% arrange(MATCH_ADDRESS, GEOCODE) %>% mutate(n=n()) 
a[which(a$n != b$n),]
b[which(a$n != b$n),]

# 3722 LAS VEGAS BLVD S, LAS VEGAS, NV, 89158 have 2 GEOCODE, somthing is WRONG
a[1629,]

# -115.173,36.107533 OR -115.173,36.107567
# Lookup on https://geocoding.geo.census.gov/geocoder/geographies/address?street=3722+Las+Vegas+Blvd&city=Las+Vegas&state=NV&zip=89158&benchmark=4&vintage=4
# Should be -115.173,36.107567
geocode[which(geocode$MATCH_ADDRESS == "3722 LAS VEGAS BLVD S, LAS VEGAS, NV, 89158"),]$GEOCODE <- "-115.173,36.107567"

# Check uniqueness again 
length(unique(geocode$MATCH_ADDRESS)) == length(unique(geocode$GEOCODE))

# Now I believe the Geocode file is clean. 
# We have unique ADDRESS, no NA.
# MATCH_ADDRESS is not unique, but align with GEOCODE with 1:1 relationship.
# NA still exist but it's reasonable. I don't want to remove them simply because you can't find the geocode. 
# Note there are some less informative ADDRESS, such as , LAS VEGAS, you can find an example at row 69. 

################################################
#
# realtor file investigation
#
################################################

# Similarly I'll conver to upper case for address, so matching is easier
data <- data %>% mutate(address = str_to_upper(address))

# Check NA for all columns, only found lot_size has 15% of NAs
NA.percentage(data,0)

# Check uniqueness of address
length(unique(data$address)) == length(data$address)

# Also contains duplicates
dup_address <- data %>% group_by(address) %>% arrange(address) %>% mutate(n=n()) %>% arrange(desc(n)) %>% filter(n > 1)

# 237 obs with duplicated addresses, since they have different descriptions, they are likely real data but only with unspecified address
# I do not want to remove them, but to use Averaging/Taking Mode for (factors) to aggregate them into one single unit as representation of the entire group.

my_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


data <- data %>% 
  group_by(address) %>% 
  arrange(address) %>% 
  mutate(bedrooms = round(mean(bedrooms)), 
         full_bathrooms = round(mean(full_bathrooms)), 
         half_bathrooms = round(mean(half_bathrooms)), 
         type = my_mode(type), 
         size_sqft = mean(size_sqft), 
         lot_size = mean(na.omit(lot_size)), 
         price = mean(price)) %>% 
  distinct(address, .keep_all = TRUE) %>%
  arrange(id) %>%
  ungroup()

# Glad to see that the revised realtor data and geocode data both having exactly the same number of roles. 
# This might be an indication that my cleaning work is accurate.

# re-run uniqueness check, 
length(unique(data$address)) == length(data$address)  # TRUE

# Now the final step is to merge two data files into one data frame so that we will only be working on 1 single data frame object
# I could use id to join the two tables, but not quite sure if they are consistent with each other
# Safetest to join by the address/ADDRESS field.

# But first need to replace the comma in (geocode file) with blank to match the addres to ADDRESS
geocode$ADDRESS <- str_replace_all(geocode$ADDRESS, ",", "")

# Then join by ADDRESS/address
mydata <- left_join(data, geocode, by = c("address" = "ADDRESS"))

# Finalize the data frame by removing irrelevant fields, such as ID, MATCH_IND, MATCH_TYPE, MATCH_ADDRESS
mydata <- select(mydata, -ID, -MATCH_IND, -MATCH_TYPE, -MATCH_ADDRESS)



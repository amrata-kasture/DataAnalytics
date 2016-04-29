## Dependency package - if you do not have dplyr installed, you will need to execute
## install.packages('dplyr') before running this line.
#dplyr is a powerful R-package to transform and summarize tabular data with rows and columns
#dplyr contains a set of functions (or “verbs”) that perform common data manipulation operations
library(dplyr)

## Set your working directory here: this should be the location where you saved SFO_AirBnB_listings.csv
setwd('/Users/amrata/Documents/Data Analytics/DA_Proj')

## read.csv function reads in only the first 1K rows of the csv file. 
## head=TRUE specifics that first line of CSV is label and we are replacing all empty spaces with NA
SFO_Data <- read.csv('SFO_AirBnB_listings.csv', na.strings=c(""," ","N/A","NA"),head=TRUE,stringsAsFactors=FALSE)

##As part of data cleaning we are considering 39 columns out of 92
##Out of 39, 23 column values will be considered for Popularity Index calculation-
##and rest will be used for displaying details of listing for user
SFO_Data <- select(SFO_Data, id, host_id, host_name, host_response_time, 
                   host_acceptance_rate, host_is_superhost, 
                   host_identity_verified, street,	city, state, zipcode, latitude, 
                   longitude, is_location_exact, property_type, room_type, 
                   accommodates, bathrooms	, bedrooms, beds, bed_type, 
                   amenities, price, security_deposit, cleaning_fee, 
                   guests_included, extra_people, has_availability, 
                   number_of_reviews, review_scores_rating, review_scores_accuracy,
                   review_scores_cleanliness, review_scores_checkin, 
                   review_scores_communication, review_scores_location, 
                   review_scores_value, requires_license, instant_bookable, 
                   cancellation_policy)

#In columns like security_deposit, cleaning_fee, guests_included, extra_people safely NA can be 0
SFO_Data$security_deposit[is.na(SFO_Data$security_deposit)] <- 0
SFO_Data$cleaning_fee[is.na(SFO_Data$cleaning_fee)] <- 0
SFO_Data$guests_included[is.na(SFO_Data$guests_included)] <- 0
SFO_Data$extra_people[is.na(SFO_Data$extra_people)] <- 0


#Remove rows having null/NA valuesand name the dataset for Indexing
SFO_Data <- na.omit(SFO_Data)
# Finding division values to split data into test and train with 80:20 rule
divData = sample(1:nrow(SFO_Data), size=0.2*nrow(SFO_Data))

# Split data
test = SFO_Data[divData,]
dim(test) 
train = SFO_Data[-divData,]
dim(train) 

#host response time
train$host_response_time[train$host_response_time == "within an hour"] <- 100
train$host_response_time[train$host_response_time == "within a few hours"] <- 80
train$host_response_time[train$host_response_time == "within a day"] <- 60
train$host_response_time[train$host_response_time == "a few days or more"] <- 40
train$host_response_time <-as.numeric(train$host_response_time)

#superhost scoring  conversions
train$host_is_superhost[train$host_is_superhost == "t"] <- as.numeric(100)
train$host_is_superhost[train$host_is_superhost == "f"] <- as.numeric(0)
train$host_is_superhost <-as.numeric(train$host_is_superhost)

#host identity scoring  conversions
train$host_identity_verified[train$host_identity_verified == "t"] <- as.numeric(100)
train$host_identity_verified[train$host_identity_verified == "f"] <- as.numeric(0)
train$host_identity_verified <- as.numeric(train$host_identity_verified)

#property type scoring conversions
train$property_type_score[train$property_type == "Bed & Breakfast"] <- 100
train$property_type_score[train$property_type == "Condominium"] <- 90
train$property_type_score[train$property_type == "Apartment"] <- 90
train$property_type_score[train$property_type == "Loft"] <- 80
train$property_type_score[train$property_type == "Townhouse"] <- 80
train$property_type_score[train$property_type == "Bungalow"] <- 70
train$property_type_score[train$property_type == "House"] <- 70
train$property_type_score[train$property_type == "Villa"] <- 70
train$property_type_score[train$property_type == "Cabin"] <- 60
train$property_type_score[train$property_type == "Boat"] <- 40
train$property_type_score[train$property_type == "Castle"] <- 40
train$property_type_score[train$property_type == "Camper/RV"] <- 40
train$property_type_score[train$property_type == "Dorm"] <- 40
train$property_type_score[train$property_type == "C"] <- 0
train$property_type_score[train$property_type == "Island"] <- 0
train$property_type_score[train$property_type == "N/A"] <- 0
train$property_type_score[train$property_type == "Plane"] <- 0
train$property_type_score[train$property_type == "Tent"] <- 0
train$property_type_score[train$property_type == "Treehouse"] <- 0
train$property_type_score[train$property_type == "Island"] <- 0
train$property_type_score[train$property_type == "Yurt"] <- 0
train$property_type_score[train$property_type == "Other"] <- 0
train$property_type_score <- as.numeric(train$property_type_score)

#islocation exact scoring conversions
train$is_location_exact[train$is_location_exact == "t"] <- as.numeric(100)
train$is_location_exact[train$is_location_exact == "f"] <- as.numeric(0)
train$is_location_exact <-as.numeric(train$is_location_exact)

#room type scoring conversion
train$room_type[train$room_type == "Entire home/apt"] <-100
train$room_type[train$room_type == "Private room"] <-90
train$room_type[train$room_type == "Shared room"] <-80
train$room_type <- as.numeric(train$room_type)

#accomomodates scoring conversion
train$accommodates_score[train$accommodates > 10] <-40
train$accommodates_score[train$accommodates == 4] <-100
train$accommodates_score[train$accommodates == 5] <-100
train$accommodates_score[train$accommodates == 6] <-100
train$accommodates_score[train$accommodates == 2] <-90
train$accommodates_score[train$accommodates == 3] <-90
train$accommodates_score[train$accommodates == 7] <-80
train$accommodates_score[train$accommodates == 8] <-80
train$accommodates_score[train$accommodates == 9] <-80
train$accommodates_score[train$accommodates == 10] <-80
train$accommodates_score[train$accommodates == 1] <-40
train$accommodates_score <- as.numeric(train$accommodates_score) 

#bathroom scoring conversion
train$bathrooms[train$bathrooms >1] <-100
train$bathrooms[train$bathrooms ==1] <-80
train$bathrooms[train$bathrooms <1] <-0
train$bathrooms <- as.numeric(train$bathrooms)

#price per person
train$price <- sub('$','',as.character(train$price),fixed=TRUE)
train$price <- sub(',','',as.character(train$price),fixed=TRUE)
train$price <- as.numeric(train$price)
train$price_per_person[train$price > 0] <- train$price / train$accommodates
train$price_per_person[train$price == 0] <- 0
train$price_score[train$price_per_person > 100] <-60
train$price_score[train$price_per_person < 25] <-70
train$price_score[train$price_per_person > 24 & train$price_per_person < 51] <-100
train$price_score[train$price_per_person > 49 & train$price_per_person < 76] <-90
train$price_score[train$price_per_person > 74 & train$price_per_person < 101] <-80
train$price_score <- as.numeric(train$price_score)

#secuirty deposit conversion
train$security_deposit <- sub('$','',as.character(train$security_deposit),fixed=TRUE)
train$security_deposit <- sub(',','',as.character(train$security_deposit),fixed=TRUE)
train$security_deposit <- as.numeric(train$security_deposit)
train$security_per_person[train$security_deposit > 0] <- train$security_deposit / train$accommodates
train$security_per_person[train$security_deposit == 0] <- 0
train$security_score[train$security_per_person >=0 & train$security_per_person < 51] <-100
train$security_score[train$security_per_person >49 & train$security_per_person < 101] <-90
train$security_score[train$security_per_person >100 & train$security_per_person < 151] <-80
train$security_score[train$security_per_person >150 & train$security_per_person < 201] <-70
train$security_score[train$security_per_person >200 & train$security_per_person < 351] <-60
train$security_score[train$security_per_person >350 & train$security_per_person < 501] <-50
train$security_score[train$security_per_person >499 & train$security_per_person < 651] <-40
train$security_score[train$security_per_person >650 & train$security_per_person < 801] <-30
train$security_score[train$security_per_person >800] <-20
train$security_score <- as.numeric(train$security_score)

#cleaning fee conversion
train$cleaning_fee <- sub('$','',as.character(train$cleaning_fee),fixed=TRUE)
train$cleaning_fee <- sub(',','',as.character(train$cleaning_fee),fixed=TRUE)
train$cleaning_fee <- as.numeric(train$cleaning_fee)
train$cleaning_per_person[train$cleaning_fee == 0] <- 0
train$cleaning_per_person[train$cleaning_fee > 0] <- train$cleaning_fee / train$accommodates
train$cleaning_score[train$cleaning_per_person >=0 & train$cleaning_per_person < 21] <-100
train$cleaning_score[train$cleaning_per_person >20 & train$cleaning_per_person < 41] <-90
train$cleaning_score[train$cleaning_per_person >40 & train$cleaning_per_person < 61] <-80
train$cleaning_score[train$cleaning_per_person >60 & train$cleaning_per_person < 81] <-70
train$cleaning_score[train$cleaning_per_person >80 & train$cleaning_per_person < 101] <-60
train$cleaning_score[train$cleaning_per_person >100 & train$cleaning_per_person < 121] <-50
train$cleaning_score[train$cleaning_per_person >120] <-0
train$cleaning_score <- as.numeric(train$cleaning_score)

#has availability conversion
train$has_availability[train$has_availability=='t'] <- 100
train$has_availability[train$has_availability=='f'] <- 0
train$has_availability <- as.numeric(train$has_availability)

#Change cancellation_policy into numericals
#sapply(SFO_Data$cancellation_policy,switch,'flexible'=10,'moderate'=8,'strict'=6,
#       'super_strict_30'=4,'super_strict_60'=2)
train$cancellation_policy[train$cancellation_policy == "flexible"] <- 10
train$cancellation_policy[train$cancellation_policy == "moderate"] <- 8
train$cancellation_policy[train$cancellation_policy == "strict"] <- 6
train$cancellation_policy[train$cancellation_policy == "super_strict_30"] <- 4
train$cancellation_policy[train$cancellation_policy == "super_strict_60"] <- 2
train$cancellation_policy <- as.numeric(train$cancellation_policy)

#number of reviews
train$number_of_reviews_score <- (train$number_of_reviews/max(train$number_of_reviews))*100
train$number_of_reviews_score <- round(train$number_of_reviews_score,2)
train$number_of_reviews_score <- as.numeric(train$number_of_reviews_score)
#review rating
train$review_scores_rating <- train$review_scores_rating *10

#review cleanliness
train$review_scores_cleanliness <- train$review_scores_cleanliness *10

#review checkin
train$review_scores_checkin <- train$review_scores_checkin *10

#review communication
train$review_scores_communication <- train$review_scores_communication *10

#review location
train$review_scores_location <- train$review_scores_location *10

#review value
train$review_scores_value <- train$review_scores_value *10

#amenities
train$amenities

#hosting score 10
train$host_score <- ((train$host_response_time*20)/100) + ((train$host_is_superhost*30)/100) + ((train$host_identity_verified*50)/100)
#listing score 15
train$listing_score <- ((train$is_location_exact*10)/100) + ((train$property_type_score*20)/100) + ((train$room_type*15)/100) + ((train$accommodates_score*30)/100) + ((train$bathrooms*10)/100) 
#+((train$amenities*20)/100)
#pricing score 25
train$pricing_score <- ((train$price_score*70)/100) + ((train$security_score*15)/100) + ((train$cleaning_score*15)/100)  
#review score 25
train$review_score <- ((train$number_of_reviews_score*5)/100) + ((train$review_scores_rating*5)/100) +((train$review_scores_cleanliness*30)/100) + ((train$review_scores_checkin*10)/100) + ((train$review_scores_communication*10)/100) + ((train$review_scores_location*15)/100) + ((train$review_scores_value*15)/100)
train$review_score <- round(train$review_score,2)
#easbility score 10
train$easibility_score <- ((train$has_availability*60)/100) + ((train$cancellation_policy*40)/100)

train$popularity_index <- ((train$host_score*10)/100) + ((train$listing_score*15)/100) + ((train$pricing_score *25)/100) + ((train$review_score*25)/100) + ((train$easibility_score*10)/100) 
train$popularity_index <- round(train$popularity_index,2)


counts <- table(train$popularity_index)
barplot(counts, main="Popularity Index Distribution", 
        xlab="Score")

str(test)


#plot on the map Red having popularity index = 100
#sum(train$popularity_index=="100")
#train_temp <- subset(train, (train$popularity_index ==100))
train_temp <- train[1:1000,]
library(leaflet)

counts <- table(train_temp$popularity_index)
barplot(counts, main="Popularity Index Distribution", 
        xlab="Score")

#leaflet(data = train_temp) %>% addTiles() %>%
# addMarkers(~longitude, ~latitude, popup = ~street)

#mutate(train_temp, group = cut(popularity_index, breaks = c(100, 80, 75, 70, Inf), labels = c("100", "90", "80","70"))) -> mytemp
levels <- c(-Inf, 70, 75, 80, Inf)
labels <- c("good", "better", "best", "super")
mytemp <- mutate(train_temp, group = cut(train_temp$popularity_index,levels, labels = labels)) 


train_temp$url <- paste("https://www.airbnb.com/rooms/",train_temp$id,sep="")
### I edit this png file and created my own marker.
### https://raw.githubusercontent.com/lvoogdt/Leaflet.awesome-markers/master/dist/images/markers-soft.png
airbnbIcons <- iconList("super" = makeIcon("pink.png", iconWidth = 24, iconHeight =32),
                        "best" = makeIcon("blue.png", iconWidth = 24, iconHeight =32),
                        "better" = makeIcon("green.png", iconWidth = 24, iconHeight =32),
                        "good" = makeIcon("purple.png", iconWidth = 24, iconHeight =32))

listingurl_string <- paste("<b><a href='",train_temp$url,"'>",train_temp$id,"</a></b>")

pop_string <- paste("<b>Listing Id: </b>",listingurl_string,"<br>",
                    "<b>Address: </b>",train_temp$street,"<br>",
                    "<b>Property Type: </b>",train_temp$property_type,"<br>",
                    "<b>Room Type: </b>",train_temp$room_type,"<br>",
                    "<b>Accomodates: </b>",train_temp$accommodates,"<br>",
                    "<b>Price: </b>",train_temp$price)

leaflet(data = mytemp) %>% 
  addTiles() %>%
  addMarkers(icon = ~airbnbIcons[group],popup = ~pop_string)

###TO-DO :
# 1. install and convert R file into R-Markdown
#2. Publish R Markdown using Knitr
#3. install and use RPubs for publishing on web

# # Build a regression model
# SFOmodel <- lm(review_scores_rating ~ review_scores_cleanliness + review_scores_checkin +
#               review_scores_communication + review_scores_location +
#                 review_scores_value + requires_license + instant_bookable + cancellation_policy,
#               data = train.data)
# 
# #This didnt help much as unable to interpret anything
# plot(SFOmodel)
# 
# #Still Error as Figure Margins Too Large
# SFO_temp_Data <- select(SFO_Data, review_scores_rating, review_scores_accuracy,
#                    review_scores_cleanliness, review_scores_checkin, 
#                    review_scores_communication, review_scores_location, 
#                    review_scores_value, requires_license, instant_bookable, 
#                    cancellation_policy)
# train.list = sample(nrow(SFO_temp_Data), 20)
# # Pick 20 records as corelation verification sample data
# train.data = SFO_temp_Data[train.list,]
# 
# # How much is rest data. Is it 980?
# test.data = SFO_Data[-train.list,]
# require(graphics)
# pairs(train.data, main = "train.data data")

#replace all blank values with N/A or with 0 (N/A for strings/chars and 0 for count or price)
#rm(test.data, train.data)#to remove datasets
#remove(train.list)#to remove variable values
#rm(SFOmodel)#to remove variable values#remove/rm can be used interchangeably
#rm(SFO_temp_Data)
#unique(train_temp$popularity_index)

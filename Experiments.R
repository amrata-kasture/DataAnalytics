library(dplyr)
setwd('/Users/amrata/Documents/Data Analytics/DA_Proj')

listcsv <- dir(pattern = "*_AirBnB_listings.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  SFO_Data <- read.csv(listcsv[k], na.strings=c(""," ","N/A","NA"),head=TRUE,stringsAsFactors=FALSE)
  break
}

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


SFO_Data$security_deposit[is.na(SFO_Data$security_deposit)] <- 0
SFO_Data$cleaning_fee[is.na(SFO_Data$cleaning_fee)] <- 0
SFO_Data$guests_included[is.na(SFO_Data$guests_included)] <- 0
SFO_Data$extra_people[is.na(SFO_Data$extra_people)] <- 0

SFO_Data$cancellation_policy[SFO_Data$cancellation_policy == "flexible"] <- 10
SFO_Data$cancellation_policy[SFO_Data$cancellation_policy == "moderate"] <- 8
SFO_Data$cancellation_policy[SFO_Data$cancellation_policy == "strict"] <- 6
SFO_Data$cancellation_policy[SFO_Data$cancellation_policy == "super_strict_30"] <- 4
SFO_Data$cancellation_policy[SFO_Data$cancellation_policy == "super_strict_60"] <- 2

SFO_Data$instant_bookable[SFO_Data$instant_bookable == "t"] <- 10
SFO_Data$instant_bookable[SFO_Data$instant_bookable == "f"] <- 0

SFO_Data$requires_license[SFO_Data$requires_license == "t"] <- 10
SFO_Data$requires_license[SFO_Data$requires_license == "f"] <- 0

#Remove rows having null/NA valuesand name the dataset for Indexing
SFO_Data <- na.omit(SFO_Data)

divData = sample(1:nrow(SFO_Data), size=0.2*nrow(SFO_Data))

# Split data
test = SFO_Data[divData,]
dim(test) 
train = SFO_Data[-divData,]
dim(train) 


# # Build a regression model
SFOmodel <- lm(review_scores_rating ~ review_scores_cleanliness + review_scores_checkin +
              review_scores_communication + review_scores_location +
                review_scores_value + requires_license + instant_bookable + cancellation_policy,
              data = train)

plot(SFOmodel)

SFO_temp_Data <- select(SFO_Data, review_scores_rating, review_scores_accuracy,
                   review_scores_cleanliness, review_scores_checkin,
                   review_scores_communication, review_scores_location,
                   review_scores_value, requires_license, instant_bookable,
                   cancellation_policy)
train.list = sample(nrow(SFO_temp_Data), 500)
# Pick 20 records as corelation verification sample data
train.data = SFO_temp_Data[train.list,]

# How much is rest data. Is it 980?
test.data = SFO_Data[-train.list,]

str(train.data)

train.data$instant_bookable<-strtoi(train.data$instant_bookable)
train.data$cancellation_policy<-strtoi(train.data$cancellation_policy)
train.data$requires_license<-strtoi(train.data$requires_license)

train.data$requires_license<-as.numeric(as.character(train.data$requires_license))

require(graphics)
pairs(train.data, main = "train.data data")

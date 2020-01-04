## ------------------------------------------------- ##
## Aerial Bombardments on Europe During World War II ##
##              created by Simon Cox                 ##
##          final version 4 January 2020             ##
## ------------------------------------------------- ##

# approximate time for complete analysis of this script is 60 minutes
# approximation based on my not so powerfull laptop
# script consists of 5 parts
# 0. importing packages & loading dataset  approx.  1 min
# 1. data wrangling                        approx. 10 min
# 2. data analysis, creating graphs        approx.  5 min
# 3. machine learning                      approx. 30 min
# 4. animations                            approx. 15 min


## --------------------------- ##
## 0. Import required packages ##
## --------------------------- ##

if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(sp)) install.packages("sp", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(gifski)) install.packages("gifski", repos = "http://cran.us.r-project.org")
if(!require(Amelia)) install.packages("Amelia", repos = "http://cran.us.r-project.org")
if(!require(visdat)) install.packages("visdat", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(gganimate)) install.packages("gganimate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(rworldmap)) install.packages("rworldmap", repos = "http://cran.us.r-project.org")


## ----------------------------------------------------------------------------------- ##
## Loading the dataset "Aerial Bombing Operations in World War II" from www.kaggle.com ##
## ----------------------------------------------------------------------------------- ##

path <- "data.csv"
dat <- read.csv(path, header = TRUE, sep = ",", na.strings=c("","NA"))


## ----------------- ##
## 1. Data wrangling ##
## ----------------- ##

# remove columns with more than 50% NA's
ops <- dat[, colMeans(is.na(dat)) < 0.5] 
# remove columns without a target longitude and latitude
ops <- ops %>% filter(!is.na(Target.Longitude) & !is.na(Target.Latitude)) 

# boundary values for map of Europe
min_lon <- -20 
max_lon <- 30
min_lat <- 35
max_lat <- 60
# make plot of Europe to be used as underlay
eu_map <- ggplot() +  borders("world", colour = "gray85", fill = "gray70") +
  coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) +
  labs(x="LON", y="LAT")

## find the correct country names
# get the world map
worl_map = getMap(resolution = "high") 
# create function to find the correct country based on target latitude and longitude
find_country <- function(coordinate){
  spatial_point = SpatialPoints(coordinate, proj4string = CRS(proj4string(worl_map)))
  index = over(spatial_point, worl_map)
  index$ADMIN
}
# run the function for all entries in the ops dataset
ops <- ops %>% mutate(temp = toupper(find_country(data.frame(Target.Longitude,
                                                             Target.Latitude))))
# replace target country with correct one unless function returns NA (op was in sea)
ops <- ops %>% filter(!is.na(temp)) %>% mutate(Target.Country = temp)

## remove unwanted entries from dataset
# Select only countries within Europe and North-Africa
ops <- ops %>% filter(between(Target.Longitude, min_lon, max_lon) &
                        between(Target.Latitude, min_lat, max_lat))
# del North African countries and rename countries
ops <- ops %>% filter(!Target.Country %in% c("ALGERIA", "TUNISIA"))
ops$Target.Country[ops$Target.Country == "NETHERLANDS"] <- "THE NETHERLANDS"     # rename
ops$Target.Country[ops$Target.Country == "CZECH REPUBLIC"] <- "CZECHOSLOVAKIA"   # rename
ops$Target.Country[ops$Target.Country == "SLOVAKIA"] <- "CZECHOSLOVAKIA"         # rename
ops$Target.Country <- as_factor(ops$Target.Country) # turn back to factors
ops <- ops %>% select(-temp) %>% droplevels()       # remove temp and drop unused levels
# reduce length column names
colnames(ops) <- c("mission_id", "date", "thor", "country", "air_force", "ac_series",
                   "t_id", "t_country", "t_city", "t_type", "t_industry", "t_priority",
                   "t_lat", "t_lon", "altitude", "attacking_ac", "he_weight_tons",
                   "total_weight_tons", "source_id")

# Rename target industries
ops$t_industry <- gsub('\"', "", ops$t_industry)   # remove backslash
ops$t_industry <- gsub(' - ', " ", ops$t_industry) # remove -
ops$t_industry <- gsub('-', " ", ops$t_industry)   # remove -
ops$t_industry <- gsub('  ', " ", ops$t_industry)  # remove double space
ops$t_industry <- as_factor(gsub(' $', "", ops$t_industry)) # remove end space
# rename long industry descritions
ops <- transform(ops, t_industry=revalue(t_industry, c("IRON AND STEEL PRODUCTION FACILITIES, BLAST FURNACES, BOILER SHOPS, FORGES, FOUNDRIES, STEEL WORKS, ROLLING MILLS" =
                                                         "IRON AND STEEL PRODUCTION FACILITIES", 
                                                       "MANUFACTURING INSTALLATIONS (NOT SPECIFICALLY IDENTIFIED BELOW FROM CLASS 11 THROUGH 59)" =
                                                         "MANUFACTURING INSTALLATIONS", 
                                                       "PUBLIC UTILITIES ELECTRIC LIGHT AND POWER COMPANIES, GAS COMPANIES, TELEPHONE COMPANIES, WATER COMPANIES." =
                                                         "PUBLIC UTILITIES", 
                                                       "TRANSPORTATION FACILITIES (NOT SPECIFICALLY IDENTIFIED BELOW)" =
                                                         "TRANSPORTATION FACILITIES", 
                                                       "TACTICAL TARGETS: (UNIDENTIFIED OR NOT LISTED BELOW)" =
                                                         "TACTICAL TARGETS",
                                                       "RR INSTALLATIONS, TRACKS, MARSHALLING YARDS, AND STATIONS" =
                                                         "RR INST., TRACKS, M. YARDS, AND STATIONS")))

# tranform dates from factor to date
ops$date <- as.Date(mdy(ops$date))
# transform altitude to meters
ops$altitude <- ops$altitude * 100 * 0.3048
# combine aircraft series of same type but different versions
ops$ac_series <- fct_collapse(ops$ac_series,
                              B17 = c('0B17', 'B17', 'GB17', 'OB17', 'SB17'), 
                              B24 = c('0B24', 'B24', 'GB24', 'OB24', 'SB24'))
# replace NA's by -1 or UNKNOWN
ops$thor <- factor(ops$thor, levels=c(levels(ops$thor), "UNKNOWN"))
ops$thor[is.na(ops$thor)] <- "UNKNOWN"
ops$country <- factor(ops$country, levels=c(levels(ops$country), "UNKNOWN"))
ops$country[is.na(ops$country)] <- "UNKNOWN"
ops$air_force <- factor(ops$air_force, levels=c(levels(ops$air_force), "UNKNOWN"))
ops$air_force[is.na(ops$air_force)] <- "UNKNOWN"
ops$ac_series <- factor(ops$ac_series, levels=c(levels(ops$ac_series), "UNKNOWN"))
ops$ac_series[is.na(ops$ac_series)] <- "UNKNOWN"
ops$t_id <- as.integer(ops$t_id)
ops$t_id[is.na(ops$t_id)] <- -1
ops <- ops %>% filter(!is.na(t_city))
ops$t_type[is.na(ops$t_type)] <- "UNIDENTIFIED TARGET"
ops$t_industry[is.na(ops$t_industry)] <- "UNIDENTIFIED TARGETS"
ops$t_priority <- factor(ops$t_priority, levels=c(levels(ops$t_priority), "UNKNOWN"))
ops$t_priority[is.na(ops$t_priority)] <- "UNKNOWN"
ops$altitude[is.na(ops$altitude)] <- -1
ops$attacking_ac[is.na(ops$attacking_ac)] <- -1
ops$attacking_ac <- as.integer(ops$attacking_ac)
ops$he_weight_tons[is.na(ops$he_weight_tons)] <- -1
ops$total_weight_tons[is.na(ops$total_weight_tons)] <- -1
ops$source_id[is.na(ops$source_id)] <- -1
ops$source_id <- as.integer(ops$source_id)


## ---------------- ##
## 2. Data analysis ##
## ---------------- ##

## 2.1 Relation between target country and ...
### ... the number of missions
# select the 10 most bombed countries in EU
top_10_countries <- ops %>% group_by(t_country) %>%
  summarize(val = n()) %>% arrange(desc(val)) %>% top_n(10, val)

# plot most bombed countries vs total number of missions (flip bar plot)
top_10_countries %>%
  ggplot(aes(x=reorder(t_country, val), y=val, fill=t_country)) +
  geom_bar(stat="identity") +
  labs(title="Total number of missions during the war per country",
       x="Target country", y = "Total number of missions") +
  coord_flip() +
  theme(legend.position="none", plot.title = element_text(size=12))

# calculate proprtion for each country
sub_country <- ops %>% group_by(t_country) %>% summarise(prop = n()/nrow(ops)) %>% top_n(10,prop)
# rearrange and plot with two digits
knitr::kable(arrange(sub_country,-sub_country$prop), col.names=c("Target country", "Proportion"), digits = 2)

# plot top 10 most bomb countries vs total number of missions and stratify per year
ops %>% filter(ops$t_country %in% top_10_countries$t_country) %>% # filter top 10
  mutate(year = year(date)) %>% filter(!year==1939) %>%           # above 1939
  group_by(t_country, year) %>% summarize(val = n()) %>%          # group dan calculate nr. of times
  # create plot 
  ggplot(aes(x=reorder(t_country, val), y=val, fill=t_country)) +
  geom_bar(stat="identity") +
  scale_y_continuous(breaks = c(0,10000,20000)) +
  labs(title="Total number of missions per year per country",
       x="Target country", y = "Total number of missions") +
  coord_flip() +
  theme(legend.position="none", plot.title = element_text(size=12)) +
  facet_wrap(~year)

### ... the total weight of bombs dropped
# get top 10 countries by total weight of bombs
top_10_countries <- ops %>% group_by(t_country) %>%
  summarize(tons=sum(total_weight_tons)) %>% arrange(desc(tons)) %>%
  top_n(10, tons)
# create bar plot and flip coordinates
top_10_countries %>%
  ggplot(aes(x=reorder(t_country, tons), y=tons, fill=t_country)) +
  geom_bar(stat="identity") +
  labs(title="Total weight of bombs dropped per country",
       x = "Target country", y = "Total weight of bombs in tons") +
  coord_flip() +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(legend.position="none", plot.title = element_text(size=12))
# save only top 10 country names to be used furtheron
top_10_countries <- top_10_countries %>% pull(t_country)

# filter top 10 countries
ops %>% filter(ops$t_country %in% top_10_countries) %>%
  filter(total_weight_tons >= 1) %>%
  # create deinsity plot of weight of bombs per mission for top 10 countries
  ggplot(aes(x=total_weight_tons, y=reorder(t_country, total_weight_tons), fill = t_country)) +
  geom_density_ridges(bandwidth = 0.05, alpha=0.5) +
  scale_x_log10(breaks=c(0.1,1,10,100,1000)) +
  labs(title="Distribution of total weight of bombs\n per mission for different countries",
       x="Total weight of bombs", y="Target country") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, size=12))

### ... the aircraft type
# get top 10 aircraft series (n10_acs) and top four countries (n4_country) by missions
n10_acs <- ops %>% group_by(ac_series) %>% summarize(val = n()) %>%
  arrange(desc(val)) %>% top_n(10, val)  %>% pull(ac_series)
n4_country <- ops %>% group_by(t_country) %>% summarize(val = n()) %>%
  arrange(desc(val)) %>% top_n(4, val) %>% pull(t_country)

# filter and calculate number of times ac_series were used in different countries
ops %>% filter(ac_series %in% n10_acs & t_country %in% n4_country) %>%
  group_by(ac_series, t_country) %>% summarize(val = n()) %>%
  # create bar plot and flip coordinates
  ggplot(aes(x=reorder(ac_series, val), y=val, fill=ac_series)) +
  geom_bar(stat="identity") +
  labs(title="Number of missions per aircraft serie per country",
       x = "Aircraft series", y = "Total number of missions") +
  coord_flip() +
  theme(legend.position = 'none', plot.title = element_text(size=12)) +
  facet_wrap(~t_country)


## 2.2 Relation between the target industry and...
### ... the number of missions
# get top 10 industries by number of missions, filter out UNIDENTIFIED TARGETS
n10_t_industry <- ops %>% filter(!t_industry == "UNIDENTIFIED TARGETS") %>%
  group_by(t_industry) %>% summarize(val=n()) %>% arrange(desc(val)) %>%
  top_n(10, val) %>% pull(t_industry)
# create bar plot of top 10 industries against number of missions 
ops %>% filter(ops$t_industry %in% n10_t_industry) %>%
  ggplot(aes(reorder(stringr::str_wrap(t_industry,25),t_industry,function(x) length(x)), fill = t_industry)) +
  geom_bar() +
  coord_flip() +
  labs(title="Total number of missions per target industry",
       x="Target industry", y="Total number of missions") +
  theme(legend.position = 'none', plot.title = element_text(size=12))

# calculate proprtion for top 10 industries
sub_industry <- ops %>% group_by(t_industry) %>% filter(!t_industry == "UNIDENTIFIED TARGETS") %>%
  summarise(prop = n()/nrow(ops)) %>% top_n(10,prop)
# rearrange and plot with two digits
knitr::kable(arrange(sub_industry,-sub_industry$prop), col.names=c("Target industry", "Proportion"), digits = 2)

### ... the date
# filter top 10 industries
ops %>% filter(ops$t_industry %in% n10_t_industry) %>%
  # create density plot of top 10 industies and number of missions over the years
  ggplot(aes(x = date, stringr::str_wrap(t_industry,25), fill = t_industry)) +
  geom_density_ridges(bandwidth = 20, alpha=0.5) +
  theme(legend.position = 'none', plot.title = element_text(size=12)) +
  labs(title="Distribution of attacks on different industries during the war",
       x="Date", y="Target industry")

### ... the target priority
# filter top 10 industries
ops %>% filter(ops$t_industry %in% n10_t_industry) %>%
  # create jitter plot of industry vs priority
  ggplot(aes(t_priority, reorder(stringr::str_wrap(t_industry, 25),t_industry,function(x) length(x)),
             color = t_industry)) +
  geom_jitter(alpha=0.3, size=0.1) +
  labs(title="Jitter plot for different priority - industry combinations",
       x="Target priority", y="Target industry") +
  theme(legend.position = 'none', plot.title = element_text(size=12))

### ... the total weight of the bombs
# set breaks for log scale plot
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
ops %>% filter(t_industry %in% n10_t_industry, total_weight_tons > 0) %>%
  # plot points as jitter and distribution as violin and set y-axis to log scale
  ggplot(aes(stringr::str_wrap(t_industry, 20), total_weight_tons, color = t_industry)) +
  geom_jitter(alpha=0.2) +
  geom_violin(trim=FALSE, alpha=0.5) +
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
  labs(title="Distribution of the total weight of bombardments for different industries",
       x="Target industry", y="Total weight of bombs") +
  theme(axis.text.x = element_text(angle =45, hjust=1), legend.position = 'none',
        plot.title = element_text(size=12))

## 2.3 Is there a relation between the target country and target industry?
# filter top 10 industry and top 10 countries
ops %>% filter(t_industry %in% n10_t_industry
               & t_country %in% top_10_countries) %>%
  # create jitter plot of target country vs industry
  ggplot(aes(reorder(t_country, t_country, function(x){-length(x)}),
             stringr::str_wrap(reorder(t_industry,t_industry, FUN=length), 25),
             color = t_country)) +
  geom_jitter(alpha=0.3, size=0.1) +
  labs(title="Jitter plot for different country - industry combinations",
       x="Target country", y="Target industry") +
  theme(axis.text.x = element_text(angle =45, hjust=1),
        legend.position = 'none', plot.title = element_text(size = 12))


## ------------------------------ ##
## 3. Machine learning algorithms ##
## ------------------------------ ##

### Create subset
# create subset and get rid of unwanted columns
sub_set <- select(ops, -t_type, -t_city, -t_lat, -t_lon)

# filter top 10 countries
country_filter <- sub_set %>% group_by(t_country) %>%
  summarise(prop = n()/nrow(sub_set)) %>% top_n(10,prop)
# filter top 20 industries
industry_filter <- sub_set %>% group_by(t_industry) %>%
  filter(!t_industry == "UNIDENTIFIED TARGETS") %>%
  summarise(prop = n()/nrow(sub_set)) %>% top_n(20,prop)
# filter only top 10 countries and top 20 industries in subset
sub_set <- sub_set %>% filter(t_country %in% sub_country$t_country &
                                t_industry %in% sub_industry$t_industry)

# restore NA's back into the subset (these were removed earlier)
sub_set$thor <- revalue(sub_set$thor, c("UNKNOWN"=NA))
sub_set$country <- revalue(sub_set$country, c("UNKNOWN"=NA))
sub_set$air_force <- revalue(sub_set$air_force, c("UNKNOWN"=NA))
sub_set$ac_series <- revalue(sub_set$ac_series, c("UNKNOWN"=NA))
sub_set$t_id[sub_set$t_id == -1] <- NA
sub_set$t_priority <- revalue(sub_set$t_priority, c("UNKNOWN"=NA))
sub_set$altitude[sub_set$altitude <= 0] <- NA
sub_set$attacking_ac[sub_set$attacking_ac == -1] <- NA
sub_set$he_weight_tons[sub_set$he_weight_tons == -1] <- NA
sub_set$total_weight_tons[sub_set$total_weight_tons == -1] <- NA
sub_set$source_id[sub_set$source_id == -1] <- NA

# remove NA's
sub_set <- sub_set %>% filter(!is.na(attacking_ac)
                              & !is.na(altitude)
                              & !is.na(total_weight_tons)
                              & !is.na(he_weight_tons)
                              & !is.na(thor)
                              & !is.na(t_priority)
                              & !is.na(air_force))

# create subset of 5,000 rows by randomly sampling
sub_set <- sample_n(sub_set, 5000, replace=FALSE)
# remove unused levels
sub_set <- droplevels(sub_set)

# transform all predictors which now are characters to integers
sub_set$thor <- as.integer(sub_set$thor)
sub_set$country <- as.integer(sub_set$country)
sub_set$air_force <- as.integer(sub_set$air_force)
sub_set$ac_series <- as.integer(sub_set$ac_series)
sub_set$t_priority <- as.integer(sub_set$t_priority)
# create test and training set (20/80)
test_index <- createDataPartition(sub_set$t_country, times=1, p=0.2, list=FALSE)
test_set <- sub_set[test_index,]
train_set <- sub_set[-test_index,]

# make sure the levels in the train and test set are the same
test_set <- test_set %>%
  semi_join(train_set, by = "ac_series") %>%
  semi_join(train_set, by = "t_priority") %>%
  semi_join(train_set, by = "t_country") %>%
  semi_join(train_set, by = "t_industry")%>%
  semi_join(train_set, by = "thor")
train_set <- train_set %>%
  semi_join(test_set, by = "ac_series") %>%
  semi_join(test_set, by = "t_priority") %>%
  semi_join(test_set, by = "t_country") %>%
  semi_join(test_set, by = "t_industry") %>%
  semi_join(test_set, by = "thor")
# drop unused levels
train_set <- droplevels(train_set) 
test_set <- droplevels(test_set)


## -----------------------------
## Predicting the target country
# t_industry is a predictor for this exercise, make it numeric
test_set_tc <- test_set %>% mutate(t_industry = as.numeric(t_industry))
train_set_tc <- train_set %>% mutate(t_industry = as.numeric(t_industry))

### Probability
# calculate probabilities for subset
prob <- sub_set %>% group_by(t_country) %>%
  summarize(prob = n()/nrow(sub_set)) %>% arrange(-prob)

# predict target country using the probability
guessing <- sample(prob$t_country, size=nrow(test_set_tc),
                   prob = prob$prob, replace=TRUE)
# calculate accuracy and add to table
summary_tc <- tibble(Method = "Target country by probability",
                     Accuracy = mean(guessing == test_set_tc$t_country))

### Logistic regression
# fit glm model
fit_glm <- train_set_tc %>%
  mutate(t_country = as.numeric(t_country)) %>% # make country numeric
  glm(t_country ~ ., data=.)
# predict target country
y_hat <- round(predict(fit_glm, test_set_tc))
# add to table
summary_tc <- add_row(summary_tc, Method = "Target country with glm",
                      Accuracy = mean(y_hat == as.integer(test_set_tc$t_country)))

### knn (about 5 - 10 minutes)
# train knn model
train_knn <- train_set_tc %>%
  train(t_country ~ .,
        method="knn",
        tuneGrid = data.frame(k = seq(1, 10, 1)),
        data=.)
# predict target country
y_hat <- predict(train_knn, test_set_tc)
# add to table
summary_tc <- add_row(summary_tc, Method = "Target country with knn",
                      Accuracy = confusionMatrix(y_hat, test_set_tc$t_country)$overall["Accuracy"])

### Decision tree (about 1 - 2 minutes)
# train decision tree model
train_rpart <- train_set_tc %>%
  train(t_country ~ .,
        method="rpart",
        tuneGrid = data.frame(cp=seq(0.0,0.1,len=25)),
        data = .)
# predict target country
y_hat <- predict(train_rpart, test_set_tc)
# add to table
summary_tc <- add_row(summary_tc, Method = "Target country with rpart",
                      Accuracy = confusionMatrix(y_hat, test_set_tc$t_country)$overall["Accuracy"])

### Random forest (about 10 - 15 minutes)
# create sequence for cross validation
nodesize <- seq(1, 51, 10)
# apply all nodesizes to train function
acc <- sapply(nodesize, function(ns){
  train(t_country ~ ., method = "rf", data = train_set_tc,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy})

# use the best fit for nodesize to train the random forest model
ns <- nodesize[which.max(acc)]
train_rf <- train_set_tc %>% train(t_country ~ ., method="rf", data = ., nodesize=ns)
# make predictions
y_hat <- predict(train_rf, test_set_tc)
# add to table
summary_tc <- add_row(summary_tc, Method = "Target country with rf",
                      Accuracy = confusionMatrix(y_hat, test_set_tc$t_country)$overall["Accuracy"])


## ------------------------
## Predicting the target country
# t_industry is a predictor for this exercise, make it numeric
test_set_ti <- test_set %>% mutate(t_country = as.numeric(t_country))
train_set_ti <- train_set %>% mutate(t_country = as.numeric(t_country))

### Probability
# calculate probabilities for subset
prob <- sub_set %>% group_by(t_industry) %>%
  summarize(prob = n()/nrow(sub_set)) %>% arrange(-prob)

# predict target country using the probability
guessing <- sample(prob$t_industry, size=nrow(test_set_ti),
                   prob = prob$prob, replace=TRUE)
# calculate accuracy and add to table
summary_ti <- tibble(Method = "Target industry by probability",
                     Accuracy = mean(guessing == test_set_ti$t_industry))

### Logistic regression
# fit glm model
fit_glm <- train_set_ti %>%
mutate(t_industry = as.numeric(t_industry)) %>% # make industry numeric
  glm(t_industry ~ ., data=.)
# predict target country
y_hat <- round(predict(fit_glm, test_set_ti))
# add to table
summary_ti <- add_row(summary_ti, Method = "Target industry with glm",
                      Accuracy = mean(y_hat == as.integer(test_set_ti$t_industry)))

### knn (about 5 - 10 minutes)
# train knn model
train_knn <- train_set_ti %>%
  train(t_industry ~ .,
        method="knn",
        tuneGrid = data.frame(k = seq(1, 10, 1)),
        data=.)
# predict target country
y_hat <- predict(train_knn, test_set_ti)
# add to table
summary_ti <- add_row(summary_ti, Method = "Target industry with knn",
                      Accuracy = confusionMatrix(y_hat, test_set_ti$t_industry)$overall["Accuracy"])

### Decision tree (about 1 - 2 minutes)
# train decision tree model with cross validation for "complexity parameter"
train_rpart <- train_set_ti %>%
  train(t_industry ~ .,
        method="rpart",
        tuneGrid = data.frame(cp=seq(0.0,0.1,len=25)),
        data = .)
# predict target country
y_hat <- predict(train_rpart, test_set_ti)
# add to table
summary_ti <- add_row(summary_ti, Method = "Target industry with rpart",
                      Accuracy = confusionMatrix(y_hat, test_set_ti$t_industry)$overall["Accuracy"])

### Random forest (about 10 - 15 minutes)
# create sequence for cross validation
nodesize <- seq(1, 51, 10)
# apply all nodesizes to train function
acc <- sapply(nodesize, function(ns){
  train(t_industry ~ ., method = "rf", data = train_set_ti,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})

# use the best fit for nodesize to train the random forest model
ns <- nodesize[which.max(acc)]
train_rf <- train_set_ti %>% train(t_industry ~ ., method="rf", data = ., nodesize=ns)
# make predictions
y_hat <- predict(train_rf, test_set_ti)
# add to table
summary_ti <- add_row(summary_ti, Method = "Target industry with rf",
                      Accuracy = confusionMatrix(y_hat, test_set_ti$t_industry)$overall["Accuracy"])

# plot both tables
summary_tc
summary_ti


## ------------- ##
## 4. ANIMATIONS ##
## ------------- ##

## Animation 1: Bar plot with bombardments per year for each EU country
# Selecting top 10 countries and after 1939
ops_EU <- ops %>% mutate(ym = format(date, "%Y-%m")) %>% 
  filter(t_country %in% top_10_countries & year(date) > 1939) %>%
  group_by(ym, t_country) %>% summarise(val = n())
# plot code
bar_gif <- ops_EU %>%
  ggplot(aes(x=reorder(t_country, val), y=val, fill=t_country)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=val), position=position_dodge(width=0.9),hjust=-0.1, size=6) +
  labs(x = "Target country", y = "Number of attacks") +
  scale_y_continuous(trans="log10") +
  coord_flip() +
  # animation code, transition per month
  transition_states(ym, transition_length = 1, state_length = 1) +
  enter_grow() +
  exit_shrink()+
  theme(plot.title = element_text(size=16),
        axis.title = element_text(size=15),
        axis.text = element_text(size=12),
        legend.position = "none") +
  labs(title = 'Total number of aerial bombings in {closest_state}') +
  ease_aes('linear')
# save the animation
gganimate::animate(bar_gif, duration=50, fps=10, renderer = gifski_renderer())
anim_save("bombardments-per-country-barplot.gif")

## Animation 2: Map of Europe with bombardments per week
# make plot of Europe to be used as underlay (smaller plot than before)
eu_map <- ggplot() +  borders("world", colour = "gray85", fill = "gray70") +
  coord_sf(xlim = c(-12, 22), ylim = c(35, 60)) +
  labs(x="LON", y="LAT")
# select top 10 countries
ops_EU <- ops %>% filter(t_country %in% top_10_countries)
#plot code
map_gif <- eu_map +
  geom_point(aes(x=t_lon, y=t_lat, size = total_weight_tons),
               data = ops_EU,
             fill = "deepskyblue3",
             alpha = 0.5,
             shape = 21) +
  theme(legend.title=element_text(size=15),
        legend.text = element_text(size=15),
        legend.position = c(0.1, 0.8),
        plot.title = element_text(size=20)) +
  scale_size_continuous(name = "Tons of Bombs", breaks=c(10, 100, 200, 300),
  labels = c("< 10", "10 - 100", "100 - 200", "> 200")) +
  # animation code
  transition_time(date) +
  shadow_wake(wake_length = 0.01) +
  labs(title = "Date: {frame_time}") +
  ease_aes('cubic-in-out')
# save the animation
anim <- animate(map_gif, fps=5, width = 1000, height = 800, renderer = gifski_renderer())
anim_save("bombardments-on-europe-map.gif")

## ------- ##
## the end ##
## ------- ##
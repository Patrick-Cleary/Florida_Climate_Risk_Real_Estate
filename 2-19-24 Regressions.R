#Appraiser data downloaded 2-19-2024 from Collier County Appraiser

buildings <- read.csv("INT_BUILDINGS.csv")
millage <- read.csv("INT_MILLAGE.csv")
land <- read.csv("INT_LAND.csv")
millage_rates <- read.csv("INT_MILLAGE_RATES.csv")
sales <- read.csv("INT_SALES.csv")
View(sales)
parcels <- read.csv("INT_PARCELS.csv")
View(parcels)
building_codes <- read.csv("INT_BUILDINGCODES.csv")
legal <- read.csv("INT_LEGAL.csv")
usecodes <- read.csv("INT_USECODES.csv")
subcondos <- read.csv("INT_SUBCONDOS.csv")
library(readxl)
SFH_Build_Since_2003 <- read_excel("SFH-Build-Since-2003.xlsx")
SFH <- SFH_Build_Since_2003
rm(SFH_Build_Since_2003)
#Cost of Flood Insurance data downloaded 2-19-2024 from FEMA: https://www.fema.gov/flood-insurance/work-with-nfip/risk-rating/single-family-home

library("readxl")
Risk_rating <- read_excel("fema_risk-rating-2.0_exhibits-2-3-4 (2).xlsx")
View(Risk_rating)
Risk_rating <- fema_risk_rating_2_0_exhibits_2_3_4_2_
ls()
rm(Risk_rating)
rm(fema_risk_rating_2_0_exhibits_2_3_4_2_)

#looking for building attributes

View(buildings)
View(building_codes)
View(usecodes)
View(land)
View(legal)
View(millage)
View(millage_rates)
View(parcels)
View(sales)
View(subcondos)
View(usecodes)
View(SFH_Build_Since_2003)

#SFH_Build has pool attribute, acres, number of stories, site zip

model1<-lm(SFH_Build_Since_2003$`SALE AMT` ~ SFH_Build_Since_2003$`NO OF STORIES` + SFH_Build_Since_2003$ACRES)
summary(model1)
model2<-lm(SFH_Build_Since_2003$`SALE AMT` ~ SFH_Build_Since_2003$`NO OF STORIES` + SFH_Build_Since_2003$`TOTAL SQ FEET`)
summary(model2)

#convert 'pool' from y/n to 1/0

SFH_Build_Since_2003$POOL <- ifelse(SFH_Build_Since_2003$POOL == 'y', 1, 0)
model3<-lm(SFH_Build_Since_2003$`SALE AMT` ~ SFH_Build_Since_2003$`NO OF STORIES` + SFH_Build_Since_2003$`TOTAL SQ FEET` + SFH_Build_Since_2003$POOL)
summary(model3)
summary(SFH_Build_Since_2003$POOL)

#i messed up this conversion somehow and now i need to reload the data

SFH <- read_excel("SFH-Build-Since-2003.xlsx")
SFH$POOL <- ifelse(SFH$POOL == 'Y', 1, 0)
summary(SFH$POOL)
hist(SFH$POOL)
model3<-lm(SFH$`SALE AMT` ~ SFH$ACRES + SFH$POOL + SFH$`TOTAL SQ FEET` + SFH$`NO OF STORIES`)
  summary(model3)
options(scipen=999)
logmodel3<-lm(log(SFH$`SALE AMT`) ~ SFH$ACRES + SFH$POOL + SFH$`TOTAL SQ FEET` + SFH$`NO OF STORIES`)
summary(logmodel3)
logmodel4<-lm(log(SFH$`SALE AMT`) ~ SFH$POOL + SFH$`TOTAL SQ FEET` + SFH$`NO OF STORIES`)
summary(logmodel4)

#currently, the best model is ln(sale amount) = Acres+Square Feet+Stories+Pool
#Now we need to bring in distances. We'll bring in climate risk last.
#The distance figures need to be brought in via ParcelID

distance_to_golf <- read.csv("E:/Florida Project/Regression/distance_to_golf.csv")
distance_to_beach <- read.csv("E:/Florida Project/Regression/distance_to_beach.csv")
merged_distances <- merge(distance_to_golf, distance_to_beach, by.x = "FLN", by.y = "FLN", all.x=TRUE)
View(merged_distances)
rm(merged_distances)
library(dplyr)
distance_to_beach <- distance_to_beach %>% 
  rename(distance_beach = distance)
distance_to_golf <- distance_to_golf %>% 
  rename(distance_golf = distance)
parcels <- parcels %>% 
  rename(FLN = GIS_FLN_NUM)

#merge with parcels

merged_df <- parcels %>% 
  left_join(distance_to_golf, by = "FLN") %>% 
  left_join(distance_to_beach, by = "FLN")

#merge with SFH

SFH <- SFH %>% 
  rename(PARCELID = 'PARCEL ID')
merged_df2 <- merge(SFH, merged_df, by = "PARCELID")
View(merged_df2)

#now try a regression with distance to beach, distance to golf, acres, square feet, stories, pool

logmodel5<-lm(log(merged_df2$`SALE AMT`) ~ merged_df2$POOL + merged_df2$`TOTAL SQ FEET` + merged_df2$`NO OF STORIES` + merged_df2$ACRES + merged_df2$distance_beach + merged_df2$distance_golf)
summary(logmodel5)
options(scipen=999)
summary(logmodel5)

#we get an adj r2 of .43, stat. significant negative relationships between distance variables and sales
#Final piece of the puzzle: risk
fema_risk_rating_2_0_exhibits_2_3_4_2_ <- read_excel("fema_risk-rating-2.0_exhibits-2-3-4 (2).xlsx", 
                                                     sheet = "Exhibit3", skip = 2)
fema_risk <- fema_risk_rating_2_0_exhibits_2_3_4_2_
rm(fema_risk_rating_2_0_exhibits_2_3_4_2_)
fema_risk$premium_increase = (fema_risk$`Average Risk-based Cost of Insurance`-fema_risk$`Average Current Cost of Insurance`)/fema_risk$`Average Current Cost of Insurance`
#NOTE: CHANGED FROM PROPORTIONAL CHANGE TO PERCENT CHANGE HERE ON 2-29-24
fema_risk<-fema_risk %>% 
  rename('SITE ZIP' = 'ZIP Code')

#add premium increase to merged_df2
fema_risk$`SITE ZIP` <- as.numeric(fema_risk$`SITE ZIP`)

# Merge the dataframes based on 'SITE ZIP'
merged_df2 <- merged_df2 %>%
  left_join(fema_risk %>% select(`SITE ZIP`, premium_increase), by = "SITE ZIP")




#now run a regression with premium increase
logmodel6<-lm(log(merged_df2$`SALE AMT`) ~ merged_df2$POOL + merged_df2$`TOTAL SQ FEET` + merged_df2$`NO OF STORIES` + merged_df2$ACRES + merged_df2$distance_beach + merged_df2$distance_golf + merged_df2$premium_increase)
  summary(logmodel6)
#premium increase is not statistically significant -- people aren't pricing it in
  
#Now we need to do some data cleaning: get rid of outliers and sales for below fair value, pick dates we're working with, etc. start by converting sale date into a date
merged_df2$`SALE DATE` <- as.Date(as.character(merged_df2$`SALE DATE`), format = "%m%d%Y")

# create new dataframe for aftter 04/01/2020
merged_df3 <- merged_df2 %>% 
  filter('SALE DATE' >= as.Date("2020-04-01"))
summary(merged_df2$`SALE DATE`)
str(merged_df2)
names(merged_df2)
View(merged_df2$`SALE DATE`)
table(merged_df2$`SALE DATE`)
barplot(table(merged_df2$`SALE DATE`))
na_count <- sum(is.na(merged_df2$`SALE DATE`))
  print(na_count)
non_missing_observations <- sum(!is.na(merged_df2$`SALE DATE`))
print(non_missing_observations)
#we're missing a super high number of dates for the observations, like 2/3 of them. will proceed for now but come back later to figure out what's wrong.
cleaned_df <- na.omit(merged_df2)
cleaned_df <- merged_df2 %>%  filter(!is.na(`SALE DATE`))
cleaned_df <- cleaned_df %>% 
  filter(`SALE DATE` >= as.Date("2020-04-01"))
cleaned_df <- cleaned_df %>% 
  filter(`SALE AMT`>=50000)

#now regression with sale amount=premium increases+year built+pool+distance to beach+distance to golf+sqft+acres+stories

logmodel7 <- lm(log(cleaned_df$`SALE AMT`) ~ cleaned_df$premium_increase + cleaned_df$`YEAR BUILT` + cleaned_df$POOL + cleaned_df$distance_beach + cleaned_df$distance_golf + cleaned_df$`TOTAL SQ FEET` + cleaned_df$ACRES + cleaned_df$`NO OF STORIES`) 
summary(logmodel7)

#this gives us an adj R2 of .6751


model7 <- lm(cleaned_df$`SALE AMT` ~ cleaned_df$premium_increase + cleaned_df$`YEAR BUILT` + cleaned_df$POOL + cleaned_df$distance_beach + cleaned_df$distance_golf + cleaned_df$`TOTAL SQ FEET` + cleaned_df$ACRES + cleaned_df$`NO OF STORIES`) 
summary(model7)

#when millage rate is included:

logmodel8 <- lm(log(cleaned_df$`SALE AMT`) ~ cleaned_df$premium_increase + cleaned_df$`YEAR BUILT` + cleaned_df$POOL + cleaned_df$distance_beach + cleaned_df$distance_golf + cleaned_df$`TOTAL SQ FEET` + cleaned_df$ACRES + cleaned_df$`NO OF STORIES` + cleaned_df$MILLAGE) 
summary(logmodel8)

#R2 stays about the same, millage is negative and not stat. significant
#Now add dummy variables for municipalities
library(fastDummies)
cleaned_df <- dummy_cols(cleaned_df, select_columns = "SITE CITY")
names(cleaned_df)
logmodel9 <- lm(log(cleaned_df$`SALE AMT`) ~ cleaned_df$premium_increase + cleaned_df$`YEAR BUILT` + cleaned_df$POOL + cleaned_df$distance_beach + cleaned_df$distance_golf + cleaned_df$`TOTAL SQ FEET` + cleaned_df$ACRES + cleaned_df$`NO OF STORIES` + cleaned_df$MILLAGE + cleaned_df$`SITE CITY_CHOKOLOSKEE` + cleaned_df$`SITE CITY_EVERGLADES CITY` + cleaned_df$`SITE CITY_GOODLAND` + cleaned_df$`SITE CITY_IMMOKALEE` + cleaned_df$`SITE CITY_MARCO ISLAND`) 
summary(logmodel9)
summary(cleaned_df$`SITE CITY`)
table(cleaned_df$`SITE CITY`)    
#2-20-24 saved script, exported cleaned_df to flash drive
write.csv(cleaned_df, row.names = TRUE)

#--

#2-28-24 robustness checks: create correlation matrix and VIF check
library(tidyverse)
library(corrplot)
?cor
cor_matrix <- cor(cleaned_df[, c("SALE AMT", "premium_increase", "YEAR BUILT", "POOL", "distance_beach", "distance_golf", "TOTAL SQ FEET", "ACRES", "NO OF STORIES", "MILLAGE", "SITE CITY_CHOKOLOSKEE", "SITE CITY_EVERGLADES CITY", "SITE CITY_GOODLAND", "SITE CITY_IMMOKALEE", "SITE CITY_MARCO ISLAND", "SITE CITY_NAPLES")])

# Print the correlation matrix
print(cor_matrix)

# Convert integer variables to numeric
integer_columns <- c("SITE CITY_CHOKOLOSKEE", "SITE CITY_EVERGLADES CITY", "SITE CITY_GOODLAND", "SITE CITY_IMMOKALEE", "SITE CITY_MARCO ISLAND", "SITE CITY_NAPLES")
cleaned_df[, integer_columns] <- lapply(cleaned_df[, integer_columns], as.numeric)

# Convert character variables to numeric
cleaned_df$POOL <- as.numeric(as.character(cleaned_df$POOL))
cleaned_df$POOL[is.na(cleaned_df$POOL)] <- 0  # Assuming 0 for missing values, you can replace it with another value if needed


# Verify the changes
str(cleaned_df$POOL)

# Calculate the correlation matrix
numeric_columns <- c("SALE AMT", "premium_increase", "YEAR BUILT", "POOL", "distance_beach", "distance_golf", "TOTAL SQ FEET", "ACRES", "NO OF STORIES", "MILLAGE")
cor_matrix <- cor(cleaned_df[, numeric_columns], use="complete.obs")  # use="complete.obs" excludes rows with NAs

# Print the correlation matrix
print(cor_matrix)

#Messed up my pool variable and now need to fix it
cleaned_df <- cleaned_df %>%
  left_join(SFH %>% select(PARCELID, POOL), by = "PARCELID") %>%
  mutate(
    POOL = coalesce(POOL.y, POOL.x)  # Prioritize POOL values from SFH (POOL.y), use cleaned_df's POOL (POOL.x) if missing
  ) %>%
  select(-POOL.x, -POOL.y) %>%  # Remove the temporary columns created during the join
  rename(POOL = POOL)  # Rename the final POOL column appropriately
cleaned_df$POOL <- as.character(cleaned_df$POOL)
SFH$POOL <- as.character(SFH$POOL)
# Verify the changes
str(cleaned_df$POOL)
unique(cleaned_df$POOL)
print(unique_values_pool)
cleaned_df$POOL <- ifelse(cleaned_df$POOL == 'Y', 1, 0)
cleaned_df$POOL <- as.numeric(as.character(cleaned_df$POOL))
numeric_columns <- c("SALE AMT", "premium_increase", "YEAR BUILT", "POOL", "distance_beach", "distance_golf", "TOTAL SQ FEET", "ACRES", "NO OF STORIES", "MILLAGE")
cor_matrix <- cor(cleaned_df[, numeric_columns], use="complete.obs")  # use="complete.obs" excludes rows with NAs
print(cor_matrix)

#Correlation matrix looks good, highest one is .72 correlation between square feet and sales price and -.69 correlation between premium increase and distance_beach

#VIF check
library(car)

vif(logmodel9)

#VIF looks good, everything under 10
summary(logmodel9)
names(cleaned_df)

#double checking that log model 9 has the correct variables:
logmodel10 <- lm(log(`SALE AMT`) ~ premium_increase + `YEAR BUILT` + POOL + distance_beach + distance_golf + `TOTAL SQ FEET` + ACRES + `NO OF STORIES` + MILLAGE + `SITE CITY_CHOKOLOSKEE` + `SITE CITY_EVERGLADES CITY` + `SITE CITY_GOODLAND` + `SITE CITY_IMMOKALEE` + `SITE CITY_MARCO ISLAND`, data=cleaned_df) 
summary(logmodel10)

#converting beach and golf from feet to miles
cleaned_df$miles_to_beach <- cleaned_df$distance_beach/5280
View(cleaned_df$`NO OF STORIES`)
cleaned_df$miles_to_golf <- cleaned_df$distance_golf/5280
logmodel10 <- lm(log(`SALE AMT`) ~ premium_increase + `YEAR BUILT` + POOL + miles_to_beach + miles_to_golf + `TOTAL SQ FEET` + ACRES + `NO OF STORIES` + MILLAGE + `SITE CITY_CHOKOLOSKEE` + `SITE CITY_EVERGLADES CITY` + `SITE CITY_GOODLAND` + `SITE CITY_IMMOKALEE` + `SITE CITY_MARCO ISLAND`, data=cleaned_df) 
summary(logmodel10)

#check residuals
hist(residuals(logmodel10))
qqnorm(residuals(logmodel10))
qqline(residuals(logmodel10))
#check to see what it looks like in lin-lin form
model10 <- lm(`SALE AMT` ~ premium_increase + `YEAR BUILT` + POOL + miles_to_beach + miles_to_golf + `TOTAL SQ FEET` + ACRES + `NO OF STORIES` + MILLAGE + `SITE CITY_CHOKOLOSKEE` + `SITE CITY_EVERGLADES CITY` + `SITE CITY_GOODLAND` + `SITE CITY_IMMOKALEE` + `SITE CITY_MARCO ISLAND`, data=cleaned_df) 
summary(model10)
plot(cleaned_df$premium_increase,cleaned_df$`SALE AMT`)
scatterplot(cleaned_df$premium_increase,cleaned_df$`SALE AMT`)
#why is no of stories negative? let's see how it looks without that
logmodel11 <- lm(log(`SALE AMT`) ~ premium_increase + `YEAR BUILT` + POOL + miles_to_beach + miles_to_golf + `TOTAL SQ FEET` + ACRES  + MILLAGE + `SITE CITY_CHOKOLOSKEE` + `SITE CITY_EVERGLADES CITY` + `SITE CITY_GOODLAND` + `SITE CITY_IMMOKALEE` + `SITE CITY_MARCO ISLAND`, data=cleaned_df)
summary(logmodel11)
hist(cleaned_df$`NO OF STORIES`)
scatterplot(cleaned_df$`SALE AMT`, cleaned_df$`NO OF STORIES`)
scatterplot(cleaned_df$`NO OF STORIES`, cleaned_df$`SALE AMT`)
hist(cleaned_df$premium_increase)

#Just realized I've been using proportional values for premium increase not percent change. I think I need to load a new set of values in for those
# Assuming your original dataframe is named cleaned_df
# Create a new dataframe with replaced premium_increase values
cleaned_df2 <- cleaned_df

# List of SITEZIP and corresponding premium_increase values
replacement_values <- data.frame(
  SITEZIP = c(34142, 34137, 34141, 34120, 34119, 34139, 34116, 34117, 34108, 34104, 
              34109, 34140, 34112, 34134, 34102, 34103, 34105, 34138, 34110, 34145, 
              34113, 34114),
  premium_increase = c(0.598387097, 1.055075594, 1.055075594, 1.210134128, 1.379679144, 
                       1.407307172, 1.758573388, 1.860869565, 1.967741935, 2.331818182, 
                       2.381648936, 2.446547315, 2.574600355, 2.602393617, 2.620736086, 
                       2.671814672, 2.918246445, 3.302901024, 3.328320802, 3.717676452, 
                       3.977428852, 4.097922849)
)

# Replace premium_increase values based on SITEZIP
cleaned_df2$premium_increase <- ifelse(cleaned_df2$SITEZIP %in% replacement_values$SITEZIP,
                                       replacement_values$premium_increase,
                                       cleaned_df2$premium_increase)

# Print the new dataframe
cleaned_df2$premium_increase <- ifelse(cleaned_df2$SITEZIP %in% replacement_values$SITEZIP,
                                       replacement_values$premium_increase,
                                       new_premium_df$premium_increase)
# Assuming your original dataframe is named cleaned_df2
# List of SITEZIP and corresponding premium_increase values
replacement_values <- data.frame(
  SITEZIP = c(34142, 34137, 34141, 34120, 34119, 34139, 34116, 34117, 34108, 34104, 
              34109, 34140, 34112, 34134, 34102, 34103, 34105, 34138, 34110, 34145, 
              34113, 34114),
  premium_increase = c(0.598387097, 1.055075594, 1.055075594, 1.210134128, 1.379679144, 
                       1.407307172, 1.758573388, 1.860869565, 1.967741935, 2.331818182, 
                       2.381648936, 2.446547315, 2.574600355, 2.602393617, 2.620736086, 
                       2.671814672, 2.918246445, 3.302901024, 3.328320802, 3.717676452, 
                       3.977428852, 4.097922849)
)

# Merge to replace premium_increase values based on SITEZIP
cleaned_df2 <- merge(cleaned_df2, replacement_values, by = "SITEZIP", all.x = TRUE)

# Use the premium_increase values from replacement_values, or keep the original if no match
cleaned_df2$premium_increase <- ifelse(!is.na(cleaned_df2$premium_increase.y), 
                                       cleaned_df2$premium_increase.y, 
                                       cleaned_df2$premium_increase.x)

# Drop the unnecessary columns from the merge
cleaned_df2 <- cleaned_df2[, -c("premium_increase.x", "premium_increase.y")]

# Print the updated dataframe
print(cleaned_df2)

cleaned_df2 <- merge(cleaned_df2, replacement_values, by = "SITEZIP", all.x = TRUE)



cleaned_df2 <- cleaned_df2 %>%
  left_join(replacement_values, by = "SITEZIP") %>%
  mutate(premium_increase = coalesce(premium_increase.y, premium_increase.x)) %>%
  select(-premium_increase.x, -premium_increase.y)

replacement_values <- data.frame(
  'SITE ZIP' = c(34142, 34137, 34141, 34120, 34119, 34139, 34116, 34117, 34108, 34104, 
              34109, 34140, 34112, 34134, 34102, 34103, 34105, 34138, 34110, 34145, 
              34113, 34114),
  premium_increase = c(0.598387097, 1.055075594, 1.055075594, 1.210134128, 1.379679144, 
                       1.407307172, 1.758573388, 1.860869565, 1.967741935, 2.331818182, 
                       2.381648936, 2.446547315, 2.574600355, 2.602393617, 2.620736086, 
                       2.671814672, 2.918246445, 3.302901024, 3.328320802, 3.717676452, 
                       3.977428852, 4.097922849)
)

# Perform the lookup and replacement
cleaned_df2$premium_increase <- replacement_values$premium_increase[match(cleaned_df2$'SITE ZIP', replacement_values$'SITE.ZIP')]

# Print the updated dataframe
print(cleaned_df2$premium_increase)
unique(cleaned_df2$premium_increase)
sort(unique(cleaned_df2$`SITE ZIP`))
sort(unique(cleaned_df2$premium_increase))

logmodel11 <- lm(log(`SALE AMT`) ~ log(premium_increase) + `YEAR BUILT` + POOL + miles_to_beach + miles_to_golf + `TOTAL SQ FEET` + ACRES + `NO OF STORIES` + MILLAGE + `SITE CITY_CHOKOLOSKEE` + `SITE CITY_EVERGLADES CITY` + `SITE CITY_GOODLAND` + `SITE CITY_IMMOKALEE` + `SITE CITY_MARCO ISLAND`, data=cleaned_df2) 
summary(logmodel11)
logmodel11 <- lm(log(`SALE AMT`) ~ premium_increase + `YEAR BUILT` + POOL + miles_to_beach + miles_to_golf + `TOTAL SQ FEET` + ACRES + `NO OF STORIES` + MILLAGE + `SITE CITY_CHOKOLOSKEE` + `SITE CITY_EVERGLADES CITY` + `SITE CITY_GOODLAND` + `SITE CITY_IMMOKALEE` + `SITE CITY_MARCO ISLAND`, data=cleaned_df2) 


qqplot(logmodel11)

#2-29-2024 messed up my pool value again and need to try to fix it
unique(SFH$POOL)
str(SFH$POOL)
cleaned_df$POOL <- SFH$POOL[match(cleaned_df$'PARCELID', SFH$'PARCELID')]
str(cleaned_df$POOL)

summary(logmodel10)
summary(logmodel11)
str(cleaned_df$POOL)
hist(cleaned_df$`NO OF STORIES`)
install.packages("lmtest")
library(lmtest)
bptest(logmodel10)

#Notes from 2-29 meeting with Kirstin Munro
#present OLS and a quantile regression, reviewer will make you do something else
#heteroscedasticity is the problem. this is caused by some other violation, omitted variable bias, which is causing the problem with no of stories
#Land journal

#My central problem is that number of stories is facing the wrong direction. Let's try to fix that.
onestory_df <- cleaned_df[cleaned_df$`NO OF STORIES` ==1, ]
head(onestory_df)
nostory_logmodel11 <- lm(log(`SALE AMT`) ~ premium_increase + `YEAR BUILT` + POOL + miles_to_beach + miles_to_golf + `TOTAL SQ FEET` + ACRES  + MILLAGE + `SITE CITY_CHOKOLOSKEE` + `SITE CITY_EVERGLADES CITY` + `SITE CITY_GOODLAND` + `SITE CITY_IMMOKALEE` + `SITE CITY_MARCO ISLAND`, data=onestory_df)
summary(nostory_logmodel11)
bptest(nostory_logmodel11)
#No luck, the model gets worse this way

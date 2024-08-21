
### ------- Short-term Rental Price prediction (Continuous analysis) --------------------------------

rm(list = ls())
# Set your own directory
setwd("C:/Business Analytics Uni/Second Semester/Forecasting and Predictive Analytics/Project/Austin")
listings <- read.csv("listings_detailed.csv")

head(listings)
str(listings)

# Data Cleaning -----------------------------------------------------------

# Function to get missing values in descending order for esch variable
getMissingValues <- function(data) {
  # Check for missing values in each column and store the result
  missing_values <- colSums(is.na(data))
  
  # Sort the columns by the number of missing values in descending order
  sorted_missing_values <- sort(missing_values, decreasing = TRUE)
  
  # Display the sorted missing values and their corresponding column names
  for (col_name in names(sorted_missing_values)) {
    cat("Column:", col_name, "\tMissing Values:", sorted_missing_values[col_name], "\n")
  }
}

getMissingValues(listings)

# Drop variables with all NA data 
listings <- subset(listings, select = -c(neighbourhood_group_cleansed, bathrooms, calendar_updated, license))

# We consider only the obsv units that are an entire home/apt, not private or shared rooms
listings <- listings[which(listings$room_type == 'Entire home/apt'),]

# We keep only prop_type that make sense
aaa <- c('Entire home', 'Entire condo', 'Entire rental unit', 'Entire guesthouse', 'Entire townhouse', 'Entire guest suite',
         'Entire loft', 'Entire serviced apartment', 'Entire cottage', 'Entire villa', 'Entire vacation home',
         'Entire cabin', 'Entire bungalow')

listings <- listings[listings$property_type %in% aaa, ]

# We remove the remaining missing values
listings <- na.omit(listings)

# We convert neighbourhood_cleansed in a factor
listings$zip_code <- as.factor(listings$neighbourhood_cleansed)


# Drop variables that are not relevant
{
  listings <- subset(listings, select = -c(id, listing_url, scrape_id, last_scraped, host_response_rate, host_acceptance_rate,
                                           name, description, picture_url, neighborhood_overview, host_neighbourhood, host_verifications, 
                                           host_url, host_name, host_about, host_thumbnail_url, host_picture_url, host_response_time,
                                           host_has_profile_pic, minimum_minimum_nights, maximum_minimum_nights, 
                                           minimum_maximum_nights, maximum_maximum_nights, minimum_nights_avg_ntm, 
                                           maximum_nights_avg_ntm, calendar_last_scraped, first_review, last_review, calculated_host_listings_count,
                                           neighbourhood, neighbourhood_cleansed,  zip_code == "78712",
                                           scrape_id, last_scraped, calculated_host_listings_count_private_rooms, calculated_host_listings_count_shared_rooms,
                                           review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, 
                                           review_scores_value, source, host_id, host_since, host_location, amenities, room_type, bathrooms_text, calculated_host_listings_count_entire_homes))
  
  
  
}


# We change the names to the variables
library(dplyr)
{
  listings <- listings %>%
    rename(
      h_superhost = host_is_superhost,
      h_list =host_listings_count, 
      h_list_tot =host_total_listings_count,
      h_id_ver=host_identity_verified,
      lat = latitude,
      long = longitude,
      prop_type = property_type,
      min_nights= minimum_nights,
      max_nights = maximum_nights,
      avlb = has_availability,
      avlb_30 = availability_30,
      avlb_60 = availability_60,
      avlb_90 = availability_90,
      avlb_365 = availability_365,
      tot_rev = number_of_reviews,
      rev_ltm = number_of_reviews_ltm,
      rev_l30d = number_of_reviews_l30d,
      rev_score = review_scores_rating,
      rev_acc = review_scores_accuracy,
      inst_avlb = instant_bookable,
      monthly_rev = reviews_per_month
    )
  
}


# Variable manipulation
# Price variable 
listings$price <- as.numeric(gsub("[$,]", "", listings$price))


# Conversion into binary variables 1 and 0
listings$h_superhost <- as.integer(listings$h_superhost == "t")
listings$h_id_ver <- as.integer(listings$h_id_ver == "t")
listings$avlb <- as.integer(listings$avlb == "t")
listings$inst_avlb <- as.integer(listings$inst_avlb == "t")

# We remove outliers
listings <- listings[listings$price <= 6000 & listings$price > 1, ] 
summary(listings$price)

# Function to split the zip_code into 5 different regions (SE, SW, C, NE, NW)
get_region <- function(zip_code) {
  if (zip_code %in% c(78724, 78725, 78742, 78719, 78747, 78744
                      , 78741)) {
    return("SE")
  } else if (zip_code %in% c(78731, 78730, 78759, 78732, 78726, 78750,
                             78729, 78717, 78734)) {
    return("NW")
  } else if (zip_code %in% c(78724, 78725, 78742, 78719, 78744, 78741)) {
    return("SE")
  } else if (zip_code %in% c(78736, 78733, 78735, 78746, 78704,
                             78737, 78749, 78745, 78739, 78748)) {
    return("SW")
  } else {
    return("C")
  }
}
# Apply the function to create the 'region' column in the dataframe
listings$region <- sapply(listings$zip_code, get_region)

#We remove zip_code variable form the dataset
listings <- subset(listings, select = -zip_code)


# Exploratory Data Analysis -----------------------------------------------
summary(listings$price)

log_price <- log(listings$price)

### --- Box plot Price.
library(ggplot2)
ggplot(listings, aes(log_price))+
  geom_boxplot()+
  labs(title = "Boxplot of log(price)",
       x = "Logarithm of Price")

### ---  Histogram Price
hist(log_price)

### --- Density log Price
plot(density(log_price))

library(MASS)
### --- Fitting distributions (Normal and student's t)
fit.n <- fitdistr(log_price, "normal")
fit.t <- fitdistr(log_price, "t")
par(mfrow=c(1,1))
plot(density(log_price))
x.p <- seq(0, 10,by=0.01)
m.p <- fit.n$estimate[1]
s.p <- fit.n$estimate[2]
lines(x.p, dnorm(x.p,m.p,s.p), col=2)
mydt <- function(x, m, s, df){dt((x-m)/s, df)/s}
m.t <- fit.t$estimate[1]
s.t <- fit.t$estimate[2]
n.t <- fit.t$estimate[3]
lines(x.p, mydt(x.p,m.t,s.t,n.t), col=4)
# Student's t looks to fit better.

### --- Boxplot for the different prop_type
# Filter the dataset to include only prop_type with frequency > 300
prop_types_over_300 <- names(table(listings$prop_type)[table(listings$prop_type) > 300])
listings_filtered <- listings[listings$prop_type %in% prop_types_over_300, ]

# Conditional bloxplot of price to propr_type
ggplot(listings_filtered, aes(x=prop_type, y=log(price))) +
  geom_boxplot()



### --- Boxplot logprice and reviews
# Define the breaks for the bins
breaks <- c(0, 3, 4, 4.5, 5)  

# Create bins for monthly_rev
data <- listings[,]
data$reviews_bin <- cut(data$rev_score, breaks = breaks, labels = c("0-3", "3-4", "4-4.5", "4.5-5"))

# Create the boxplot of log(price) conditioned on monthly_rev bins
ggplot(data, aes(x=reviews_bin, y=log(price))) +
  geom_boxplot()

boxplot(log(price) ~ reviews_bin, data )

### --- Correlation matrix
library(corrplot)
library(RColorBrewer)
library(dplyr)
numeric_columns <- select_if(listings, is.numeric)
cor_matrix <- cor(numeric_columns)
corrplot(cor_matrix, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#  or
library(ggplot2)

# COrr matrix only for the numeric variables
numeric_columns <- select_if(listings, is.numeric)
cor_matrix <- cor(numeric_columns)

# cor matrix into datafram
cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Variable1", "Variable2", "Correlation")

# Plot of cor matrix
ggplot(cor_df, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  coord_fixed()

library(ggcorrplot)
ggcorrplot(cor_matrix)+
  labs(title = "Airbnb Correlogram")+
  theme(plot.title = element_text(hjust = 0.5))



# Modelling -----------------------------------------------------
set.seed(123)

index <- sample(1:nrow(listings), 0.5*nrow(listings))
train_data <- listings[index,]
test_data <- listings[-index, ]

## --- Binscatter

{'
  library(binsreg)
  # h_list
  plot(train_data$h_list, log(train_data$price))
  #con log(h_list)
  plot(log(train_data$h_list), log(train_data$price))
  
  filt_train <- train_data[which(train_data$h_list < 800),]
  plot(filt_train$h_list, log(filt_train$price))
  binsreg(log(train_data$price), train_data$h_list, binsmethod = "rot")
  
  # h_list_tot
  plot(train_data$h_list_tot, log(train_data$price))
  binsreg(log(train_data$price), train_data$h_list_tot, binsmethod = "rot")
  
  #h_id_ver
  plot(train_data$h_id_ver, log(train_data$price))
  binsreg(log(train_data$price), train_data$h_id_ver, binsmethod = "rot")
  
  # lat
  plot(train_data$lat, log(train_data$price))
  binsreg(log(train_data$price), train_data$lat, binsmethod = "rot")
  
  # long
  plot(train_data$long, log(train_data$price))
  binsreg(log(train_data$price), train_data$long, binsmethod = "rot")
  
  # accommodates
  plot(train_data$accommodates, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$accommodates, binsmethod = "rot")
  
  # bedrooms
  plot(train_data$bedrooms, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$bedrooms, binsmethod = "rot")
  
  # beds
  plot(train_data$beds, log(train_data$price)) #forse si può prendere qualcosa di diverso che solo lineare
  filt_train <- train_data[which(train_data$beds < 15 ),]
  plot(filt_train$beds, log(filt_train$price)) 
  binsreg(log(train_data$price), train_data$beds, binsmethod = "rot")
  
  # min_nights
  plot(train_data$min_nights, log(train_data$price)) 
  filt_train <- train_data[which(train_data$min_nights < 100 ),]
  plot(filt_train$min_nights, log(filt_train$price)) 
  binsreg(log(train_data$price), train_data$min_nights, binsmethod = "rot")
  
  # max_nights
  plot(train_data$max_nights, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$max_nights, binsmethod = "rot")
  
  # avlb
  plot(train_data$avlb, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$avlb, binsmethod = "rot")
  
  # avlb_30
  plot(train_data$avlb_30, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$avlb_30, binsmethod = "rot")
  
  # avlb_60 #Forse 2nd polynomial
  plot(train_data$avlb_60, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$avlb_60, binsmethod = "rot")
  
  # avlb_90 #Forse 2nd polynomial
  plot(train_data$avlb_90, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$avlb_90, binsmethod = "rot")
  
  # tot_rev
  plot(train_data$tot_rev, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$tot_rev, binsmethod = "rot")
  
  # rev_ltm
  plot(train_data$rev_ltm, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$rev_ltm, binsmethod = "rot")
  
  # rev_l30d
  plot(train_data$rev_l30d, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$rev_l30d, binsmethod = "rot")
  
  # rev_score #predere 2nd polynomial
  plot(train_data$rev_score, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$rev_score, binsmethod = "rot")
  
  # rev_acc #2nd polynomial
  plot(train_data$rev_acc, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$rev_acc, binsmethod = "rot")
  
  # inst_avlb
  plot(train_data$inst_avlb, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$inst_avlb, binsmethod = "rot")
  
  # monthly_rev #2nd polynomial
  plot(train_data$monthly_rev, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$monthly_rev, binsmethod = "rot")
  
  # region
  plot(train_data$region, log(train_data$price)) 
  binsreg(log(train_data$price), train_data$region, binsmethod = "rot")
'
}


# --- Model Selection with subset selection, Backward method. We use AdjR2 and BIC
library(leaps)
bwd.regfit <- regsubsets(log(price) ~ ., data = train_data, nvmax = 20, 
                         method = "backward")

#ADJR2
bwd.summary <- summary(bwd.regfit)
plot(bwd.summary$adjr2, type="l")
max <- which.max(bwd.summary$adjr2)
points(max, bwd.summary$adjr2[max], col = "blue", cex = 2, 
       pch = 20)
#BIC
plot(bwd.summary$bic, type="l")
min <- which.min(bwd.summary$bic)
points(min, bwd.summary$bic[min], col = "blue", cex = 2,
       pch = 20)
# Visualization of the best model
coef(bwd.regfit, 20) # With Adjr2
coef(bwd.regfit, 16) # WIth bic

# The best model looks the complete one, with all the variables of the dataset if we consider adjR2.
# 16 variables if we choose the BIC method

library(dplyr)
library(tidyr)

## Backward method with AdjR2
# Desired levels of prop_type and creation of the dummy variables
desired_levels <- c("Entire home", "Entire loft", "Entire rental unit", 
                    "Entire serviced apartment", "Entire villa")
listings_bwd_adjr2 <- listings %>%
  mutate(prop_type_dummy = ifelse(prop_type %in% desired_levels, prop_type, "Other"))

# Conversion into binary variables
for(level in desired_levels) {
  listings_bwd_adjr2[[level]] <- ifelse(listings_bwd_adjr2$prop_type_dummy == level, 1, 0)
}

train_bwd_adjr2 <- listings_bwd_adjr2[index,]
test_bwd_adjr2 <- listings_bwd_adjr2[-index, ]

lm_bwd_adjr2 <- lm(log(price) ~ lat + accommodates + bedrooms + min_nights + max_nights + 
                     avlb_30 + avlb_60 + avlb_90 + rev_ltm + rev_l30d + rev_score + 
                     monthly_rev + region + `Entire home` + `Entire loft` + `Entire rental unit` + 
                     `Entire serviced apartment` + `Entire villa`, data = train_bwd_adjr2)

# Summary of the model
summary(lm_bwd_adjr2)


##  Backward method with BIC
# Desired levels of prop_type and creation of the dummy variables
desired_levels <- c("Entire home", "Entire rental unit", "Entire villa")
listings_bwd_BIC <- listings %>%
  mutate(prop_type_dummy = ifelse(prop_type %in% desired_levels, prop_type, "Other"))

# Conversion into dummy variables
for(level in desired_levels) {
  listings_bwd_BIC[[level]] <- ifelse(listings_bwd_BIC$prop_type_dummy == level, 1, 0)
}
train_bwd_BIC <- listings_bwd_BIC[index,]
test_bwd_BIC <- listings_bwd_BIC[-index, ]

lm_bwd_BIC <- lm(log(price) ~ lat + bedrooms + min_nights  + 
                   avlb_30 + avlb_60 + rev_ltm + rev_l30d + rev_score + 
                   monthly_rev + region + `Entire home`  + `Entire rental unit` + 
                   `Entire villa`, data = train_bwd_BIC)


summary(lm_bwd_BIC)

# --- 1.Complete Model
lm_complete <- lm(log(price) ~., data = train_data)
summary(lm_complete)

# --- 2.Model only with significant variables of the complete model
desired_levels <- c("Entire guest suite", "Entire rental unit", "Entire villa")
listings_rid <- listings %>%
  mutate(prop_type_dummy = ifelse(prop_type %in% desired_levels, prop_type, "Other"))

# Into binary variables
for(level in desired_levels) {
  listings_rid[[level]] <- ifelse(listings_rid$prop_type_dummy == level, 1, 0)
}
train_rid <- listings_rid[index,]
test_rid <- listings_rid[-index, ]

lm_rid <- lm(log(price) ~ h_list + h_list_tot + h_id_ver + lat +  `Entire guest suite` + 
               `Entire rental unit` + `Entire villa` + 
               accommodates + bedrooms + min_nights + avlb_30 + avlb_60 + 
               rev_ltm + rev_l30d + rev_score + monthly_rev + region, 
             data= train_rid )

summary(lm_rid)

# --- 3. Model with variable transformations 

## Transformed Complete
lm_tra <- lm(log(price) ~. - avlb_60 -avlb_90 - rev_score - 
               rev_acc - monthly_rev + 
               poly(avlb_60, 2) + 
               poly(avlb_90, 2) +
               poly(rev_score, 2) +
               poly(rev_acc, 2) + 
               poly(monthly_rev, 2), 
             data = train_data)

summary(lm_tra)

## Transformed Reduced
desired_levels <- c("Entire guest suite", "Entire rental unit", "Entire villa", "Entire serviced apartment")
listings_rid2 <- listings %>%
  mutate(prop_type_dummy = ifelse(prop_type %in% desired_levels, prop_type, "Other"))

# Into binary variables
for(level in desired_levels) {
  listings_rid2[[level]] <- ifelse(listings_rid2$prop_type_dummy == level, 1, 0)
}
train_rid2 <- listings_rid2[index,]
test_rid2 <- listings_rid2[-index, ]

lm_tra_rid <- lm(log(price) ~ h_list + h_list_tot + lat +  `Entire guest suite` + 
                   `Entire rental unit` + `Entire villa` + `Entire serviced apartment` + 
                   accommodates + bedrooms + min_nights + max_nights + avlb + avlb_30 + 
                   avlb_60 + rev_ltm + rev_l30d + rev_acc + region + 
                   poly(rev_score, 2) + poly(monthly_rev, 2), 
                 data= train_rid2 )

summary(lm_tra_rid)


# --- 4. Lasso regression model
# --- Lasso
library(glmnet)
x <- model.matrix(log(price) ~ ., train_data)[,c(-1, -2)]
y <- log(train_data$price)
lasso.mod <- glmnet(x, y, alpha = 1)
plot(lasso.mod, xvar="lambda")
plot(lasso.mod, xvar="dev")

set.seed(2)
cv.lasso <- cv.glmnet(x,y,alpha=1) 
cv.lasso
#NOTE: by default cv.glmnet performs 10-fold cross validation.
#If you want to change it you can use the argument nfolds.
plot(cv.lasso)
coef(cv.lasso)

#The coef function has lambda.1se as default.
desired_levels <- c("Entire guest suite", "Entire home", "Entire loft",
                    "Entire rental unit", "Entire villa", "Entire serviced apartment")
listings_lasso <- listings %>%
  mutate(prop_type_dummy = ifelse(prop_type %in% desired_levels, prop_type, "Other"),
         region_SE = ifelse(region == "SE", 1, 0))


# Into binary variable
for(level in desired_levels) {
  listings_lasso[[level]] <- ifelse(listings_lasso$prop_type_dummy == level, 1, 0)
}
train_lasso <- listings_lasso[index,]
test_lasso <- listings_lasso[-index, ]

lm_lasso <- lm(log(price) ~ lat + long + `Entire guest suite` + `Entire home` + 
                 `Entire loft` + `Entire rental unit` + `Entire villa` + 
                 `Entire serviced apartment` +
                 accommodates + bedrooms + beds+
                 min_nights + max_nights + avlb_30 + rev_l30d +
                 rev_score + monthly_rev + `region_SE`, 
               data = train_lasso)

summary(lm_lasso)

## ---- Model Evaluation test_data on the log scale
# We are interested in the evaluation on the original scale. (Analysis is below)

'
# Function to calculate evaluation metrics
eval_log <- function(model, data) {
  predictions <- predict(model, newdata = data)
  actuals <- log(data$price)
  mse <- mean((predictions - actuals)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(predictions - actuals))
  aic_value <- AIC(model)
  bic_value <- BIC(model)
  adjr2 <- summary(model)$adj.r.squared
  
  return(list(MSE = mse, RMSE = rmse, MAE = mae, AIC = aic_value, BIC = bic_value, AdjR2 = adjr2))}

# Evaluate each model
eval_log_complete <- eval_log(lm_complete, test_data)
eval_log_bwd_adjr2 <- eval_log(lm_bwd_adjr2, test_bwd_adjr2)
eval_log_bwd_BIC <- eval_log(lm_bwd_BIC, test_bwd_BIC)
eval_log_rid <- eval_log(lm_rid, test_rid)
eval_log_tra <- eval_log(lm_tra, test_data)
eval_log_tra_rid <- eval_log(lm_tra_rid, test_rid2)
eval_log_lasso <- eval_log(lm_lasso, test_lasso)

# Combine the results into a data frame for comparison
eval_log_results <- data.frame(
  Model = c("Complete", "BWD AdjR2", "BWD BIC", "Reduced", "Transformed Complete", "Transformed Reduced", "Lasso"),
  MSE = c(eval_log_complete$MSE, eval_log_bwd_adjr2$MSE, eval_log_bwd_BIC$MSE, 
          eval_log_rid$MSE, eval_log_tra$MSE, eval_log_tra_rid$MSE, eval_log_lasso$MSE),
  RMSE = c(eval_log_complete$RMSE, eval_log_bwd_adjr2$RMSE, eval_log_bwd_BIC$RMSE, 
           eval_log_rid$RMSE, eval_log_tra$RMSE, eval_log_tra_rid$RMSE, eval_log_lasso$RMSE),
  MAE = c(eval_log_complete$MAE, eval_log_bwd_adjr2$MAE, eval_log_bwd_BIC$MAE, 
          eval_log_rid$MAE, eval_log_tra$MAE, eval_log_tra_rid$MAE, eval_log_lasso$MAE),
  AIC = c(eval_log_complete$AIC, eval_log_bwd_adjr2$AIC, eval_log_bwd_BIC$AIC, 
          eval_log_rid$AIC, eval_log_tra$AIC, eval_log_tra_rid$AIC, eval_log_lasso$AIC),
  BIC = c(eval_log_complete$BIC, eval_log_bwd_adjr2$BIC, eval_log_bwd_BIC$BIC, 
          eval_log_rid$BIC, eval_log_tra$BIC, eval_log_tra_rid$BIC, eval_log_lasso$BIC),
  AdjR2 = c(eval_log_complete$AdjR2, eval_log_bwd_adjr2$AdjR2, eval_log_bwd_BIC$AdjR2, 
            eval_log_rid$AdjR2, eval_log_tra$AdjR2, eval_log_tra_rid$AdjR2, eval_log_lasso$AdjR2)
)

eval_log_results

# Predictions
pred_complete_log <- predict(lm_complete, test_data)
pred_lasso_log <- predict(lm_lasso, test_lasso)
pred_tra_log <- predict(lm_tra, test_data)
pred_tra_rid_log <- predict(lm_tra_rid, test_rid2)

# Errors
err_complete_log <- log(test_data$price) - pred_complete_log
err_lasso_log <- log(test_lasso$price) - pred_lasso_log
err_tra_log <- log(test_data$price) - pred_tra_log
err_tra_rid_log <- log(test_rid2$price) - pred_tra_rid_log

# Diebold-Mariano test 
# 2sided. Rejection of null hp
# Model 1 is preferred when tn is negative
# Model 2 is preferred when tn is positive

library(forecast)
# Between complete and lasso
dm.test(err_complete_log, err_lasso_log, alternative = "two.sided")

# Between complete and transformed reduce
dm.test(err_complete_log, err_tra_rid_log, alternative = "two.sided")

# Between complete and transformed 
dm.test(err_complete_log, err_tra_log, alternative = "two.sided")

# Between transformed and transformed reduce
dm.test(err_tra_log, err_tra_rid_log, alternative = "two.sided")


#Plots
plot(lm_complete)
plot(lm_lasso)
plot(lm_tra)
plot(lm_tra_rid)
'

# Model Evaluation on the original scale
# If we get the exp(log(price)) we evaluate the performance of predicting the price on the original scale
evaluate_model <- function(model, data) {
  predictions <- exp(predict(model, newdata = data))
  actuals <- data$price
  mse <- mean((predictions - actuals)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(predictions - actuals))
  aic_value <- AIC(model)
  bic_value <- BIC(model)
  adjr2 <- summary(model)$adj.r.squared
  
  return(list(MSE = mse, RMSE = rmse, MAE = mae, AIC = aic_value, BIC = bic_value, AdjR2 = adjr2))
}



# Evaluate each model
eval_complete <- evaluate_model(lm_complete, test_data)
eval_bwd_adjr2 <- evaluate_model(lm_bwd_adjr2, test_bwd_adjr2)
eval_bwd_BIC <- evaluate_model(lm_bwd_BIC, test_bwd_BIC)
eval_rid <- evaluate_model(lm_rid, test_rid)
eval_tra <- evaluate_model(lm_tra, test_data)
eval_tra_rid <- evaluate_model(lm_tra_rid, test_rid2)
eval_lasso <- evaluate_model(lm_lasso, test_lasso)

# Combine the results into a data frame for comparison
eval_results <- data.frame(
  Model = c("Complete", "BWD AdjR2", "BWD BIC", "Reduced", "Transformed Complete", "Transformed Reduced", "Lasso"),
  MSE = c(eval_complete$MSE, eval_bwd_adjr2$MSE, eval_bwd_BIC$MSE, 
          eval_rid$MSE, eval_tra$MSE, eval_tra_rid$MSE, eval_lasso$MSE),
  RMSE = c(eval_complete$RMSE, eval_bwd_adjr2$RMSE, eval_bwd_BIC$RMSE, 
           eval_rid$RMSE, eval_tra$RMSE, eval_tra_rid$RMSE, eval_lasso$RMSE),
  MAE = c(eval_complete$MAE, eval_bwd_adjr2$MAE, eval_bwd_BIC$MAE, 
          eval_rid$MAE, eval_tra$MAE, eval_tra_rid$MAE, eval_lasso$MAE),
  AIC = c(eval_complete$AIC, eval_bwd_adjr2$AIC, eval_bwd_BIC$AIC, 
          eval_rid$AIC, eval_tra$AIC, eval_tra_rid$AIC, eval_lasso$AIC),
  BIC = c(eval_complete$BIC, eval_bwd_adjr2$BIC, eval_bwd_BIC$BIC, 
          eval_rid$BIC, eval_tra$BIC, eval_tra_rid$BIC, eval_lasso$BIC),
  AdjR2 = c(eval_complete$AdjR2, eval_bwd_adjr2$AdjR2, eval_bwd_BIC$AdjR2, 
            eval_rid$AdjR2, eval_tra$AdjR2, eval_tra_rid$AdjR2, eval_lasso$AdjR2)
)

eval_results

# Predictions
pred_tra <- exp(predict(lm_tra, test_data))
pred_tra_rid <- exp(predict(lm_tra_rid, test_rid2))
pred_complete <- exp(predict(lm_complete, test_data))
pred_lasso <- exp(predict(lm_lasso, test_lasso))

# Errors
err_complete <- test_data$price - pred_complete
err_lasso <- test_lasso$price - pred_lasso
err_tra <- test_data$price - pred_tra
err_tra_rid <- test_rid2$price - pred_tra_rid

# Diebold-Mariano test 
# 2sided. Rejection of null hp
# Model 1 is preferred when tn is negative
# Model 2 is preferred when tn is positive

library(forecast)
# Between complete and lasso
dm.test(err_complete, err_lasso, alternative = "two.sided")

# Between complete and transformed reduce
dm.test(err_complete, err_tra_rid, alternative = "two.sided")

# Between complete and transformed 
dm.test(err_complete, err_tra, alternative = "two.sided")

# Between transformed and transformed reduce
dm.test(err_tra, err_tra_rid, alternative = "two.sided")

# Between Compleye and transformed reduce
dm.test(err_complete, err_tra_rid, alternative = "two.sided")


# Let's analyze why we have difference in the MSE values. In the log scale the 
# Transformed Complete has the lowest among the 4 chosen models, instead in the 
# original scale the Transformed Complete has the highest MSE. 
# Let's see the errors summary for the two situations
'
# Log scale
summary(err_complete_log)
summary(err_lasso_log)
summary(err_tra_log)
summary(err_tra_rid_log)
'

# Original scale
summary(err_complete)
summary(err_lasso)
summary(err_tra) # the Min value here is much smaller of the others. If in calculating
# the MSE we get the square of this we understand why we have that difference in the MSE values.
summary(err_tra_rid)




### ------- Will the Property be Sold within Two Months? (Classification analysis) -------
rm(list = ls())
setwd("C:/Business Analytics Uni/Second Semester/Forecasting and Predictive Analytics/Project/Austin")
library(foreign)

# Load the dataset. (It might take a while. Do not panic)
data_2017 <- read.csv("data_2017.csv")

# Data Cleaning -----------------------------------------------------------
getMissingValues <- function(data) {
  # Check for missing values in each column and store the result
  missing_values <- colSums(is.na(data))
  
  # Sort the columns by the number of missing values in descending order
  sorted_missing_values <- sort(missing_values, decreasing = TRUE)
  
  # Display the sorted missing values and their corresponding column names
  for (col_name in names(sorted_missing_values)) {
    cat("Column:", col_name, "\tMissing Values:", sorted_missing_values[col_name], "\n")
  }
}

getMissingValues(data_2017)

data_2017 <- data_2017[!is.na(data_2017$Days_on_Market_), ]
data_2017$Ends_With_TX <- gsub(".*TX$", TRUE, data_2017$ZipName)
data_2017$Ends_With_TX <- ifelse(data_2017$Ends_With_TX == "TRUE", TRUE, FALSE)
data_2017 <- data_2017[which(data_2017$Ends_With_TX == "TRUE"),]

# Remove not necessary variables
data_2017 <- data_2017[, !(names(data_2017) %in% c("X","Month", "ZipCode", "ZipName", "Footnote", "Price_Increase_Count_Y", "Price_Increase_Count_M", "Ends_With_TX"))]
data_2017 <- na.omit(data_2017)

# We change the names to the variables
library(dplyr)
{
  data_2017 <- data_2017 %>%
    rename( 
      med_price = Median_Listing_Price,
      med_price_M = Median_Listing_Price_M,
      med_price_Y = Median_Listing_Price_Y,
      act_count = Active_Listing_Count_,
      act_count_M = Active_Listing_Count_M,
      act_count_Y = Active_Listing_Count_Y,
      d_market = Days_on_Market_,
      d_market_M = Days_on_Market_M,
      d_market_Y = Days_on_Market_Y,
      new_count = New_Listing_Count_,
      new_count_M = New_Listing_Count_M,
      new_count_Y = New_Listing_Count_Y,
      price_incr = Price_Increase_Count_,
      price_decr = Price_Decrease_Count_,
      price_decr_M = Price_Decrease_Count_M,
      price_decr_Y = Price_Decrease_Count_Y,
      pend = Pending_Listing_Count_,
      pend_M = Pending_Listing_Count_M,
      pend_Y = Pending_Listing_Count_Y,
      avg_price = Avg_Listing_Price,
      avg_price_M = Avg_Listing_Price_M,
      avg_price_Y = Avg_Listing_Price_Y,
      tot_list = Total_Listing_Count,
      tot_list_M = Total_Listing_Count_M,
      tot_list_Y = Total_Listing_Count_Y,
      pend_ratio = Pending_Ratio,
      pend_ratio_M = Pending_Ratio_M,
      pend_ratio_Y = Pending_Ratio_Y
    )
  
}

#  Target variable: Sold_2m. Sold_2m = 1 if the property was in the market for less than 60 days 
data_2017$sold_2m <- ifelse(data_2017$d_market <= 60, 1, 0)
table(data_2017$sold_2m)/nrow(data_2017)*100


# Exploratory Data Analysis -----------------------------------------------
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)

#Proportion of properties sold within 90 days for the different median Listing Price
library(readr)
{
  format_labels <- function(x) {
    labels <- as.character(x)
    labels <- gsub("^\\(", "", labels)
    labels <- gsub(",", "k", labels)
    labels <- gsub("\\]", "", labels)
    labels <- readr::parse_number(labels)  
    labels <- scales::number(labels, scale = 1e-3, suffix = "k", big.mark = ",")  # Formatta come "k"
    return(labels)
  }
  
  
  # Creation of the intervals
  intervals <- cut(data_2017$med_price, 
                   breaks = quantile(data_2017$med_price, probs = seq(0, 1, by = 0.1)), 
                   include.lowest = TRUE)
  
  # Plot with personalized  intervals
  ggplot(data_2017, aes(x = intervals, fill = factor(sold_2m))) +
    geom_bar(position = "fill") +
    labs(x = "Median Listing Price (binned)", y = "Proportion of Sold within 90 days") +
    scale_fill_discrete(name = "Sold within 90 days", labels = c("No", "Yes")) +
    scale_x_discrete(labels = format_labels) +
    theme_minimal()
  
}

#Count of sold_2m = 1 and sold_2m = 0
library(ggplot2)

ggplot(data_2017, aes(x = factor(sold_2m)))+
  geom_bar(fill = "skyblue") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")
labs(x = "Sold within 2 months", y = "Count") +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme_minimal()

# Boxplot of the variable log(avg_price) with respect to the target var sold_2m
ggplot(data_2017, aes(x = log(avg_price), fill = factor(sold_2m))) +
  geom_boxplot() +
  labs(x = "Logarithm of Avg_listing Price", y = "Value", fill = "Sold within 3 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Correlation Matrix
matr <- cor(data_2017)
cor_df <- as.data.frame(as.table(matr))
ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  coord_fixed()


# Modelling ----------------------------------------------------------------
# Train and test set
set.seed(123)

index <- sample(1:nrow(data_2017), 0.5*nrow(data_2017))
train_data <- data_2017[index,]
test_data <- data_2017[-index, ]

# --- Complete Model
numeric_vars <- sapply(train_data, is.numeric)
numeric_data <- train_data[, numeric_vars]

# Histograms of continuous variables. (Scroll between plots)
par(mfrow=c(2,2)) 
for (var in names(numeric_data)) {
  hist(numeric_data[[var]], main=var, xlab=var)
}

dev.off()
# ------- COMPLETE
glm_complete <- glm(sold_2m ~ med_price +med_price_M+med_price_Y+ 
                      act_count + act_count_M+ act_count_Y+
                      d_market_M + d_market_Y+
                      new_count_M+ new_count_Y +
                      price_incr +
                      price_decr + price_decr_M + price_decr_Y+
                      pend+pend_M + pend_Y + 
                      avg_price + avg_price_M+ avg_price_Y+
                      tot_list_M+tot_list_Y+
                      pend_ratio+pend_ratio_M+pend_ratio_Y,
                    data = train_data, family = binomial)

summary(glm_complete)

# ------- Reduced 1
# Model only with significant variables of the complete one.
glm_rid <- glm(sold_2m ~ act_count + d_market_M + d_market_Y + new_count_M + 
                 price_decr+
                 price_decr_Y + pend_ratio + pend_ratio_Y,
               data = train_data, family = binomial)
summary(glm_rid)

# ------- Reduced 2 Best subset selection
# Correlation matrix
cor_matrix <- cor(train_data)

library(caret)
library(leaps)
# Var with corr > 0.99
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.98)

# Remove highly correlated variables
train_data_reduced <- train_data[, -highly_correlated]

best.regfit <- regsubsets(sold_2m ~ ., data = train_data_reduced)

best.summary <- summary(best.regfit)

#BIC
plot(best.summary$bic, type="l")
min <- which.min(best.summary$bic)
points(min, best.summary$bic[min], col = "blue", cex = 2,
       pch = 20)
coef(best.regfit, min) # With bic

glm_rid_best <- glm(sold_2m ~ d_market + price_decr  + tot_list + tot_list_M + new_count+
                      new_count_M + pend_ratio ,
                    data = train_data_reduced)
summary(glm_rid_best)



# ------- Transformed complete
# If we apply only the log of the variable we obtain 
log_modulo <- function(x) {
  sign(x) * log(abs(x) + 1)
}
set.seed(123)



#We apply the log_modulo function to the var we want to transform
{ data_2017_tra <- data_2017[,]
  data_2017_tra$log_med_price <- log_modulo(data_2017_tra$med_price)
  data_2017_tra$log_price_decr <- log_modulo(data_2017_tra$price_decr)
  data_2017_tra$log_new_count <- log_modulo(data_2017_tra$new_count)
  data_2017_tra$log_new_count_M <- log_modulo(data_2017_tra$new_count_M)
  data_2017_tra$log_price_incr <- log_modulo(data_2017_tra$price_incr)
  data_2017_tra$log_pend <- log_modulo(data_2017_tra$pend)
  data_2017_tra$log_avg_price <- log_modulo(data_2017_tra$avg_price)
}

index <- sample(1:nrow(data_2017), 0.5*nrow(data_2017))
train_data_tra <- data_2017_tra[index,]
test_data_tra <- data_2017_tra[-index, ]


glm_tra1 <- glm(sold_2m ~ log_med_price + I(log_price_decr^2) +
                  log_new_count + log_new_count_M + log_avg_price+ 
                  log_pend + 
                  med_price + med_price_M + med_price_Y + 
                  act_count + act_count_M + act_count_Y +
                  d_market_M + d_market_Y +
                  new_count_M + new_count_Y +
                  price_decr + price_decr_M + price_decr_Y +
                  log_pend + pend_M + pend_Y + 
                  log_avg_price + avg_price_M + avg_price_Y +
                  tot_list_M + tot_list_Y +
                  pend_ratio + pend_ratio_M + pend_ratio_Y,
                data = train_data_tra, family = binomial)

summary(glm_tra1)

# ------- Transformed reduced 
glm_tra2 <- glm(sold_2m ~ log_med_price + I(log_price_decr^2) +
                  log_new_count + log_new_count_M + 
                  log_pend + act_count + act_count_M +
                  d_market_M + d_market_Y + new_count_M +
                  price_decr + log_pend + pend_M + 
                  log_avg_price + avg_price_Y +
                  pend_ratio +pend_ratio_Y,
                data = train_data_tra, family = binomial)

summary(glm_tra2)


### --- Model Evaluation
p_complete.oos <- predict(glm_complete, newdata = test_data, type = "response")
d_complete.oos <- 1*(p_complete.oos>0.5)
p_rid.oos <- predict(glm_rid, newdata = test_data, type = "response")
d_rid.oos <- 1*(p_rid.oos>0.5)
p_rid_best.oos <- predict(glm_rid_best, newdata = test_data, type = "response")
d_rid_best.oos <- 1*(p_rid_best.oos>0.5)
p_tra1.oos <- predict(glm_tra1, newdata = test_data_tra, type = "response")
d_tra1.oos <- 1*(p_tra1.oos>0.5)
p_tra2.oos <- predict(glm_tra2, newdata = test_data_tra, type = "response")
d_tra2.oos <- 1*(p_tra2.oos>0.5)

# MSE on the test set
mse_complete <- mean((p_complete.oos-test_data$sold_2m)^2)
mse_rid <- mean((p_rid.oos-test_data$sold_2m)^2)
mse_rid_best <- mean((p_rid_best.oos-test_data$sold_2m)^2)
mse_tra1 <- mean((p_tra1.oos-test_data_tra$sold_2m)^2)
mse_tra2 <- mean((p_tra2.oos-test_data_tra$sold_2m)^2)

cbind(mse_complete, mse_tra1, mse_tra2)

# Hosmer-Lemeshow test
library(ResourceSelection) # Contains HL test
library(ROCR) # Contains ROC curve function

HL_complete <- hoslem.test(test_data$sold_2m, p_complete.oos)
HL_rid <- hoslem.test(test_data$sold_2m, p_rid.oos)
HL_rid_best <- hoslem.test(test_data$sold_2m, p_rid_best.oos)
HL_tra1 <- hoslem.test(test_data_tra$sold_2m, p_tra1.oos)
HL_tra2 <- hoslem.test(test_data_tra$sold_2m, p_tra2.oos)

cbind(HL_complete, HL_rid, HL_rid_best, HL_tra1, HL_tra2)

# Accuracy
cm_complete <- table(test_data$sold_2m, d_complete.oos)
acc_complete <- (cm_complete[1,1] + cm_complete[2,2])/nrow(test_data)

cm_rid <- table(test_data$sold_2m, d_rid.oos)
acc_rid <- (cm_rid[1,1] + cm_rid[2,2])/nrow(test_data)

cm_rid_best <- table(test_data$sold_2m, d_rid_best.oos)
acc_rid_best <- (cm_rid_best[1,1] + cm_rid_best[2,2])/nrow(test_data)

cm_tra1 <- table(test_data_tra$sold_2m, d_tra1.oos)
acc_tra1 <- (cm_tra1[1,1] + cm_tra1[2,2])/nrow(test_data_tra)

cm_tra2 <- table(test_data_tra$sold_2m, d_tra2.oos)
acc_tra2 <- (cm_tra2[1,1] + cm_tra2[2,2])/nrow(test_data_tra)

cbind(acc_complete,acc_tra1, acc_tra2)

######## - ------- ROC curve
library(ROCR)
pred_complete <- prediction(p_complete.oos, test_data$sold_2m)
perf_complete <- performance(pred_complete,"tpr","fpr")

pred_rid <- prediction(p_rid.oos, test_data$sold_2m)
perf_rid <- performance(pred_rid,"tpr","fpr")

pred_rid_best <- prediction(p_rid_best.oos, test_data$sold_2m)
perf_rid_best <- performance(pred_rid_best,"tpr","fpr")

pred_tra1 <- prediction(p_tra1.oos, test_data_tra$sold_2m)
perf_tra1 <- performance(pred_tra1,"tpr","fpr")

pred_tra2 <- prediction(p_tra2.oos, test_data_tra$sold_2m)
perf_tra2 <- performance(pred_tra2,"tpr","fpr")

par(mar=c(4,4,1,1))
plot(perf_complete, lwd=2,xlab="False Positive Rate", ylab = "True Positive Rate", col = 4)
lines(perf_tra1@x.values[[1]],perf_tra1@y.values[[1]], col=2, lwd=2)
lines(perf_tra2@x.values[[1]],perf_tra2@y.values[[1]], col=3, lwd=2)

# Add AUC values as text annotations
text(0.5, 0.55, labels = paste("Complete:", round(performance(pred_complete, "auc")@y.values[[1]], 2)), col = 4)
text(0.5, 0.45, labels = paste("Transformed:", round(performance(pred_tra1, "auc")@y.values[[1]], 2)), col = 2)
text(0.5, 0.35, labels = paste("Transformed red.:", round(performance(pred_tra2, "auc")@y.values[[1]], 2)), col = 3)

legend("bottomright", legend = c("Complete", "Transformed", "Red. Transformed"), col=c(4, 2, 3), lwd=2)

dev.off()



##-------Diebold Mariano test

# Errors
err_complete <- test_data$sold_2m - d_complete.oos
err_tra <- test_data$sold_2m - d_tra1.oos
err_tra_rid <- test_data$sold_2m - d_tra2.oos

# Diebold-Mariano test 
# 2sided. Rejection of null hp
# Model 1 is preferred when tn is negative
# Model 2 is preferred when tn is positive

library(forecast)
# Between complete and transformed reduce
dm.test(err_complete, err_tra_rid, alternative = "two.sided")

# Between complete and transformed 
dm.test(err_complete, err_tra, alternative = "two.sided")

# Between transformed and transformed reduce
dm.test(err_tra, err_tra_rid, alternative = "two.sided")









### ------- Time series Analysis. Forecasting ZHVI --------------------
rm(list = ls())
setwd("C:/Business Analytics Uni/Second Semester/Forecasting and Predictive Analytics/Project/Austin")

library(foreign)

#load the Dataset
data <- read.csv2("data2.csv")

# Data Cleaning -----------------------------------------------------------

getMissingValues <- function(data) {
  # Check for missing values in each column and store the result
  missing_values <- colSums(is.na(data))
  
  # Sort the columns by the number of missing values in descending order
  sorted_missing_values <- sort(missing_values, decreasing = TRUE)
  
  # Display the sorted missing values and their corresponding column names
  for (col_name in names(sorted_missing_values)) {
    cat("Column:", col_name, "\tMissing Values:", sorted_missing_values[col_name], "\n")
  }
}

getMissingValues(data)
# there is no evidence of missing data

# Convert Month variable to date format
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$ZHVI <- as.numeric(data$ZHVI)


# TIME-SERIES ANALYSIS
library(ggfortify)
library(forecast)
library(tseries)

# Exploratory Data Analysis ----

# Boxplot by Year
data$Year <- as.factor(format(data$Date, "%Y"))
library(ggplot2)
ggplot(data, aes(x = Year, y = ZHVI)) +
  geom_boxplot(fill = 4) +
  labs(title = "Yearly Boxplot",
       x = "Year",
       y = "AvgZHVI")



# ts data
price_ts <- ts(data$ZHVI, start = c(1997,1), end = c(2023,12), frequency = 12)
summary(price_ts)
str(price_ts)

# Time series plot
autoplot(price_ts) +
  ggtitle("Monthly ZHVI in Texas") +
  xlab("Year") +
  ylab("ZHVI")


# Seasonal plots
ggseasonplot(price_ts, season.labels=NULL, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("ZHVI") +
  ggtitle("Monthly ZHVI in Texas")
# From the Seasonal plot it does not seem to be strong seasonality.


ggsubseriesplot(price_ts, na.rm=T) +
  ylab("Avg_Price") +
  ggtitle("Average monthly house price in Texas")

# Autocorrelation function
ggAcf(price_ts)
# Analyze the autocorrelation of the data to understand patterns and dependencies
# in different time steps.
# Looking at the plot there could be some trend in data as it is slowly decaying.
# The ts might be non-stationary

'
### Moving Average original ts
ma_5 <- ma(price_ts, 5)
ma_10 <- ma(price_ts, 10)

autoplot(price_ts, series="Data") +
  autolayer(ma_5, series="5-MA") +
  xlab("Year") +
  ylab("Avg_Price") +
  ggtitle("Average monthly house price in Texas") +
  scale_colour_manual(values=c("Data"="blue","5-MA"="red"),
                      breaks=c("Data","5-MA"))

autoplot(price_ts, series="Data") +
  autolayer(ma_10, series="10-MA") +
  xlab("Year") +
  ylab("Avg_Price") +
  ggtitle("Average monthly house price in Texas") +
  scale_colour_manual(values=c("Data"="blue","10-MA"="red"),
                      breaks=c("Data","10-MA"))
'


# Decomposition

# Classical decomposition
price_ts %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Monthly ZHVI in Texas")

# Modern decomposition (stl)
price_ts %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot() + xlab("Year") +
  ggtitle("Monthly ZHVI in Texas")
# There is evidence of an increasing trend
# As for seasonality, prices increase month by month before having a slight decrease by the end of the year.

#Augmented Dickey-Fuller and Phillips-Perron Test
adf.test(price_ts)

pp.test(price_ts)

# As rejected there is no stochastic trend.
# The time series is non-stationary.

# DATA INTEGRATION THROUGH DIFFERENTIATION (Yt' = Yt - Yt-1)
# due to non-costant average and variance
int1_data <- diff(price_ts, differences = 1)
# Test the integrated data using the augmented Dickey-Fuller test
adf.test(int1_data) #p-value = 0.01
# the warning just indicates a highly significant result, no need for action.
ggseasonplot(int1_data, season.labels=NULL, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("ZHVI") +
  ggtitle("Monthly ZHVI in Texas")
# Now there seems to be seasonality in spring time


# The results show that the original series (price_ts) is non-stationary, 
# and after differenciating once, the series (int1_data) becomes stationary. 
# This is a common approach in time series analysis to transform a non-stationary 
# series into a stationary one, making it suitable for further analysis or modelling.

# autoplot(int1_data)


# There is evidence in the trend that it is not constantly increasing,
# as we now have a stationary and integrated series of data.

# DOUBT:
# DO WE NEED TO DO PREVIOUS ANALYSIS AND PLOTS AGAIN (WITH ALSO MA) WITH THE NEW INTEGRATED DATA? FROM LINES 50 TO 104

'
### MA ts integrated
ma_5 <- ma(int1_data, 5)
ma_10 <- ma(int1_data, 10)

autoplot(int1_data, series="Data") +
  autolayer(ma_5, series="5-MA") +
  xlab("Year") +
  ylab("Avg_Price") +
  ggtitle("Average monthly house price in Texas") +
  scale_colour_manual(values=c("Data"="blue","5-MA"="red"),
                      breaks=c("Data","5-MA"))

autoplot(int1_data, series="Data") +
  autolayer(ma_10, series="10-MA") +
  xlab("Year") +
  ylab("Avg_Price") +
  ggtitle("Average monthly house price in Texas") +
  scale_colour_manual(values=c("Data"="blue","10-MA"="red"),
                      breaks=c("Data","10-MA"))

'



# Linear trend regression model
n <- length(price_ts)
t <- 1:n
lr <- lm(price_ts~t)
summary(lr)
# The summary of the results shows a significant trend variable explaining a 
# percentage of the variance in the dependent variable approximately 
# equal to 75%.

# Plot linear trend 
blh <- fitted(lr)
blh.ts <- ts(as.vector(blh), start = c(1997,1), end = c(2023,12), frequency = 12)

autoplot(price_ts) +
  autolayer(blh.ts) +
  ggtitle("Monthly ZHVI in Texas") +
  xlab("Year") +
  ylab("ZHVI")

# As we can see from the plot, the linear trend doesn't capture the trend of data accurately.

# Diagnostic linear trend regression
lr.e <- ts(residuals(lr), start = c(1997,1), end = c(2023,12), frequency = 12)
autoplot(lr.e)
ggAcf(lr.e)

# Let's also consider the impact that seasonality could have in this regression framework. 

# Linear trend + season regression model
n <- length(price_ts)
t <- 1:n
s <- apply(replicate(n/12,diag(12)),1,"c")
lr2 <- lm(price_ts~t+s)
summary(lr2)

# It doesn't seem to perform any better than the previous (trend) model,
# as the seasonality's terms are never significant.
# The R-squared is basically very very similar, around 75% with just a slight increase by some decimal points.

# Plot linear trend + season
blh2 <- fitted(lr2)
blh2.ts <- ts(as.vector(blh2), start = c(1997,1), end = c(2023,12), frequency = 12)

autoplot(blh.ts) +
  autolayer(blh2.ts) +
  ggtitle("Monthly ZHVI in Texas") +
  xlab("Year") +
  ylab("ZHVI")

# Diagnostic linear trend regression + season
lr2.e <- ts(residuals(lr2), start = c(1997,1), end = c(2023,12), frequency = 12)
autoplot(lr2.e)
ggAcf(lr2.e)

# AR 1 on the original ts
# As from previous analysis there seemed to be some seasonality, let's analyse it
# while modelling.
n <- length(price_ts)
lm1 <- lm(price_ts[-1]~price_ts[-n])
summary(lm1)

# R-squared of 0.99, better to do it on the integrated ts

# Plot
lm1_fit <- fitted(lm1)
fit.ts <- ts(as.vector(lm1_fit), start = c(1997,1), end = c(2023,12), frequency = 12)
autoplot(price_ts) +
  autolayer(fit.ts) +
  ggtitle("Monthly ZHVI in Texas") +
  xlab("Year") +
  ylab("ZHVI")

# Seems to be perfectly fitted.

lm1.e <- ts(residuals(lm1), start = c(1997,1), end = c(2023,12), frequency = 12)
autoplot(lm1.e)
ggAcf(lm1.e)

# Can surely be improved.

# Let's add the deterministic trend to the previous model to see if we can improve it.

t <- 2:n
lm2 <- lm(price_ts[-1]~price_ts[-n]+t)  
summary(lm2)

# The trend is not significant, leading to the exact same result that we have seen before.
# It can be omitted in this part of the analysis.

lm2.e <- ts(residuals(lm2), start = c(1997,1), end = c(2023,12), frequency = 12)
autoplot(lm2.e)
ggAcf(lm2.e)

# Also the plot shows that we get the same results as before.

### AR 1 on the int ts
# As from previous analysis there seemed to be some seasonality, let's analyse it
# while modelling.
n <- length(int1_data)
lm1 <- lm(int1_data[-1]~int1_data[-n])
summary(lm1)

# AdjR-squared of 0.3697

# Plot
lm1_fit <- fitted(lm1) 
fit.ts <- ts(as.vector(lm1_fit), start = c(1997,1), end = c(2023,12), frequency = 12)
autoplot(int1_data) +
  autolayer(fit.ts) +
  ggtitle("Monthly ZHVI in Texas") +
  xlab("Year") +
  ylab("ZHVI")


lm1.e <- ts(residuals(lm1), start = c(1997,1), end = c(2023,12), frequency = 12)
autoplot(lm1.e)
ggAcf(lm1.e, lag = 100)

# Can surely be improved.

# Let's add the deterministic trend to the previous model to see if we can improve it.

t <- 2:n
lm2 <- lm(int1_data[-1]~int1_data[-n]+t)  
summary(lm2)

# AdjR2 0.3736
# Trend "." significant

lm2.e <- ts(residuals(lm2), start = c(1997,1), end = c(2023,12), frequency = 12)
autoplot(lm2.e)
ggAcf(lm2.e, lag = 100)


# Modelling  -------------------------------------------------
# Train 80% and test 20%
ix.is <- 1:259
ix.os <- 260:324

#Modelling on Train
#using one lag of ar without moving average model, obv 1 diff
arima110 <- Arima(price_ts[ix.is], order = c(1,1,0))
summary(arima110)
#Using one lag of ar and one lag of moving average model
arima111 <- Arima(price_ts[ix.is], order = c(1,1,1))
summary(arima111)
#Using two lags of ar without moving average model 
arima211 <- Arima(price_ts[ix.is], order = c(2,1,1))
summary(arima211)
#Using two lags of ar and two lags of moving average model
arima212 <- Arima(price_ts[ix.is], order = c(2,1,2))
summary(arima212)



#SARIMA Model with two lags of AR and two lags of MA
# Seasonal 1,1,1 miglior BIC e AIC. Seasonal 2,1,1 miglior RMSE e MAE

sarima211 <-  Arima(price_ts[ix.is], order = c(2, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))
summary(sarima211)

sarima212 <-  Arima(price_ts[ix.is], order = c(2, 1, 2), seasonal = list(order = c(1, 1, 1), period = 12))
summary(sarima212)


v1 <- cbind(arima110$bic,arima111$bic,arima211$bic,arima212$bic, sarima211$bic, sarima212$bic)
v1
order(v1, decreasing = F)
# Now Sarima(2,1,2), Sarima(2,1,1) have the lowest BIC.
# Among the Arima models, Arima(2,1,2) has the lowest BIC, followed by Arima(2,1,1).
# It's important to say and justify that even though it does not seem to be seasonality in the
# ts, SARIMA models are better that ARIMA in terms of BIC. 
# Look at the seasonal plot of the integrated ts and the seasonal plot of the residuals of an ARIMA model


#Let's see the behavior of the residuals of each model
# Arima(1,1,0)
re1=ts(arima110$residuals,start = c(1997,1), end = c(2023,12), frequency = 12)
acf(re1)
autoplot(re1)
ggAcf(re1)
pacf(re1, lag = 50) 
checkresiduals(arima110) #not all of them are in the interval

# Arima(1,1,1)
re2=ts(arima111$residuals, start = c(1997,1), end = c(2023,12), frequency = 12)
acf(re2)
autoplot(re2)
ggAcf(re2)
pacf(re2)
checkresiduals(arima111) #not all of them are in the interval

# Arima(2,1,1)
re3=ts(arima211$residuals, start = c(1997,1), end = c(2023,12), frequency = 12)
acf(re3)
autoplot(re3)
ggAcf(re3)
pacf(re3)
checkresiduals(arima211) #not all of them are in the interval

# Seasonal Plot of the residual Arima211, suggest there is seasonality so we can capture it with SARIMA models, indeed SARIMA performs better
ggseasonplot(re3, season.labels=NULL, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("ZHVI") +
  ggtitle("Monthly ZHVI in Texas")

# Checking seasonality in residuals for Arima(2,2,2)
arima222 <- Arima(price_ts[ix.is], order = c(2,2,2))
summary(arima222)
rearima222=ts(arima222$residuals, start = c(1997,1), end = c(2023,12), frequency = 12)
ggseasonplot(rearima222, season.labels=NULL, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("ZHVI") +
  ggtitle("Monthly ZHVI in Texas")

# Arima(2,1,2)
re4=ts(arima212$residuals, start = c(1997,1), end = c(2023,12), frequency = 12)
acf(re4)
autoplot(re4)
ggAcf(re4)
pacf(re4)
checkresiduals(arima212) #not all of them are in the interval

# Sarima(2,1,1)
re5=ts(sarima211$residuals, start = c(1997,1), end = c(2023,12), frequency = 12)
acf(re5)
autoplot(re5)
ggAcf(re5)
pacf(re5)
checkresiduals(sarima211) #almost all of them are in the interval

# Sarima(2,1,2)
re6=ts(sarima212$residuals, start = c(1997,1), end = c(2023,12), frequency = 12)
acf(re6)
autoplot(re6)
ggAcf(re6)
pacf(re6)
checkresiduals(sarima212) #almost all of them are in the interval

# What if we go with a parameter of 2 for differentiation?
sarima221 <-  Arima(price_ts[ix.is], order = c(2, 2, 1), seasonal = list(order = c(1, 1, 1), period = 12))
summary(sarima221)

sarima222 <-  Arima(price_ts[ix.is], order = c(2, 2, 2), seasonal = list(order = c(1, 1, 1), period = 12))
summary(sarima222)

# in terms of BIC, results are very similar to the ones obtained by differentiating once

# Sarima(2,2,1)
re7=ts(sarima221$residuals, start = c(1997,1), end = c(2023,12), frequency = 12)
acf(re7)
autoplot(re7)
ggAcf(re7)
pacf(re7)
checkresiduals(sarima221) #almost all of them are in the interval, but no significantly different result obtained

# Sarima(2,2,2)
re8=ts(sarima222$residuals, start = c(1997,1), end = c(2023,12), frequency = 12)
acf(re8)
autoplot(re8)
ggAcf(re8)
pacf(re8)
checkresiduals(sarima222) # all of them are in the interval if looking at the pacf

library(forecast)

#Plotting the fit on the train data
#Model 5 Sarima 211
y5m <- fitted(sarima211)
y.ts5m <- ts(as.vector(y5m), start = 1997, frequency = 12)
plot(price_ts)
lines(y.ts5m, col=2)

#Model 3 Arima 211
y3m <- fitted(arima211)
y.ts3m <- ts(as.vector(y3m), start = 1997, frequency = 12)
plot(price_ts)
lines(y.ts3m, col=2)

#Model 4 Arima 212
y4m <- fitted(arima212)
y.ts4m <- ts(as.vector(y4m), start = 1997, frequency = 12)
plot(price_ts)
lines(y.ts4m, col=2)

#Model 6 Sarima 212  
y6m <- fitted(sarima212)
y.ts6m <- ts(as.vector(y6m), start = 1997, frequency = 12)
plot(price_ts)
lines(y.ts6m, col=2)

#Model 7 Sarima 221  
y7m <- fitted(sarima221)
y.ts7m <- ts(as.vector(y7m), start = 1997, frequency = 12)
plot(price_ts)
lines(y.ts7m, col=2)

#Model 8 Sarima 222  
y8m <- fitted(sarima222)
y.ts8m <- ts(as.vector(y8m), start = 1997, frequency = 12)
plot(price_ts)
lines(y.ts8m, col=2)

# Three-step ahead forecast of the models on out-of-sample data:
# Three step, because the agency decides if buy/sell apartments three months after.

#Model 5 Sarima 211
h <- 3
n <- length(ix.os) - h
yh.os5<- rep(NA, n)
for(i in 1:n){
  ix.up  <- c(ix.is,ix.os[1:i])
  fit.up <- Arima(price_ts[ix.up], model=sarima211)
  yh.os5[i] <- forecast(fit.up, h=h)$mean[h]
}
plot(price_ts[ix.os],type="l", ylim = c(148000,230000))
lines(yh.os5, col=2)
mse5=mean((price_ts[ix.os[-c(1:3)]]-yh.os5)^2)
mse5

#Model 3 Arima 211
h <- 3
n <- length(ix.os) - h
yh.os3 <- rep(NA, n)
for(i in 1:n){
  ix.up  <- c(ix.is,ix.os[1:i])
  fit.up <- Arima(price_ts[ix.up], model=arima211)
  yh.os3[i] <- forecast(fit.up, h=h)$mean[h]
}
plot(price_ts[ix.os],type="l", ylim = c(148000,230000))
lines(yh.os3, col=2)
mse3=mean((price_ts[ix.os[-c(1:3)]]-yh.os3)^2)
mse3


#Model 4 Arima 212
h <- 3
n <- length(ix.os) - h
yh.os4 <- rep(NA, n)
for(i in 1:n){
  ix.up  <- c(ix.is,ix.os[1:i])
  fit.up <- Arima(price_ts[ix.up], model=arima212)
  yh.os4[i] <- forecast(fit.up, h=h)$mean[h]
}
plot(price_ts[ix.os],type="l")
lines(yh.os4, col=2)
mse4=mean((price_ts[ix.os[-c(1:3)]]-yh.os4)^2)
mse4


#Model 6 Sarima 212
h <- 3
n <- length(ix.os) - h
yh.os6 <- rep(NA, n)
for(i in 1:n){
  ix.up  <- c(ix.is,ix.os[1:i])
  fit.up <- Arima(price_ts[ix.up], model=sarima212)
  yh.os6[i] <- forecast(fit.up, h=h)$mean[h]
}
plot(price_ts[ix.os],type="l", ylim = c(148000,230000))
lines(yh.os6, col=2)
mse6=mean((price_ts[ix.os[-c(1:3)]]-yh.os6)^2)
mse6

#Model 7 Sarima 221
h <- 3
n <- length(ix.os) - h
yh.os7 <- rep(NA, n)
for(i in 1:n){
  ix.up  <- c(ix.is,ix.os[1:i])
  fit.up <- Arima(price_ts[ix.up], model=sarima221)
  yh.os7[i] <- forecast(fit.up, h=h)$mean[h]
}
plot(price_ts[ix.os],type="l", ylim = c(148000,230000))
lines(yh.os7, col=2)
mse7=mean((price_ts[ix.os[-c(1:3)]]-yh.os7)^2)
mse7

#Model 8 Sarima 222
h <- 3
n <- length(ix.os) - h
yh.os8 <- rep(NA, n)
for(i in 1:n){
  ix.up  <- c(ix.is,ix.os[1:i])
  fit.up <- Arima(price_ts[ix.up], model=sarima222)
  yh.os8[i] <- forecast(fit.up, h=h)$mean[h]
}
plot(price_ts[ix.os],type="l", ylim = c(148000,230000))
lines(yh.os8, col=2)
mse8=mean((price_ts[ix.os[-c(1:3)]]-yh.os8)^2)
mse8

mse_table <- data.frame(
  Model = c("ARIMA(2,1,1)", "ARIMA(2,1,2)", "SARIMA(2,1,1)", "SARIMA(2,1,2)", "SARIMA(2,2,1)", "SARIMA(2,2,2)"),
  MSE = c(mse3, mse4, mse5, mse6, mse7, mse8)
)

# Step 2: Sort the data frame by MSE in decreasing order
mse_table_sorted <- mse_table[order(mse_table$MSE), ]
mse_table_sorted
# Increasing order: Sarima221, Sarima222, Sarima211, Sarima212, Arima211, Arima212

dev.off()

# Plot combined Model 5 SARIMA 211
plot(price_ts, type = "l", col = "black", xlab = "Date", ylab = "ZHVI")
abline(v = time(price_ts)[length(ix.is)], col = "blue", lty = 2) 
lines(time(price_ts)[length(ix.is) + 1:length(yh.os5)], yh.os5, col = "red")
legend("topleft", legend = c("Observed", "Forecasted SARIMA211", "In-Sample/Out-of-Sample"), col = c("black", "red", "blue"), lty = c(1, 1, 2))

# Plot combined Model 3 ARIMA 211
plot(price_ts, type = "l", col = "black", xlab = "Date", ylab = "ZHVI")
abline(v = time(price_ts)[length(ix.is)], col = "blue", lty = 2) 
lines(time(price_ts)[length(ix.is) + 1:length(yh.os3)], yh.os3, col = "red")
legend("topleft", legend = c("Observed", "Forecasted ARIMA211", "In-Sample/Out-of-Sample"), col = c("black", "red", "blue"), lty = c(1, 1, 2))

# Plot combined Model 6 SARIMA 212
plot(price_ts, type = "l", col = "black", xlab = "Date", ylab = "ZHVI")
abline(v = time(price_ts)[length(ix.is)], col = "blue", lty = 2) 
lines(time(price_ts)[length(ix.is) + 1:length(yh.os6)], yh.os6, col = "red")
legend("topleft", legend = c("Observed", "Forecasted SARIMA212", "In-Sample/Out-of-Sample"), col = c("black", "red", "blue"), lty = c(1, 1, 2))

# Plot combined Model 4 ARIMA 212
plot(price_ts, type = "l", col = "black", xlab = "Date", ylab = "ZHVI")
abline(v = time(price_ts)[length(ix.is)], col = "blue", lty = 2) 
lines(time(price_ts)[length(ix.is) + 1:length(yh.os4)], yh.os4, col = "red")
legend("topleft", legend = c("Observed", "Forecasted ARIMA212", "In-Sample/Out-of-Sample"), col = c("black", "red", "blue"), lty = c(1, 1, 2))

# Plot combined Model 7 SARIMA 221
plot(price_ts, type = "l", col = "black", xlab = "Date", ylab = "ZHVI", ylim = c(90000,230000))
abline(v = time(price_ts)[length(ix.is)], col = "blue", lty = 2) 
lines(time(price_ts)[length(ix.is) + 1:length(yh.os7)], yh.os7, col = "red")
legend("topleft", legend = c("Observed", "Forecasted SARIMA221", "In-Sample/Out-of-Sample"), col = c("black", "red", "blue"), lty = c(1, 1, 2))

# Plot combined Model 8 SARIMA 222
plot(price_ts, type = "l", col = "black", xlab = "Date", ylab = "ZHVI", ylim = c(90000,230000))
abline(v = time(price_ts)[length(ix.is)], col = "blue", lty = 2) 
lines(time(price_ts)[length(ix.is) + 1:length(yh.os8)], yh.os8, col = "red")
legend("topleft", legend = c("Observed", "Forecasted SARIMA222", "In-Sample/Out-of-Sample"), col = c("black", "red", "blue"), lty = c(1, 1, 2))
dev.off()

# Plot of future forecasting for the different models --
autoplot(forecast(arima211))
autoplot(forecast(arima212))
autoplot(forecast(sarima211))
autoplot(forecast(sarima212))
autoplot(forecast(sarima221))
autoplot(forecast(sarima222))





library(tidyverse)
library(pROC)
library(ggplot2)
library(mice)

# import flight data 
data.jan2019 <- read.csv("/Users/minxiangliu/Desktop/MFIT/867/team project/Jan_2019_ontime.csv", header=TRUE, sep=",")
data.jan2020 <- read.csv("/Users/minxiangliu/Desktop/MFIT/867/team project/Jan_2020_ontime.csv", header=TRUE, sep=",")
data.feb2019 <- read.csv("/Users/minxiangliu/Desktop/MFIT/867/team project/Feb_2019_ontime.csv", header=TRUE, sep=",")
data.feb2020 <- read.csv("/Users/minxiangliu/Desktop/MFIT/867/team project/Feb_2020_ontime.csv", header=TRUE, sep=",")

# create a year indicator column to differentiate the sets later
data.jan2019$YEAR <- "2019"
data.jan2020$YEAR <- "2020"

data.feb2019$YEAR <- "2019" 
data.feb2020$YEAR <- "2020"

head(data.jan2020)

#glimpse(data.jan2019) # Rows: 583,985
#glimpse(data.jan2020) # Rows: 607,346

# merge the two jan datasets
data.jan <- rbind(data.jan2019, data.jan2020)

# merge the two feb datasets
data.feb <- rbind(data.feb2019, data.feb2020)

# NOTE: UNCOMMENT THE LINE BELOW TO USE THE CONDOLIDATED DATA JAN/FEB 2019/2020
# # consolidate jan data with feb data
# data.jan <- rbind(data.jan, data.feb)

# delete redundant object to conserve space
rm(data.feb, data.jan2019, data.jan2020, data.feb2019, data.feb2020)
#glimpse(data.jan)

# drop the last redundant column X
data.jan$X <- NULL

# review the structure and do data type formatting as appropriate
glimpse(data.jan)

# check for missing values
#md.pattern(data.jan) # has missing values
# md.pattern(data.jan[,-c(15:22)]) # has missing values
# md.pattern(data.jan[,-c(14:22)]) # no missing values
# md.pattern(data.jan[,c(1:13)]) # no missing values

sapply(data.jan, function(x) sum(is.na(x)))
sum(is.na(data.jan)) # total missing values

# analyze canceled flights
data.cancelled <- (filter(data.jan, data.jan$CANCELLED == 1 ))
# view(data.cancelled) 
table(data.cancelled$YEAR) 
table(data.jan$CANCELLED) 

# analyze diverted flights
data.diverted <- filter(data.jan, data.jan$DIVERTED == 1) # 2,446 flights diverted

# remove canceled flight record because a canceled flight can't be delayed!
data.jan <- filter(data.jan, data.jan$CANCELLED == 0) # remains 1,167,677 records

# remove all diverted flights  - all diverted flights have ARR_DEL15 = NA
data.jan <- filter(data.jan, data.jan$DIVERTED == 0) # remains 1,165,231 records

# check the remaining records for  missing values
#md.pattern(data.jan) # has missing values
sapply(data.jan, function(x) sum(is.na(x))) # no more missing records
sum(is.na(data.jan)) # total missing values - none

##############################
# *** Feature Engineering *** 
# ############################

# aggregation of departure delays as a result of DAY_OF_WEEK
agg_week <- data.jan %>% 
  group_by(DAY_OF_WEEK) %>% 
  summarise(TOT_DOW_DEP_DEL = sum(DEP_DEL15)) %>% 
  arrange(desc(TOT_DOW_DEP_DEL))

agg_week$TOT_DOW_DEP_DEL_PERC <- (agg_week$TOT_DOW_DEP_DEL/table(data.jan$DAY_OF_WEEK)[agg_week$DAY_OF_WEEK]) * 100
agg_week

# merge into dataset
data.jan <- merge(data.jan, agg_week[-c(2)], by = "DAY_OF_WEEK")

# aggregation of departure delays as a result of DAY_OF_MONTH
agg_month <- data.jan %>% 
  group_by(DAY_OF_MONTH) %>% 
  summarise(TOT_DOM_DEP_DEL = sum(DEP_DEL15)) %>% 
  arrange(desc(TOT_DOM_DEP_DEL))

agg_month$TOT_DOM_DEP_DEL_PERC <- (agg_month$TOT_DOM_DEP_DEL/table(data.jan$DAY_OF_MONTH)[agg_month$DAY_OF_MONTH]) * 100
agg_month

# merge into dataset
data.jan <- merge(data.jan, agg_month[-c(2)], by = "DAY_OF_MONTH")

# aggregation of departure delays as a result of DEP_TIME_BLK
agg_dep_timblk <- data.jan %>% 
  group_by(DEP_TIME_BLK) %>% 
  summarise(TOT_DTB_DEP_DEL = sum(DEP_DEL15)) %>% 
  arrange(desc(TOT_DTB_DEP_DEL))

agg_dep_timblk$TOT_DTB_DEP_DEL_PERC <- (agg_dep_timblk$TOT_DTB_DEP_DEL/table(data.jan$DEP_TIME_BLK)[agg_dep_timblk$DEP_TIME_BLK]) * 100
# agg_dep_timblk

# merge into dataset
data.jan <- merge(data.jan, agg_dep_timblk[-c(2)], by = "DEP_TIME_BLK")

# origin airport with worst delay record
agg_origin_delay <- data.jan %>% 
  group_by(ORIGIN) %>% 
  summarise(TOT_ORIGIN_DEP_DEL = sum(DEP_DEL15)) %>% 
  arrange(desc(TOT_ORIGIN_DEP_DEL))

agg_origin_delay$TOT_ORIGIN_DEP_DEL_PERC <- (agg_origin_delay$TOT_ORIGIN_DEP_DEL/table(data.jan$ORIGIN)[agg_origin_delay$ORIGIN]) * 100
# View(agg_origin_delay)

# merge into dataset
data.jan <- merge(data.jan, agg_origin_delay[-c(2)], by = "ORIGIN")

# destination airport with worst delay record
agg_dest_delay <- data.jan %>% 
  group_by(DEST) %>% 
  summarise(TOT_DEST_DEP_DEL = sum(DEP_DEL15)) %>% 
  arrange(desc(TOT_DEST_DEP_DEL))

agg_dest_delay$TOT_DEST_DEP_DEL_PERC <- (agg_dest_delay$TOT_DEST_DEP_DEL/table(data.jan$DEST)[agg_dest_delay$DEST]) * 100
# agg_dest_delay

# merge into dataset
data.jan <- merge(data.jan, agg_dest_delay[-c(2)], by = "DEST")

# create arrival time block ARR_TIME_BLK which follows DEP_TIME_BLK pattern
# transform DEP_TIME_BLK to the exact time duration
data.jan$ARR_TIME_BLK <- ifelse(data.jan$ARR_TIME < 600, "0001-0559",
                                paste(
                                  str_pad(as.character(data.jan$ARR_TIME - (data.jan$ARR_TIME %% 100)), 4, pad = "0"),
                                  str_pad(as.character((data.jan$ARR_TIME - (data.jan$ARR_TIME %% 100)) + 59), 4, pad = "0"),
                                  sep="-")
)

# aggregation of departure delays as a result of ARR_TIME_BLK
agg_dep_timblk <- data.jan %>% 
  group_by(ARR_TIME_BLK) %>% 
  summarise(TOT_ATB_DEP_DEL = sum(DEP_DEL15)) %>% 
  arrange(desc(TOT_ATB_DEP_DEL))

agg_dep_timblk$TOT_ATB_DEP_DEL_PERC <- (agg_dep_timblk$TOT_ATB_DEP_DEL/table(data.jan$ARR_TIME_BLK)[agg_dep_timblk$ARR_TIME_BLK]) * 100
# agg_dest_delay

# merge into dataset
data.jan <- merge(data.jan, agg_dep_timblk[-c(2)], by = "ARR_TIME_BLK")

# partition distance into quantile categories
quant <- quantile(data.jan$DISTANCE, prob=c(.3333333333333333,0.6666666666666666))

data.jan$DISTANCE_CAT <- ifelse(data.jan$DISTANCE >= quant[[2]], "HIGH",
                                ifelse(data.jan$DISTANCE >= quant[[1]],"MEDIUM",
                                       ifelse(data.jan$DISTANCE < quant[[1]],"LOW", "ERROR"))
)

# check for errors
filter(data.jan, data.jan$DISTANCE_CAT == "ERROR")

# delete redundant object to conserve space
rm(agg_origin_delay,agg_dep_timblk,agg_dest_delay,agg_month,agg_week)

      # clean objects to reclaim memory
      # rm(data.training,data.test,cols, data.cancelled, data.diverted, data.jan2019, data.jan2020)
      # rm(include_vars,cols1,auc2,cols,cols1,cols2,cols3,cols4,model.1,model.2,model.3,model.4,predicted_1,confusion.matrix,Count.correct,Count.wrong,Accuracy.rate,auc)
 
# Testing with different options...

# hist(data.training$TOT_DOW_DEP_DEL_PERC)
# hist(data.training$TOT_DOM_DEP_DEL_PERC)
# hist(data.training$TOT_DTB_DEP_DEL_PERC)
# hist(data.training$TOT_ORIGIN_DEP_DEL_PERC)
# hist(data.training$TOT_DEST_DEP_DEL_PERC)
# hist(data.training$TOT_ATB_DEP_DEL_PERC)
# hist(log(data.training$DISTANCE))
library(moments)
skewness(data.jan$DISTANCE)
skewness(log(data.jan$DISTANCE))

# Following variables are skewed
# TOT_ORIGIN_DEP_DEL_PERC
# TOT_DEST_DEP_DEL_PERC
# DISTANCE

data.jan$TOT_ORIGIN_DEP_DEL_PERC <-log(data.jan$TOT_ORIGIN_DEP_DEL_PERC)
data.jan$TOT_DEST_DEP_DEL_PERC <- log(data.jan$TOT_DEST_DEP_DEL_PERC)
data.jan$DISTANCE <- log(data.jan$DISTANCE)

include_vars <- c(
  "DISTANCE",
  "TOT_DOW_DEP_DEL_PERC",
  "TOT_DOM_DEP_DEL_PERC",
  "TOT_DTB_DEP_DEL_PERC",
  "TOT_ATB_DEP_DEL_PERC",
  "TOT_ORIGIN_DEP_DEL_PERC",
  "TOT_DEST_DEP_DEL_PERC",
  "DISTANCE_CAT",
  "YEAR",
  "DEP_DEL15"
)
data.jan <- data.jan[,(names(data.jan) %in% include_vars)]

# split data into training and test sets
data.training <- data.jan[data.jan$YEAR == 2019,] 
data.test <- data.jan[data.jan$YEAR == 2020,]
summary(data.training)

data.training.delay <-data.training$DEP_DEL15
data.test.delay <-data.test$DEP_DEL15

X<-model.matrix(~ DISTANCE+DISTANCE_CAT
                +TOT_DOW_DEP_DEL_PERC*TOT_DOM_DEP_DEL_PERC
                +TOT_DTB_DEP_DEL_PERC*TOT_ATB_DEP_DEL_PERC
                +TOT_ORIGIN_DEP_DEL_PERC*TOT_DEST_DEP_DEL_PERC
                , data.training)[, -1]

X.test<-model.matrix(~ DISTANCE+DISTANCE_CAT
                +TOT_DOW_DEP_DEL_PERC*TOT_DOM_DEP_DEL_PERC
                +TOT_DTB_DEP_DEL_PERC*TOT_ATB_DEP_DEL_PERC
                +TOT_ORIGIN_DEP_DEL_PERC*TOT_DEST_DEP_DEL_PERC
                , data.test)[, -1]

lasso.fit<-glmnet(x = X, y = data.training.delay, alpha = 1, family=binomial)
plot(lasso.fit, xvar = "lambda")

library(doParallel)
registerDoParallel(2)
crossval <-  cv.glmnet(x = X, y = data.training.delay, alpha = 1,  family=binomial, type.measure="auc",
                       parallel=TRUE) #create cross-validation data. By default, the function performs ten-fold cross-validation, though this can be changed using the argument nfolds. 
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph

plot(crossval,xlim=c(log(penalty.lasso)-3,log(penalty.lasso)+3)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X, y = data.training.delay, alpha = 1, lambda = penalty.lasso, family=binomial) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

predicted_1 <- predict(lasso.opt.fit, s = penalty.lasso, newx =X.test, type="response", family=binomial)

auc <- auc(data.test$DEP_DEL15, predicted_1)
auc # exclude_vars2: 0.8916,    Jan+Feb: 0.8927

# plot ROC and AUC for the final model chosen
# model.1_roc = roc(data.test$ARR_DEL15 ~ predicted_1, plot = TRUE, print.auc = TRUE)
model.1_roc = roc(data.test$DEP_DEL15 ~ predicted_1, plot = TRUE, print.auc = TRUE)

# build a logistic regression model
# model.1 <- glm(ARR_DEL15 ~ ., data=data.training, family=binomial)
#library(parglm)
model.1 <- glm(DEP_DEL15 ~ +DISTANCE +TOT_DOW_DEP_DEL_PERC*TOT_DOM_DEP_DEL_PERC+
                 TOT_DTB_DEP_DEL_PERC*TOT_ATB_DEP_DEL_PERC+
                 TOT_ORIGIN_DEP_DEL_PERC*TOT_DEST_DEP_DEL_PERC+
                 DISTANCE_CAT
                 #+TOT_DOW_ARR_DEL_PERC*TOT_DOM_ARR_DEL_PERC + TOT_DEP_ORIGIN_DEL_PERC*TOT_ARR_DEST_DEL_PERC
                 , data=data.training, family=binomial)
summary(model.1)

predicted_1 = predict(model.1, newdata=data.test, type="response")

# confusion.matrix <- table(data.test$ARR_DEL15, predicted_1 >= 0.15) #
confusion.matrix <- table(data.test$DEP_DEL15, predicted_1 >= 0.15) #

# calculate the accuracy rate
Count.correct <- confusion.matrix[1,1] + confusion.matrix[2,2]
Count.wrong <- confusion.matrix[1,2] + confusion.matrix[2,1]
Accuracy.rate <- Count.correct/(Count.correct + Count.wrong)
Accuracy.rate
Accuracy.rate

# calculate AUC, check closeness to 1
auc <- auc(data.test$DEP_DEL15, predicted_1)
auc # exclude_vars2: 0.8916,    Jan+Feb: 0.8927

# plot ROC and AUC for the final model chosen
# model.1_roc = roc(data.test$ARR_DEL15 ~ predicted_1, plot = TRUE, print.auc = TRUE)
model.1_roc = roc(data.test$DEP_DEL15 ~ predicted_1, plot = TRUE, print.auc = TRUE)

# Extra plots - note the effect of legacy.axes = FALSE/TRUE on specificity on the x-axes
 ggroc(model.1_roc, colour = 'steelblue', size = 1, legacy.axes = TRUE) +
   ggtitle(paste0('ROC Curve ', '(AUC = ', round(auc,2), ')')) +
   geom_abline() +
   theme_minimal()




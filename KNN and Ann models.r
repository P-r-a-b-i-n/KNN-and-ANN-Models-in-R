
### Supervised Learning with Polynomial, KNN and ANN regression model

##### Loading covid_tbl_final.xslx

library(readxl)
covid_data <- read_xlsx("/home/prabin/Downloads/covid_tbl_final.xlsx")
head(covid_data)

##### Cleaning covid_data

str(covid_data)

covid_data$Date<-as.Date(as.POSIXct(covid_data$Date))
str(covid_data)


##### Plotting total deaths vs date


plot(covid_data$Date,covid_data$Deaths_total, xlab="Date",ylab="Total Deaths")


##### Plotting daily deaths vs date


plot(covid_data$Date,covid_data$Deaths_daily,main="Daily Deaths from 2020-01-23 to 2020-05-31", xlab="Date", ylab = "Daily Deaths")

summary(covid_data$Deaths_daily)

library(dplyr)
filter(covid_data, Deaths_daily >=50 & Date <=as.Date("2021-03-05"))


wsn<-c(399,401,408)
for(i in 1:length(wsn)){

temp_sn = wsn[i]  
# Get the Value to be adjusted
curr_val<-covid_data[covid_data$SN==temp_sn,"Deaths_daily"]
# Calculate the average daily deaths for last 30 days
avg_daily_deaths<-ceiling(mean(covid_data[covid_data$SN %in% c((temp_sn-1):(temp_sn-1-30)),]$Deaths_daily))

# Change the Value for given SN
covid_data[covid_data$SN==temp_sn,"Deaths_daily"]=avg_daily_deaths
# Change values for last 30 days
covid_data[covid_data$SN %in% c((temp_sn-1):(temp_sn-1-30)),]$Deaths_daily=as.integer( round(curr_val/30))
}

plot(covid_data$Date,
covid_data$Deaths_daily,
main = "Daily Deaths: 23 Jan 2020
- 31 May 2021",
xlab = "Date",
ylab = "Daily Deaths")



##### Splitting the data into training and testing set

set.seed(1234)
ind <- sample(2, nrow(covid_data), replace=T, prob=c(0.7,0.3))
train_data <- covid_data[ind==1,]
test_data <- covid_data[ind==2,]



##### Linear regression model


library(caret)
lm1 <- train(Deaths_daily~SN, data = train_data, method="lm")
summary(lm1)
predict1 <- predict(lm1, newdata = test_data)


######Evaluating metrics

R2 <- R2(predict1,test_data$Deaths_daily)
RMSE <- RMSE(predict1,test_data$Deaths_daily)
MAE <- MAE(predict1,test_data$Deaths_daily)
R2
RMSE
MAE


##### Quadratic linear regression model

lm2 <- train(Deaths_daily~poly(SN,2),data=train_data, method="lm")
summary(lm2)
predict2 <- predict(lm2,newdata = test_data)

head(predict2)

R2 <- R2(predict2,test_data$Deaths_daily)
RMSE <- RMSE(predict2,test_data$Deaths_daily)
MAE <- MAE(predict2,test_data$Deaths_daily)
R2
RMSE
MAE

##### Cubic linear regression model

lm3 <- train(Deaths_daily~poly(SN,3), data = train_data, method="lm")
summary(lm3)

predict3 <- predict(lm3,newdata = test_data)
head(predict3)

R2 <- R2(predict3,test_data$Deaths_daily)
RMSE <- RMSE(predict3,test_data$Deaths_daily)
MAE <- MAE(predict3,test_data$Deaths_daily)
R2
RMSE
MAE



##### Double quadratic linear model

lm4 <- train(Deaths_daily~poly(SN,4), data=train_data, method="lm")
summary(lm4)

predict4 <- predict(lm4, newdata = test_data)
head(predict4)

R2 <- R2(predict4,test_data$Deaths_daily)
RMSE <- RMSE(predict4,test_data$Deaths_daily)
MAE <- MAE(predict4,test_data$Deaths_daily)
R2
RMSE
MAE

##### Fifth order polynomial regression model

lm5 <- train(Deaths_daily ~SN, data= train_data, methos="lm")
summary(lm5)

predict5 <- predict(lm5, newdata = test_data)
head(predict5)

R2 <- R2(predict5,test_data$Deaths_daily)
RMSE <- RMSE(predict5,test_data$Deaths_daily)
MAE <- MAE(predict5,test_data$Deaths_daily)
R2
RMSE
MAE



##### KNN regression model

knnmodel <- train(Deaths_daily ~SN, data=train_data, method="knn")
summary(knnmodel)

predict6 <- predict(knnmodel, newdata = test_data)
head(predict6)

R2 <- R2(predict6,test_data$Deaths_daily)
RMSE <- RMSE(predict6,test_data$Deaths_daily)
MAE <- MAE(predict6,test_data$Deaths_daily)
R2
RMSE
MAE

##### ANN-MLP regression model with 2 hidden layers with 3 and 2 neurons


#install.packages("neuralnet")
library(neuralnet)
neural1 <- neuralnet(Deaths_daily ~ SN, data= train_data, hidden=c(3,2),linear.output = F)
plot(neural1, main="Neural network with 2 hidden layers with 3 and 2 neurons")
summary(neural1)

predict7 <- predict(neural1, newdata = test_data)
head(predict7)

R2 <- R2(predict7,test_data$Deaths_daily)
RMSE <- RMSE(predict7,test_data$Deaths_daily)
MAE <- MAE(predict7,test_data$Deaths_daily)
R2
RMSE
MAE

##### Selection of best model based on RMSE on test data


##### Summary and Recommendation


plot(covid_data$SN, covid_data$Deaths_daily,
main = "Daily Covid Deaths",
xlab = "SN",
ylab = "Daily Deaths")
lines(predict(knnmodel,newdata = covid_data), col = "red", lwd=2)



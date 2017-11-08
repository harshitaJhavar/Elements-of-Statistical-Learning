#setwd("/home/jhavarharshita/Desktop/WS 2017_18/ESL/Assignments/assignment1")
#Submitted by: Harshita Jhavar 2566267 s8hajhav@stud.uni-saarland.de
#ESL Exercise 1
#exercise 1 Solution 4: 

#Part b and c
#Loading the ozone dataset
load("/home/jhavarharshita/Desktop/WS 2017_18/ESL/Assignments/assignment1/ozone.RData")
library(FNN)
#Inspecting the structure of the ozone dataset
ls(ozone) #Listing the ozone dataset components
str(ozone) #Structure of the dataset ozone
summary(ozone) #Computing column-wise summary of the dataset - gives the mean, median, range and the quantile ranges for each column.
dim(ozone) #Querying the dimensions of the dataset
length(ozone) #Number of columns in the dataset
colnames(ozone) #Querying the column names in the dataset
#Inspecting the structure of the trainingset containing the row indexes
str(trainset)
#Inspecting the structure of the testset
str(testset)

#Define function to return the range, mean and sd of the data table

find_range_mean_sd <- function(datacolumn){
  cat("range = ",range(datacolumn),"mean = ",mean(datacolumn),"standard deviation = ",sd(datacolumn))
}
#Computing the range, mean and sd for complete ozone dataset
find_range_mean_sd(ozone$ozone)
find_range_mean_sd(ozone$radiation)
find_range_mean_sd(ozone$temperature)
find_range_mean_sd(ozone$wind)

# Computing the range, mean and sd for the training set
trainingdata <- ozone[trainset,]
summary(trainingdata)
find_range_mean_sd(trainingdata$ozone)
find_range_mean_sd(trainingdata$radiation)
find_range_mean_sd(trainingdata$temperature)
find_range_mean_sd(trainingdata$wind)

# Computing the range, mean and sd for the test set
testdata <- ozone[testset,]
summary(testdata)
find_range_mean_sd(testdata$ozone)
find_range_mean_sd(testdata$radiation)
find_range_mean_sd(testdata$temperature)
find_range_mean_sd(testdata$wind)

#Part d: Pearson's Correlation Coefficient and scatter plotting

#Defining the function
find_correlation <- function(col1, col2, colour_of_plot, title, xlabel, ylabel){
  cat(cor(col1, col2)) #Prinitng the correlation value
  print(t.test(col1, col2)) #Performing the t-test
  plot(col1, col2, type = "p"  , main = c(title," P.C. Coeff. = ", cor(col1, col2)), xlab = xlabel, ylab = ylabel, frame.plot = TRUE, col = colour_of_plot) #Plotting the scatter plot
}

#For ozone ~ radiation
find_correlation(ozone$radiation, ozone$ozone, "blue", "Ozone ~ Radiation", "Radiation","Ozone Concentration")
#For ozone ~ temperature
find_correlation(ozone$temperature, ozone$ozone, "green", "Ozone ~ Temperature", "Temperature","Ozone Concentration")
#For ozone ~ wind
find_correlation(ozone$wind, ozone$ozone, "red", "Ozone ~ Wind", "Wind","Ozone Concentration")
#For radiation ~ temperature
find_correlation(ozone$temperature,ozone$radiation, "blue", "Radiation~Temperature", "Temperature","Radiation")
#For radiation ~ wind
find_correlation(ozone$wind, ozone$radiation, "green", "Radiation~Wind", "Wind","Radiation")
#For temperature ~ wind
find_correlation(ozone$wind, ozone$temperature, "red", "Temperature ~ Wind", "Wind","Temperature")

#Part e: Function of residual Sum of Squares
rss <- function(predicted_value_vector, true_value_vector){
  return(sum((true_value_vector - predicted_value_vector)^2))
}

#Part f: Prediction model using linear regression model and plotting the values

model <- lm(ozone ~ radiation + temperature + wind, data = trainingdata) #Trained on normalized data
predicted_values <- predict.lm(model,testdata)
rss(predicted_values,testdata$ozone)
find_correlation(predicted_values,testdata$ozone,"blue","Predicted Values ~ True Values", "True Values", "Predicted Values")

#Part g: KNN algorithm
rss_vector_testdata <- {}
rss_vector_traindata <- {}


#For Training Data
for (i in 1:30){ 
  predicted_values_knn <- knn.reg(trainingdata, trainingdata, trainingdata$ozone, i, algorithm=c("kd_tree", "cover_tree", "brute"))
  rss_vector_traindata[i] <-rss(predicted_values_knn$pred,trainingdata$ozone)
}

#For Test Data
for (k in 1:30){ 
  predicted_values_knn <- knn.reg(trainingdata, testdata, trainingdata$ozone, k, algorithm=c("kd_tree", "cover_tree", "brute"))
  rss_vector_testdata[k] <-rss(predicted_values_knn$pred,testdata$ozone)
  plot(rss_vector_testdata)
}

#Plotting the rss values
# first plot
plot(rss_vector_testdata, ylim=range(c(rss_vector_testdata,rss_vector_traindata)),col="green",xlab = "",ylab = "")
par(new = TRUE)
plot(rss_vector_traindata, ylim=range(c(rss_vector_testdata,rss_vector_traindata)), axes = FALSE, col = "blue", xlab = "Value of k in KNN", ylab = "RSS Value")
legend("topleft", inset=.05,
       c("test RSS error","train RSS error"), fill=c("green","blue"), horiz=TRUE)

#Part h
#RSS for K=3 KNN model
predicted_values_knn <- knn.reg(trainingdata, testdata, trainingdata$ozone, 3, algorithm=c("kd_tree", "cover_tree", "brute"))
rss(predicted_values_knn$pred,testdata$ozone)
#Finding correlation of 3NN Nearest Neighbour model
find_correlation(predicted_values_knn$pred,testdata$ozone,"blue","Predicted Values From 3NN ~ True Values", "True Values", "Predicted Values from 3NN")











#Solutions to assignment 3
#Submitted by: Harshita Jhavar
#Matriculation Number: 2566267

#Question 4
#Part a
#setwd("/home/jhavarharshita/Desktop/WS 2017_18/ESL/Assignments/assignment3")
library(ISLR)
library(MASS)
library(ggplot2)
library(gridExtra)
library(scales)
phoneme_data <- read.csv("phoneme.csv")
#Splitting the dataset into training and test set 
train_data <- subset(phoneme_data[c(-1,-259)], grepl("train",phoneme_data$speaker))
test_data <- subset(phoneme_data[c(-1,-259)], grepl("test",phoneme_data$speaker))

#Part b
#Fitting the LDA function
#Column g is the class label here and is excluded from the feature set
lda_model <- lda(g ~ .,train_data) 
#Plotting the lda model
plot(lda_model)
#Prediction model
#Train Error
lda_prediction_train <- predict(lda_model, train_data)$class
train_error <- mean(lda_prediction_train != train_data$g)
train_error
#Test Error
lda_prediction_test <- predict(lda_model, test_data)$class
test_error <- mean(lda_prediction_test != test_data$g)
test_error

#Part c
plda <- predict(lda_model,train_data)
prop.lda = lda_model$svd^2/sum(lda_model$svd^2)
dataset = data.frame(Phonems = train_data[,"g"], lda = plda$x)
lda.LD1 = lda_model$scaling[,1]
lda.LD2 = lda_model$scaling[,2]
lda.LD3 = lda_model$scaling[,3]
lda.LD4 = lda_model$scaling[,4]
#Plotting them
plot1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = Phonems), size = 2) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""), y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))
grid.arrange(plot1)
plot2 <- ggplot(dataset) + geom_point(aes(lda.LD3, lda.LD4, colour = Phonems), size = 2) + 
  labs(x = paste("LD3 (", percent(prop.lda[3]), ")", sep=""), y = paste("LD4 (", percent(prop.lda[4]), ")", sep=""))
grid.arrange(plot2)

#Plotting with different dimensions
plot3 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD3, colour = Phonems), size = 2) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""), y = paste("LD3 (", percent(prop.lda[3]), ")", sep=""))
grid.arrange(plot3)
plot4 <- ggplot(dataset) + geom_point(aes(lda.LD2, lda.LD4, colour = Phonems), size = 2) + 
  labs(x = paste("LD2 (", percent(prop.lda[2]), ")", sep=""), y = paste("LD4 (", percent(prop.lda[4]), ")", sep=""))
grid.arrange(plot4)

#Part d
#Selecting dataset for class "aa" and "ao"
train_data2 = train_data[which(train_data$g=="ao"|train_data$g =="aa"),]
test_data2 = test_data[which(test_data$g=="ao"|test_data$g =="aa"),]
#Converting type of g to character to reduce the number of levels
train_data2$g <- as.character(train_data2$g)
test_data2$g <- as.character(test_data2$g)
#LDA Modelling
lda_model2 <- lda(g ~ ., train_data2)
lda_prediction_train_2 <- predict(lda_model2, train_data2)$class
lda_prediction_test_2 <- predict(lda_model2, test_data2)$class
#Train Error
mean(lda_prediction_train_2 != train_data2$g)
#Test Error
mean(lda_prediction_test_2 != test_data2$g)

#Part e
#QDA model fitting on the full train data
qda_model <- qda(g ~ ., train_data)
qda_prediction_train <- predict(qda_model, train_data)$class
qda_prediction_test <- predict(qda_model, test_data)$class
#QDA Training Error
mean(qda_prediction_train != train_data$g)
#QDA Testing Error
mean(qda_prediction_test != test_data$g)

#QDA on class "aa" and "ao"
qda_model2 <- qda(g ~ ., train_data2)
qda_prediction_train_2 <- predict(qda_model2, train_data2)$class
qda_prediction_test_2 <- predict(qda_model2, test_data2)$class
#QDA Training Error
mean(qda_prediction_train_2 != train_data2$g)
#QDA Testing Error
mean(qda_prediction_test_2 != test_data2$g)
#Part f
#LDA Confusion Matrix
g_test = test_data[,257]
g_test2 = subset(g_test, g_test %in% c("ao","aa"))
g_test2 <- droplevels(g_test2)
test_data2 = subset(test_data, g %in% c("ao","aa"))
train_data2 = subset(train_data, g %in% c("ao", "aa"))
test_data2 <- droplevels(test_data2)
train_data2 <- droplevels(train_data2)
lda_model2 <- lda(g ~ ., train_data2)
prediction_test2 <- predict(lda_model2,test_data2)
table(prediction_test2$class,g_test2)
# QDA Confusion Matrix
qda_model2 <- qda(g ~ ., train_data2)
qda_prediction_test2 <- predict(qda_model2,test_data2)
table(qda_prediction_test2$class,g_test2)


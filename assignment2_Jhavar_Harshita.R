#Submission for assignment2
#Submitted by: Harshita Jhavar 2566267 s8hajhav@stud.uni-saarland.de
#Question 4
#Installing ISLR package
#install.packages("ISLR")
#Loading the library ISLR
library(ISLR)

#Part a: Computing the correlation Matrix
#Excluding non-numeric columns
correlation_matrix <- cor(Auto[,unlist(lapply(Auto, is.numeric))])
correlation_matrix
#Generating graphical representation of the correlation matrix for Parta
#install.packages("corrplot")
#library(corrplot)
#corrplot(correlation_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#Part b: 
#Plotting positively correlated variables
plot(Auto$cylinders, Auto$displacement, main="Cylinders vs Displacement : Highly Positively Correlated = 0.95")
plot(Auto$cylinders, Auto$horsepower, main="Cylinders vs Horsepower : Highly Positively Correlated = 0.84")
plot(Auto$cylinders, Auto$Weight, main ="Cylinders vs Weight : Highly Positively Correlated = 0.89")
plot(Auto$displacement, Auto$horsepower, main = "Displacement vs Horsepower : Highly Positively Correlated = 0.89")
plot(Auto$displacement, Auto$weight, main = "Displacement vs Weight : Highly Positively Correlated = 0.93")
plot(Auto$horsepower, Auto$weight, main = "Horsepower vs Weight : Highly Positively Correlated = 0.86")
#Plotting negatively correlated variables
plot(Auto$mpg, Auto$cylinders, main= "Cylinders vs MPG: Highly negatively correlated = -0.78")
plot(Auto$mpg, Auto$displacement, main ="Mpg vs Displacement: Highly negatively correlated = -0.81")
plot(Auto$mpg, Auto$horsepower, main = "Mpg vs Horsepower: Highly negatively correlated = -0.78")
plot(Auto$mpg, Auto$weight, main = "Mpg vs Weight: Highly negatively correlated = -0.83")

#Scatter Plotting the different variables together 
pairs(Auto[,1:8], main = " Scatter Plot of different pairs of variables",
      pch = 21, lower.panel = panel.smooth, upper.panel = panel.cor)

#Part c
cylinder_lr_model <- lm(data = Auto, mpg~cylinders)
displacement_lr_model <- lm(data = Auto, mpg~displacement)
horsepower_lr_model <- lm(data = Auto, mpg~horsepower)
year_lr_model <- lm(data = Auto, mpg~year)
#Checking statistical significance relationship and commenting on the model quality
#Which predictors appear to have a statistically significant relationship to the
#outcome and how good are the resulting models (measured using R^2)?
summary(cylinder_lr_model)
summary(displacement_lr_model)
summary(horsepower_lr_model)
summary(year_lr_model)

#Part d
multiple_lr_model <- lm(data = Auto, mpg~cylinders+displacement+horsepower+weight+acceleration+year)
summary(multiple_lr_model)

#Comparison between models of Part c and Part d
# What can you observe in the different models concerning
#the significance of the relationship between response and individual predictors? What does the sign of the
#coefficient tell you about the relationship between the predictor and the response?

#Part e
#Diagnostic plot of the linear relationship of the model in Part d
plot(multiple_lr_model)
#Part f
interaction_log_lm1 <- lm(data = Auto, mpg~cylinders:weight+ weight:year + year:cylinders + log(displacement))
interaction_sqrt_lm2 <- lm(data = Auto, mpg~cylinders:weight+ weight:year + year:cylinders + sqrt(displacement))
interaction_square_lm3 <- lm(data = Auto, mpg~cylinders:weight+ weight:year + year:cylinders + (displacement^2))

summary(interaction_log_lm1)
summary(interaction_sqrt_lm2)
summary(interaction_square_lm3)




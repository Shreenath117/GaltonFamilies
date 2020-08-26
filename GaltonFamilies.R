# Case Study Solutions : GaltonFamilies.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('GaltonFamilies.csv')
# View the data loaded
data
# Dropping the first column which is nothing but the Serial number
data=data[2:9]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 934 rows and 8 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

# The key objective of the case study is to : predicting children's height from parents' heights.
# question of the relation between heights of parents and their offspring?
# The response variable : childHeight using the predictor variables given in the dataset for parents being : Father, Mother, midparent Height, gender

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 3:

# Correlation Matrix

cor(data[,c("childHeight","father","mother")])

#-------------------------------------------------------------------------------------------------
# Soln. to Question 4:
#Plotting a scatterplot matrix

pairs(data[,c("childHeight","father","mother")], pch=16, las=1)

#-------------------------------------------------------------------------------------------------

# Soln. to Question 5:

#Summarising the dataset : 
summary(data)

# Observations :
# The mean height of father is 69.2 with maximum being 78.5
# The mean height of mother is 64.09 with maximum being 70.5
# The mean height of child is 66.75 with maximum being 79
# Amongst the children : Count of Male distribution is higher than female 

# Check the datatypes
str(data)
sapply(data, class)

#Here : Family and Gender are factors and rest all are integer and numeric
#-------------------------------------------------------------------------------------------------
# Soln. to Question 6:

# Calculation average  height for father & mother
(parents_avg <- colMeans(data[,c("father","mother")]))

parents_avg["father"]/parents_avg["mother"]

# 1.08 is the ratio 

# This shows that Father's mean height is about 8% higher than Mother's mean height.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

# Calculation average height for children ( split by gender)
(children_avg <- tapply(data$childHeight, data$gender,mean))

# Calculating the ratio

children_avg["male"]/children_avg["female"]

# Male children's mean height is about 8% higher than female children's mean height.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

# As we clearly saw : Both Mean and Median for Father is about 8% higher than Mother
# Similarly, both mean and median for the male children's height is about 8% higher than female's

# A factor of  1.08 was introduced to account for the gender difference. 
# (To address the gender difference between male and female heights)
# Galton also "transmuted" the heights of all female children to the male equivalents by multiplying the female heights by 1.08.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 9:

cor(data$childHeight,data$midparentHeight)

# Correlation of 0.321 or 32.1%

#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:
data$adj_childHeight <- data$childHeight
data$adj_childHeight[data$gender=="female"] <- data$childHeight[data$gender=="female"]*1.08


#-------------------------------------------------------------------------------------------------
# Soln. to Question 11:
cor(data$adj_childHeight,data$midparentHeight)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

# Correlation of adjusted child height with mid parent height is higher (0.50) compared to
# Correlation scores of child height with mid parent  height

# The reason being  the multiplication factor of 1.08 already taken care of in the mid parent height  column
#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:
plot(data$childHeight, data$father, main="Scatterplot: Child height vs Father height",
     xlab="Father height ", ylab="Child Height", pch=19)

plot(data$childHeight, data$mother, main="Scatterplot: Child height vs Mother height",
     xlab="Mother height ", ylab="Child Height", pch=19)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:
# Reset "male" to the reference level for the 'Gender' factor:

data$gender <- relevel(data$gender, ref="male")

# Fit the multiple regression model

fit_mult <- lm(childHeight ~ gender+father+mother, data=data)

summary(fit_mult)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:

# Assumptions : 
#  1)  Relationship between your independent and dependent variables should always be linear i.e. you can depict a relationship between two variables with help of a straight line. 
#  2)  Mean of residuals should be zero or close to 0 as much as possible. It is done to check whether our line is actually the line of "best fit".
#  3)  There should be homoscedasticity or equal variance in our regression model. This assumption means that the variance around the regression line is the same for all values of the predictor variable (X)
#  4)  All the dependent variables and residuals should be uncorrelated.
#  5)  Number of observations should be greater than the number of independent variables
#  6)  There should be no perfect multicollinearity in your model. Multicollinearity generally occurs when there are high correlations between two or more independent variables.
#      We can check multicollinearity using VIF(variance inflation factor).
#  7)  Residuals should be normally distributed. This can be checked by visualizing Q-Q Normal plot

# We can use the gvlma library to evaluate the basic assumptions of linear regression for us automatically.

# install.packages("gvlma")
library(gvlma)
gvlma(fit_mult)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:

# Checking the multicollinearity assumption
cor(data[,c("father","mother")])

# Correlation is very less between the  predictor variables
# There is no multicollinearity existing between any of the predictor variables
#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

# The Variance Inflation Factor (VIF) is a measure of colinearity among predictor variables within a multiple regression.
# V.I.F. = 1 / (1 - R^2)
# if the VIF is between 5-10, multicolinearity is likely present and you should consider dropping the variable.

# In R : we have vif() function in  CAR package to evaluate the VIF score
library(car)
vif(fit_mult)

# Value is within 1 : so we keep it

#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:

# All the variables are significant the gender female having a negative co-efficient
# Here p-value is very low : <2.2e-16 , which shows it's highly significant
# Adjusted R score of 63%

# Plotting residuals vs fitted

plot(fit_mult$residuals ~ fit_mult$fitted.values, col=data$gender, 
     xlab="Predicted", ylab="Residuals", pch=16, las=1)
abline(h=0)
legend("topleft",c("Male","Female"),pch=16,col=1:2)

# We can clearly see two clusters of points here. 
# The cluster of points with smaller predicted heights belong to the female children and the other cluster of points belong to the male children
#-------------------------------------------------------------------------------------------------
# Soln. to Question 19:

# Simple Linear Regression model

fit <- lm(data$adj_childHeight ~ data$midparentHeight, data=data)
summary(fit)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 20:

plot(fit$residuals ~ fit$fitted.values, col=data$gender,
     xlab="Predicted", ylab="Residuals", pch=16, las=1)
abline(h=0)
legend("topleft",c("Male","Female"),pch=16,col=1:2)

# We no longer see the two clusters of points because we have added the  multiplication factor to take care of the gender differences

#-------------------------------------------------------------------------------------------------
# Soln. to Question 21:

# How does the simple regression model compare with the multiple regression model ?
# One measure of the "goodness of fit" is R2. 
# However, comparing R2 returned by the simple linear & that of multiple regression is misleading
# because their predicted variables are different. 

# In multiple regression, the predicted variable is children's height, whereas 
# in linear regression , the predicted variable is children's adjusted height. 

# To have a fair comparison, we want to calculate the R2 of the model in multiple regression
# for the adjusted height and then compare it with the R2 in simple linear  regression.

#-------------------------------------------------------------------------------------------------

library(PPforest)
library(ggplot2)
library(dplyr)
library(readr)
library(ISLR)
library(nlme)
library(drc)

#ANALYSING ALL FISH DATA 

#After examining the data, there was one fish entered with a weight of 0
#this is not possible, so we have removed this row 
fishcatch = fishcatch %>%
  filter(weight > 0)
fishcatch = subset(fishcatch, select = -c(predicted, residuals))
head(fishcatch)

#exploratory analysis

#firstly, I looked at pictures of the fish. For this first stage of analysis, I will include all the different Types of fish to establish a general linear relationship
#however, I noticed that two of the types of fish - smelt and pike - are much longer and thinner proportionally than the other types of fish
#thus, at the end i will build two different models to determine whether it is more accurate to separate them from the other types of fish 
#looking at the pictures also helped me to determine that just using length3 - the length from nose to end of tail - is sufficient
#if some of the fish had had disproportionately long tails, using another length measurement may have been useful, but they are all fairly similar 


#correlation tests 
correlation = cor(fishcatch$weight, fishcatch$length3)
correlation 
#so Pearson's Correlation coefficient between length and weight is r = 0.924606

#linear regression models

#determining which variables to use through AIC
#i used all three methods to make sure my judgement is valid 
full = lm(weight ~ ., data = fishcatch)
summary(full)
null = lm(weight ~ 1, data = fishcatch)
summary(null)
#forward selection - selects length3 and type and height, AIC = 1436.75
step(null, scope = list(lower = null, upper = full), direction = "forward")
#backward selection - selects type and length1 and length2 and height, AIC = 1436.69
step(full, direction = "backward")
#stepwise selection - selects length3 and type and height, AIC = 1436.75
step(null, scope = list(upper = full), direction = "both")
#there is very little in it
#as the slightly higher AIC involves one less variable, this may be easier to use. It also avoids using two length variables, which would have potentially introducted the problem of multicollinearity

#basic linear model plotting weight against length
model = lm(weight ~ length3, data = fishcatch)
summary(model)
#so using the least squares method, the regression equation for the data is yhat = -487.9506 + 28.4564x

#null hypothesis: there is no relationship between length and weight of fish 
#from model, we can see that the p-value is < 2.2e-16. since this is much less than 0.05, we can reject the null hypothesis
#the adjusted R-squared value, 0.854, tells us that 85.4% of the variation in the response variable (the weight) is explained by the length

#linear model including all of the variables selected by AIC
AIC_model = lm(weight ~ length3 + Type + height, data = fishcatch)
summary(AIC_model)
#with our better AIC model, the adjusted R-squared value is 0.9343, which tells us that 93.4% of the variation in the weight is explained by the length, height and type combined
AIC_log_model = lm(log1p(weight) ~ log1p(length3) + log1p(height) + Type, data = fishcatch)
summary(AIC_log_model)
#taking the log of our values results in an adjusted R-squared value of 0.9955, which tells us that 99.55% of the variation in the log value of the weight is explained by the log values of the length, height and type 


#calculate the residuals
#initial model residuals
error = model$residuals
lm_error = sqrt(mean(error^2))
lm_error
#initial model residuals = 135.8859

#non-transformed AIC model
error2 = AIC_model$residuals
lm_error2 = sqrt(mean(error2^2))
lm_error2
#this is 89.09647

#log transformed AIC model
error3 = AIC_log_model$residuals
lm_error3 = sqrt(mean(error3^2))
lm_error3
#this is 0.08448 ***
#thus, we can see that taking the log of the values provides a much better predictive model for weight of fish 


#visualization

#think about adding SE values and error bars to the plot 
#functions to label our graphs correctly
show_as_cm = function(length3) {
  output = paste0(length3, " cm")
  return(output)
}
show_as_grams = function(weight) {
  output = paste0(weight, " g")
  return(output)
}

#plotting the data using all specified AIC variables 
AIC_plot = ggplot(data = fishcatch,
                  aes(x = length3,
                      y = weight,
                      color = Type,
                      size = height)) +
  geom_point(alpha = .5) +
  scale_x_continuous(labels = show_as_cm) +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "The Weight of Fish Based on Length, Height and Type", subtitle = "Fish Catch Data", x = "Length", y = "Weight", color = "Type of Fish", size = "Height as a % of Length")
AIC_plot

#plotting a linear model fit, excluding type specification and including a linear model line 
AIC_plot2 = ggplot(data = fishcatch,
                  aes(x = length3,
                      y = weight,
                      size = height)) +
  geom_point(alpha = .5, aes(color = Type)) +
  geom_smooth(method = 'lm') +
  scale_x_continuous(labels = show_as_cm) +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "A Linear Regression Model of The Weight of Fish Based on Length, Height and Type", subtitle = "Fish Catch Data", x = "Length", y = "Weight",color = "Type of Fish", size = "Height as as % of Length")
AIC_plot2



#plotting all specified AIC variables, taking the log 

AIC_plot3 = ggplot(data = fishcatch,
                  aes(x = log1p(length3),
                      y = log1p(weight),
                      color = Type,
                      size = log1p(height))) +
  geom_point(alpha = .5) +
  scale_x_continuous(labels = show_as_cm) +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "The Weight of Fish Based on Length, Height and Type, Log Transformed", subtitle = "Fish Catch Data", x = "Length", y = "Weight", color = "Type of Fish", size = "Height as a % of Length")
AIC_plot3

#plotting a linear model fit using AIC variables, taking the log 
AIC_plot4 = ggplot(data = fishcatch,
                   aes(x = log1p(length3),
                       y = log1p(weight),
                       size = log1p(height))) +
  geom_point(alpha = .5, aes(color = Type)) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = show_as_cm) +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "A Log Transformed Linear Regression Model of The Weight of Fish Based on Length, Height and Type", subtitle = "Fish Catch Data", x = "Length", y = "Weight", color = "Type of Fish", size = "Height as a % of Length")
AIC_plot4



#using errorbars: geom_errorbar(aes(ymin = se.min, ymax = se.max), width = 0.2)









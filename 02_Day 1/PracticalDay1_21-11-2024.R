#==============================================================================#
# Day 1: Practical Parts 1 & 2
#==============================================================================#

# For Parts 1 & 2, load the R-workspace Workspace-TSB-Practical-Day1.RData
#load("C:/Users/[...]/Day 1/PracticalDay1.RData")

### Packages required for Practical --------------------------------------------

#install.packages("performance")
#install.packages("see")
#install.packages("GGally")
#install.packages("lmerTest")
#install.packages("psych")
#install.packages("sandwich")
#install.packages("lmtest")
#install.packages("tidyverse")
#install.packages("brms")


library(performance)
library(see)
library(GGally)
library(lmerTest)
library(psych)
library(sandwich)
library(lmtest)
library(tidyverse)
library(brms)

################# Part 1: Visualization & Modeling #############################

#------------------------------------------------------------------------------#
# These assignments are about a study on the popularity of highschool students. 
# A total of 246 students from 12 different classes took part in this study in 
# which we determined how Extraversion, Gender, and Teacher Experience 
# influenced a student's popularity.

# The data for the 12 different classes are stored in de the dataframes 
# Class1 - Class12. The dataframe Total contains the combined data of all 
# classes.

# List of all the variables:
# - pupil:pupil identification variable
# - class:class identification variable
# - student-level independent variables: 
#       Extraversion (continuous; higher scores mean higher Extraversion) and 
#       Gender (dichotomous; 0=male, 1 =female)
# - class-level independent variables: 
#       teacher experience (in years)
# - outcome variable: 
#       popular (continuous: higher scores indicate higher popularity)
#------------------------------------------------------------------------------#

#### Assignment 1---------------------------------------------------------------
# Let's visualize our data and check assumptions for relevant variables.
head(Total) # shows first lines of data
describe(Total, fast = TRUE) # descriptive statistics for all variables

# As described in the lecture, descriptives alone are not enough to get to 
# know your data properly, you need to visualize. Let's do that next.

# For simplicity, we'll only look at the variables Extraversion, Popular, and
# Gender (but feel free to play around with the other variables too).
# We'll first select only those variables and store them in a dataframe called
# subset which we'll run our visualization on.

subset <- Total %>% 
          select(Extraversion, Popular, Gender)

head(subset)

# Check the datatype of all variables and see if they are correct.
# Which variable do you need to change?
subset <- as_tibble(subset)
subset
# --> Gender needs to be a factor and not an integer 

subset$Gender <- factor(subset$Gender, labels = c("Male", "Female"))

# Check distribution and scatterplots
ggpairs(subset)

# Based on the visualization so far, a linear model seems like a good initial 
# modeling choice (see the slides for a more thorough discussion of this). 

# For further visualization we need to fit our initial model and visualize
# further based on that.

# Run initial linear model with Popular as the DV and Extraversion and Gender
# as the IVs, and check t model using the check_model command (note that we
# don't look at model parameters yet, since we do not yet know if this model
# is really appropriate)
Regr1 <- lm(Popular ~ 1 + Extraversion + Gender, data = Total)                  
viz_check <- check_model(Regr1)
viz_check

# Note that we can get more information about influential cases and outliers
# by asking for this info explicitly

viz_check$INFLUENTIAL
viz_check$OUTLIERS

# --> Here everything is ok.

# You can also store the check_model(Regr1) plot in a PDF to improve 
# visualization
# pdf("check_model.pdf")
# check_model(Regr1)
# dev.off()

# Also check model assumptions when Extraversion is entered as an ordinal 
# variable
Regr1_o <- lm(Popular ~ 1 + ordered(Extraversion) + Gender, data = Total)        
check_model(Regr1_o)

# We decide to go for the regression model with Extraversion as a continuous    
# variable, because entering Extraversion as ordinal did not improve 
# diagnostics much (see the lecture for more info). However, homogeneity
# was not great for this data. As such we might opt to use robust SEs using the 
# vcovHC command

covHC0 <- vcovHC(Regr1)

# Using the HC covariance matrix covHC0 to test the coefficients of Regr1 with
# robust SEs, our final model results are.
coeftest(Regr1, vcov = covHC0)

#### Assignment 2---------------------------------------------------------------

# Alternatively, we could also deal with the heteroscedasticity in our data
# by actually modeling it. This is easy to do with Bayesian analysis.
# Let's assume that we expect based on visualization or prior knowledge that
# higher extraversion is associated with lower variability in popularity.
# In other words, the heterogeneity is based on Extraversion.

# Let's start with a Bayesian analysis without correction for heteroscedasticity
Regr1_Bayes <- brm(Popular ~ 1 + Extraversion + Gender, data = Total)
summary(Regr1_Bayes)
plot(Regr1_Bayes)
pp_check(Regr1_Bayes)

# And let's compare to the frequentist results.
summary(Regr1)

# As you'll notice the results of the frequentist and Bayesian analyses
# without corrections are pretty much similar, we can just take a more detailed
# look at results with the Bayesian analysis (create plots for parameters etc.)

# Now we'll also model a dependence of the residual variance on Extraversion
# This is easy to do, we just indicate that we want to regress sigma (the
# residual variance) on Extraversion as well in the our function call.

# Since we're basically doing two regression, one to predict the mean
# of popular and one to predict the SD, we'll combine both the models in
# one object using the bf command.
het_mod <- bf(Popular ~ 1 + Extraversion + Gender, sigma ~ Extraversion) 

# And then we'll run the model in which heteroscedasticity is predicted
# by extraversion.
Regr2_Bayes <- brm(het_mod, data = Total)
summary(Regr2_Bayes)
plot(Regr2_Bayes)
pp_check(Regr2_Bayes)

# We indeed see a negative effect of Extraverison on sigma (-0.07).

# Let's compare to the frequentist alternative again.
coeftest(Regr1, vcov = covHC0)
summary(Regr2_Bayes) 
# Results are again pretty similar, but with the Bayesian analyses we have more 
# insights into the cause of the heterogeneity because residuals are explicitly
# modeled.

######################### Part 2: Nested Data ##################################

#------------------------------------------------------------------------------# 
# Let's continue with the data on the popularity of highschool students. But
# now we'll start exploring and incorporating the nested nature of the data.
# The data of this study show a clear hierarchy - or nesting - , with students 
# nested in classes. Since observations of students from the same class are most 
# likely not independent, we can't just analyse these data using normal linear 
# regression. In addition, we are dealing with variables on different levels. 
# We have pupil level variables like Extraversion and Gender, but we also have 
# class characteristics like teacher experience. We can estimate both types of 
# variables in a single analysis only when using multilevel analysis. 
# In this assignment, we'll start with some regular regression analyses on the 
# data of each separate class separately because this will help you get the
# hang of R, but also because it will make the multilevel analyses more 
# insightful. Then, given the hierarchical nature of the data, we run a 
# multilevel regression analysis on the complete data.

# List of all the variables:
#- pupil:pupil identification variable
#- class:class identification variable
#- student-level independent variables: 
#   Extraversion (continuous; higher scores mean higher Extraversion) and 
#   Gender (dichotomous; 0=male, 1 =female)
#- class-level independent variables: 
#   teacher experience (in years)
#- outcome variable: 
#   popular (continuous; higher scores indicate higher popularity)

#------------------------------------------------------------------------------#

#### Assignment 1 --------------------------------------------------------------
# The data for the 12 different classes are stored in de the dataframes 
# Class1 - Class12. The dataframe Total contains the combined data of all 
# classes. Inspect the dataframes (maybe make a plot using the plot command. 
# For help, simply type ?plot).


# If you want to make scatterplots of Extraversion vs Popularity for all 
# classes in one go, you can use the code below.
layout(matrix(1:12, nrow = 3, ncol = 4))
for(i in 1:12){
  plot(get(paste("Class",i,sep=""))$Extraversion,
       get(paste("Class",i,sep=""))$Popular,
       main = paste("Class",i,sep=""),
       xlab = "Extraversion",
       ylab = "Popular")
}

# To do the same for Gender and Popularity we can use:
layout(matrix(1:12, nrow = 3, ncol = 4))
for(i in 1:12){
  plot(get(paste("Class",i,sep=""))$Gender,
       get(paste("Class",i,sep=""))$Popular,
       main = paste("Class",i,sep=""),
       xlab = "Gender",
       ylab = "Popular")
}

# And finally for Gender and Extraversion we can use:
layout(matrix(1:12, nrow = 3, ncol = 4))
for(i in 1:12){
  plot(get(paste("Class",i,sep=""))$Gender,
       get(paste("Class",i,sep=""))$Extraversion,
       main = paste("Class",i,sep=""),
       xlab = "Gender",
       ylab = "Extraversion")
}

# You can also use the ggpairs plots we used before on each class separately

subset_class1 <- Class1 %>% 
                 select(Extraversion, Popular, Gender)
subset_class1 <- as_tibble(subset_class1)
subset_class1$Gender <- factor(subset_class1$Gender, labels = c("Male", "Female"))
ggpairs(subset_class1)

#### Assignment 2 --------------------------------------------------------------
# Run regression analyses for each of the 12 classes separately using the lm 
# command, and save the intercepts and regression coefficients of each of these 
# analyses. In these analyses, use popularity as the dependent variable and 
# Extraversion and Gender as independent variables. If needed as for 
# instructions on using the lm command by typing ?lm in R. 

# The code for each separate analysis is given by,
ResultsClass1 <- lm(Class1$Popular ~ 1 + Class1$Extraversion + Class1$Gender)
# where you obviously change the name of the class you're interested in.
# You could also do:
ResultsClass1 <- lm(Popular ~ 1 + Extraversion + Gender,
                    data = Class1)
# The summary of the results can be obtained with
summary(ResultsClass1)

# The coefficients are stored in a separate part of the ResultsClass1 variable 
# called "coefficients". To access them separately ask for 
ResultsClass1$coefficients

# The intercept is the first value stored under coefficients and can be asked 
# for by typing
ResultsClass1$coefficients[1]

# Similarly, the regression coefficients for Extraversion and Gender can be 
# obtained by typing
ResultsClass1$coefficients[2] # and,
ResultsClass1$coefficients[3]

# The above can be done for each class separately, or you can save all 
# intercepts and regression coefficients using a for-loop.
Coefficients <- matrix(NA, ncol = 3, nrow = 12) # First we create a matrix in 
# which we store the intercepts and coefficients
# And then we specify the column names
colnames(Coefficients) <- c("Intercept", "Extraversion", "Gender") 
# Then we automatically run lm on all the 12 classes and store the results
for (i in 1:12) {
  Coefficients[i,] <- lm(Popular ~ 1+Extraversion+Gender, 
                         data = Total[Total$Class==i,])$coefficients
}

Coefficients 
# --> We can see that there are quite some differences between classes
# in intercepts, regression coefficients of Extraversion, and regression 
# coefficients of Gender. We can't test these differences for significance 
# however or predict them using class characteristics (this would require 
# multilevel analysis). They are purely descriptive.

#### Assignment 3 --------------------------------------------------------------
# Why can't we include teacher experience as a predictor in the separate 
# regression analyses above?

# --> Because the scores on teacher experience don't vary within classes.            
# To look at the effect of the class level predictor teacher experience, we need
# save the mean popularity score of each class and the teacher experience 
# scores. 

#### Assignment 4 --------------------------------------------------------------
# Save the mean popularity score of each class and the teacher experience 
# scores. Subsequently, run a regression in which we predict the mean 
# popularity using teacher experience (and save the intercept and regression 
# coefficient again). What is the sample size in this analysis?

# Getting the means can be done for each dataset by typing
mean(Class1$Popular) # and
mean(Class1$teacherExp)
# and then saving the values in two separate vectors
# Alternatively, we can again do this automatically. First we create a matrix in 
# which we store the intercepts and coefficients.
Mean <- matrix(NA, ncol = 2, nrow = 12) 
# And then we specify the column names.
colnames(Mean) <- c("Popular", "TeacherExperience") 
# Then, we automatically run lm on all the 12 classes and store the results.
for (i in 1:12) {
  Mean[i, 1] <- mean(eval(parse(text = paste("Class", i, sep = "")))$Popular)
  Mean[i, 2] <- mean(eval(parse(text = paste("Class", i, sep = "")))$teacherExp)
}

Mean <- as.data.frame(Mean)

# The regression analysis can subsequently be run using
ResultsLevel2 <- lm(Mean[,1]~1+Mean[,2]) # or
ResultsLevel2 <- lm(Mean$Popular ~ 1+ Mean$TeacherExperience)
summary(ResultsLevel2) # Note that there appears to be no effect of 
# teacher experience on popularity.

# --> The sample size is equal to the number of  classes; that is, 12.

#### Assignment 5 --------------------------------------------------------------
# Run a multilevel analysis in which you regress Popular on Gender and 
# Extraversion and you allow for between-class variance in the intercepts. Use 
# the lmer function. For help, open ?lmer(note that you will learn more details 
# about the multilevel analysis code in the next days).
# Compare the intercept and regression coefficients of Gender and Extraversion 
# from the multilevel analysis to the average estimates across the 12 separate    
# analyses. Are they the same? Why/Why not?

ResultsLmer_Gender_Ex <- lmer(Popular ~ 1 + Gender + Extraversion + (1 | Class), 
                              data = Total)
summary(ResultsLmer_Gender_Ex)

# You can calculate the mean intercept and regression coefficients by hand, but 
# if you stored them in a matrix, you can use the following code
apply(Coefficients, 2, mean, na.rm = TRUE)

# --> You see that the mean estimates across the separate analyses are not 
# identical to the multilevel estimates. This is because multilevel analysis 
# assumes that the inter-class differences in the intercept and coefficients are 
# normally distributed, whereas the estimates from the separate analyses don't 
# impose a distribution on the individual estimates. As a result the multilevel 
# estimates are "pulled" towards the mean parameter values across classes.

# What about the multilevel regression coefficient for teacherExp? Is it the 
# same as in the regression analysis you ran on the mean popularity and mean 
# teacherExp scores? Why/Why not?

ResultsLmer_teacherExp <- lmer(Popular ~ 1 + teacherExp + (1 | Class), Total)

summary(ResultsLmer_teacherExp)
summary(ResultsLevel2)

# --> No, the estimate for the effect of teacher Exp is not exactly the same. 
# This is because the classes are not all the same size. Some have more pupils
# than others. The multilevel estimates are weighted for these class-size 
# differences, while the regression analysis on the mean scores was not.    


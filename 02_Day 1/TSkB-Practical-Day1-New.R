#==============================================================================#
# Day 1: Practical Parts 1 & 2
#==============================================================================#

### Packages required for Practical --------------------------------------------

#install.packages("tidyverse")
#install.packages("report")
#install.packages("parameters")
#install.packages("performance")
#install.packages("see")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("mctest")
#install.packages("lmerTest")
#install.packages("sandwich")
#install.packages("lmtest")
#install.packages("sjPlot")
#install.packages("mvtnorm")
#install.packages("gridExtra")
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("lme4")
#install.packages("ICC")
#install.pachages("patchwork")
#install.packages("ggthemes")

library(report)
library(parameters)
library(performance)
library(see)
library(ggplot2)
library(GGally)
library(mctest)
library(lmerTest)
library(psych)
library(brms)
library(sandwich)
library(lmtest)
library(sjPlot)
library(mvtnorm)
library(gridExtra)
library(knitr)
library(kableExtra)
library(lme4)
library(ICC)
library(tidyverse)
library(ggthemes)

################# Part 1: Visualization & Modeling #############################

#------------------------------------------------------------------------------#
## Exercise 1a Visualisation
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# For this Assignment use the R-workspace ExerciseWorkspace.rdata

# These assignments are about a study on the popularity of highschool students. 
# A total of 246 students from 12 different classes took part in this study in 
# which we determined how extraversion, gender, and teacher experience 
# influenced a student's popularity.

# The data for the 12 different classes are stored in de the dataframes 
# Class1 - Class12. The dataframe Total contains the combined data of all 
# classes.

#List of all the variables:
#- pupil:pupil identification variable
#- class:class identification variable
#- student-level independent variables: 
#       extraversion (continuous; higher scores mean higher extraversion) and 
#       gender (dichotomous; 0=male, 1 =female)
#- class-level independent variables: 
#       teacher experience (in years)
#- outcome variable: 
#       popular (continuous: higher scores indicate higher popularity)
#------------------------------------------------------------------------------#

### Visualize data -------------------------------------------------------------

head(Total) # shows first lines of data
describe(Total, fast=TRUE) # descriptive statistics for all variables


### Check assumptions for relevant variables------------------------------------

# Only look at the actual variables, not the ID variables Class and Student by 
# removing the first 2 columns of the dataset

subset <- Total %>% 
          select(Extraversion, Popular, Gender)

head(subset)

# Check the datatype of all variables and see if they are correct?
# Which variable do you need to change?

subset <- as_tibble(subset)
subset

subset$Gender <- factor(subset$Gender, labels = c("Male", "Female"))

# Check distribution and scatterplots
ggpairs(subset)

# Also create the scatterplots with GGplot and create histograms too,
# compare to the density plots obtained above

b <- ggplot(Total, aes(x = Extraversion, y = Popular)) +  geom_point()
b + geom_smooth(method='lm')

ggplot(Total, aes(x = Extraversion)) + geom_bar()
ggplot(Total, aes(x = Popular)) + geom_bar()

# Also check for possible interactions, using the "fill" option from GGplot
c <- ggplot(Total, aes(x = Extraversion, y = Popular, col=factor(Gender))) +  
  geom_point()
c + geom_smooth(method='lm', se=FALSE)

ggplot(Total, aes(x = Extraversion)) + geom_bar(aes(fill=factor(Gender)))
ggplot(Total, aes(x = Popular)) + geom_bar(aes(fill=factor(Gender)))

# Run model checks using the check_model command
Regr1 <- lm(Popular ~ 1 + Extraversion + Gender, data = Total)
check_model(Regr1)

# Store the check_model(Regr1) plot in a PDF to improve visualization
pdf("check_model.pdf")
check_model(Regr1)
dev.off()

# Also check model assumptions when extraversion is entered as a factor or 
# ordinal variable
Regr1b <- lm(Popular ~ 1 + factor(Extraversion) + Gender, data = Total)
check_model(Regr1b)

Regr1c <- lm(Popular ~ 1 + ordered(Extraversion) + Gender, data = Total)
check_model(Regr1c)

# We decide to go for the regression model with Extraversion as a continuous 
# variable, but want to correct SE's just to check. Use the vcovHC command for 
# this
covHC0 <- vcovHC(Regr1)

# Use the HC covariance matrix from (1) to test the coefficients of Regr1 with
# robust SEs.
coeftest(Regr1, vcov = covHC0)

#------------------------------------------------------------------------------#
## Exercise 1b Violations
#------------------------------------------------------------------------------#

# Check effect of heteroscedasticity and outliers on results, and also 
# try some solutions to these issues.

### Heteroscedasticity ---------------------------------------------------------

# True effects are:
# Intercept; 4.5
# Extraversion: .4
# Gender: 1

# Plot data
ggplot(Total_HS, aes(x = Extraversion, y = Popular)) +  geom_jitter()

Regr1_hs <- lm(Popular ~ 1 + Extraversion + Gender, data = Total_HS)
check_model(Regr1_hs)
summary(Regr1_hs)

# Rerun model with robust SE's: What do you notice?
covHC0_hs <- vcovHC(Regr1_hs)
coeftest(Regr1_hs, vcov = covHC0_hs)

# Rerun analyses with Bayesian analysis
# First without correction for heteroscedasticity
Regr1_Bayes <- brm(Popular ~ 1 + Extraversion + Gender, data = Total_HS)
summary(Regr1_Bayes)
plot(Regr1_Bayes)
pp_check(Regr1_Bayes)

# Now also modeling the dependence of the residual variance on extraversion
het_mod <- brms::bf(Popular ~ 1 + Extraversion + Gender, sigma ~ Extraversion) 

Regr2_Bayes <- brms::brm(het_mod, data = Total_HS)
summary(Regr2_Bayes)

plot(Regr2_Bayes)
pp_check(Regr2_Bayes)

# What do you notice when you compare both Bayesian analyses against the
# frequentist ones?

summary(Regr1_hs) # frequentist model without correction for heterosc.
summary(Regr1_Bayes) # Bayesian model without correction for heterosc.
#--> just the same (both point estimates and SEs)

coeftest(Regr1_hs, vcov = covHC0_hs) # frequentist with correction for heterosc.
summary(Regr2_Bayes) # Bayesian model with correction for heterosc.
#--> more insights because residuals are modeled as function of extraversion

### Outliers -------------------------------------------------------------------

# First, plot the data!!!

# Compare original data to data with outliers added

Total %>% 
  ggplot(aes(x = Extraversion, y = Popular)) +
  geom_point() +
  stat_ellipse(geom = "polygon", alpha = .15, size = .15, level = .5) +
  stat_ellipse(geom = "polygon", alpha = .15, size = .15, level = .95) +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +
  coord_cartesian(xlim = c(-3, 4),
                  ylim = c(-2, 8)) +
  theme_fivethirtyeight() +
  theme(legend.position = "none")


Total_OL %>% 
  ggplot(aes(x = Extraversion, y = Popular, color = outlier, fill = outlier)) +
  geom_point() +
  stat_ellipse(geom = "polygon", alpha = .15, size = .15, level = .5) +
  stat_ellipse(geom = "polygon", alpha = .15, size = .15, level = .95) +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +
  coord_cartesian(xlim = c(-3, 4),
                  ylim = c(-2, 8)) +
  theme_fivethirtyeight() +
  theme(legend.position = "none")

# Check the model
Regr1_ol <- lm(Popular ~ 1 + Extraversion + Gender, data = Total_OL)
check_model(Regr1_ol)

# Check results, do you notice bias anywhere?
summary(Regr1) # without outliers
summary(Regr1_ol) # with outliers

# Try to fix analysis with robust SEs, what do you notice?
covHC0_ol <- vcovHC(Regr1_ol)
coeftest(Regr1_ol, vcov = covHC0_ol) # with outliers and robust cov.


# Redo the analyses again with Bayesian methods
# First a normal regression, how do results compare the the frequentist one?
Regr1_ol_Bayes <- brm(Popular ~ 1 + Extraversion + Gender, data = Total_OL)
summary(Regr1_ol_Bayes)

# Now use a t-distribution instead of a normal one, use the "family" option
# of the brm command for this.

# What do you notice?
Regr1_ol_Bayes2 <- brm(Popular ~ 1 + Extraversion + Gender, data = Total_OL,
                       family = student,
                       prior = c(prior(gamma(2, .1), class = nu)
                       ))

summary(Regr1_ol_Bayes2)

#------------------------------------------------------------------------------#
## Optional: Questions from BMLR Chapter 7
#------------------------------------------------------------------------------#

### 7.6 Scenario 1: No Covariates ----------------------------------------------

#### Answer questions 1 to 3. 

#### A1: Yes, each pup can be observed as an independent draw from the same 
####     process with p=.5.

#### A2: No! Pups don't all come from the same process anymore. Some come from 
####     the process with p=.5, some from a process with p=.75, and still others 
####     from a process with p=.25. So, some pups come from a process where the 
####     flip of a fair coin determines if they have a defect, but others are 
####     "assigned" defects by a coin that comes up heads 75% of the time or 
####     just 25% of the time.

#### A3: Yes, genetic variation can influence the probability of birth defects, 
####     as can age, and if dams differ with respect to these factors, the value 
####     of p will differ across them (although the range of variation in p does
####     not have to be as large as in this example).


#### Run the following code to answer questions 4 and 5: 

#### Fit model binomial---------------------------------------------------------
fit1 <- glm(cbind(count_1a, 10 - count_1a) ~ 1, # or use phat_1a
            family = binomial, 
            data = scenario_1) # for details, see Chapter 6
summary(fit1) # "~ 1" because it is an intercept only model
coef(fit1)[1] # 0.06669137 (intercept)

#### Calculate p_head manually
exp(coef(fit1)[1]) # = 1.068966 
1.068966/2.068966 # = 0.5166668
# p_head = = 0.5166668

#### Calculate p_head with function (only for intercept only model)
p_head_function <- function(x){x/(1+x)}
p_head_function(exp(coef(fit1)[1]))

#### Calculate CI = exp(parameter -/+ 1.96 * SE)
exp(coef(fit1)[1] - 1.96 * sqrt(vcov(fit1)))
exp(coef(fit1)[1] + 1.96 * sqrt(vcov(fit1)))

#### Or simply:
exp(confint(fit1))

#### Calculate CI for p_head
p_head_function(exp(coef(fit1)[1] - 1.96 * sqrt(vcov(fit1))))
p_head_function(exp(coef(fit1)[1] + 1.96 * sqrt(vcov(fit1))))

#### Fit model quasibinomial----------------------------------------------------

fit2 <- glm(cbind(count_1a, 10 - count_1a) ~ 1, 
            family = quasibinomial, 
            data = scenario_1) 
summary(fit2)
coef(fit2)[1] # 0.06669137 (intercept)


#### Calculate p_head with function (only for intercept only model)
p_head_function(exp(coef(fit2)[1]))

#### Calculate CI = exp(parameter -/+ 1.96 * ("dispersion * SE"))
exp(confint(fit2))

#### Calculate CI for p_head
p_head_function(exp(coef(fit2)[1] - 1.96 * sqrt(vcov(fit2))))
p_head_function(exp(coef(fit2)[1] + 1.96 * sqrt(vcov(fit2))))

#### Fit model binomial to data with dam effects -------------------------------
fit3_a <- glm(cbind(count_1b, 10 - count_1b) ~ 1, # or use phat_1b
              family = binomial, 
              data = scenario_1) # for details, see Chapter 6
summary(fit3_a) # "~ 1" because it is an intercept only model
coef(fit3_a)[1] # 0.268264  (intercept)
exp(coef(fit3_a)[1]) # = 1.307692 
1.307692 /2.307692  # = 0.5666666

#### Calculate p_head with function (only for intercept only model)
p_head_function(exp(coef(fit3_a)[1]))

#### Calculate CI = exp(parameter -/+ 1.96 * SE)
exp(confint(fit3_a))

#### Calculate CI for p_head
p_head_function(exp(coef(fit3_a)[1] - 1.96 * sqrt(vcov(fit3_a))))
p_head_function(exp(coef(fit3_a)[1] + 1.96 * sqrt(vcov(fit3_a))))


#### Fit model quasibinomial to data with dam effects ---------------------

fit3_b <- glm(cbind(count_1b, 10 - count_1b) ~ 1, 
              family = quasibinomial, 
              data = scenario_1) 
summary(fit3_b)
coef(fit3_b)[1] # 0.268264 (intercept)

#### Calculate p_head with function (only for intercept only model)
p_head_function(exp(coef(fit3_b)[1]))

#### Calculate CI = exp(parameter -/+ 1.96 * ("dispersion * SE"))
exp(confint(fit3_b))

#### Calculate CI for p_head
p_head_function(exp(coef(fit3_b)[1] - 1.96 * sqrt(vcov(fit3_b))))
p_head_function(exp(coef(fit3_b)[1] + 1.96 * sqrt(vcov(fit3_b))))


summary(fit1)
summary(fit2)
summary(fit3_a)
summary(fit3_b)

#### A4: The quasi-binomial allows for more variance than the binomial model. It 
####     assumes that the binomial model can't account for all variance in the 
####     data and therefore underestimates it. The quasi-binomial model adjusts 
####     for this underestimation. Because the quasi-binomial model assumes more 
####     variance, it assumes more uncertainty in the data, translating to 
####     larger SEs for parameters (i.e., less certainty in our parameter 
####     estimates) and wider CIs and higher p-values. Each CI does not 
####     necessarily contain the true value, but if we rerun the analysis 100 
####     times on 100 samples, 95% of the 95% CIs should contain the true model. 
####     In case of overdispersion, less than 95% of the 95% CIs from the 
####     binomial model will contain the true value.

#### A5: In the 1a model, there is less estimated variance than in the 1b model. 
####     Since the overdispersion correction is multiplied with SEs etc., the 
####     overdispersion correction is multiplied with a smaller number in the 1a
####     model than in the 1b model.

#------------------------------------------------------------------------------#
## 7.7 Scenario 2: Dose Effect
#------------------------------------------------------------------------------#

#### Answer questions 6 to 11. 

#### A6: Probabilities are .517 and .567 for scenarios 1a and 1b, and those 
####     probabilities are assumed to apply to every dam (no matter the dosage 
####     she received). Here we see that those are the probabilities that apply 
####     to an "average dosage" somewhere between dosage=1 and dosage=2. 

#### A7: The probability depends on Dose, so you want to know the probability 
####     for different doses. The average probability is the probability at a 
####     Dose=1.5, which is a score that doesn't exist. So it is hard to 
####     interpret.

#### A8: The intervals for the b-scenarios will always be wider than those of
####     the a-scenarios because more sources of variance are accounted for. 
####     Similarly, the quasi-binomial intervals will be wider than the binomial 
####     ones within a-scenarios and b-scenarios, again because more variance is 
####     accounted for. The point estimates will be the same between the 
####     binomial and quasi-binomial models, however, within each type of 
####     scenario.

#### A9: See the answer A4.

#### A10: See the answer A5.

#### A11: In scenario 2b, pups from the same dam are more alike than pups from 
####      different dams. Pups from a single dam have the same probability of 
####      deformation (which depends on the dosage that dam got and the 
####      characteristics of the dam herself). In contrast, pups from different 
####      dams have different probabilities of deformation.

######################### Part 2: Nested Data ##################################

#------------------------------------------------------------------------------#
## Exercise 1c Nested Data
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#
# These assignments are about a study on the popularity of highschool students.
# A total of 246 students from 12 different classes took part in this study in 
# which we determined how extraversion, gender, and teacher experience 
# influenced a student's popularity. The data of this study show a clear 
# hierarchy - or nesting - , with students nested in classes. Since observations
# of students from the same class are most likely not independent, we can't 
# just analyse these data using normal linear regression. In addition, we are 
# dealing with variables on different levels. We have pupil level variables 
# like extraversion and gender, but we also have class characteristics like 
# teacher experience. Only with multilevel analysis can we estimate both types 
# of variables in a single analysis. In this assignment we'll start with some 
# regular regression analyses on the data of each separate class (even though 
# the independence assumption is violated!). Because this will help you get the
# hang of R, but also because it will make the multilevel analyses more 
# insightful. Then, given the hierarchical nature of the data, we run a 
# multilevel regression analysis on the complete data.
#------------------------------------------------------------------------------#

#### Assignment 1 --------------------------------------------------------------
# The data for the 12 different classes are stored in de the dataframes 
# Class1 - Class12. The dataframe Total contains the combined data of all 
# classes. Inspect the dataframes (maybe make a plot using the plot command. 
# For help, simply type ?plot).

?plot


# If you want to make plots for all classes in one go, you can use the code 
# below.
layout(matrix(1:12, nrow = 3, ncol = 4))
for(i in 1:12){
  plot(get(paste("Class",i,sep=""))$Extraversion,
       get(paste("Class",i,sep=""))$Popular,
       main = paste("Class",i,sep=""),
       xlab = "Extraversion",
       ylab = "Popular")
}
dev.off()

# List of all the variables:
#- pupil:pupil identification variable
#- class:class identification variable
#- student-level independent variables: 
#   extraversion (continuous; higher scores mean higher extraversion) and 
#   gender (dichotomous; 0=male, 1 =female)
#- class-level independent variables: 
#   teacher experience (in years)
#- outcome variable: 
#   popular (continuous; higher scores indicate higher popularity)

#### Assignment 2 --------------------------------------------------------------
# Run regression analyses for each of the 12 classes separately using the lm 
# command, and save the intercepts and regression coefficients of each of these 
# analyses. In these analyses, use popularity as the dependent variable and 
# extraversion and gender as independent variables. If needed as for 
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
# called "Coefficients". To access them separately ask for 
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
                         data = eval(parse(text = 
                                    paste("Class", i, sep = ""))))$coefficients
  
}

Coefficients

#### Assignment 3 --------------------------------------------------------------
# Why can't we include teacher experience as a predictor in the separate 
# regression analyses above?

# Because the scores on teacher experience don't vary within classes            


#### Assignment 4 --------------------------------------------------------------
# For each class, save the mean popularity score and the teacher experience 
# score. Run a regression in which you predict the mean popularity using teacher 
# experience and save the intercept and regression coefficient. What is the 
# sample size in this analysis?

# Getting the means can be done for each dataset by typing
mean(Class1$Popular) # and,
mean(Class1$teacherExp)
# and then saving the values in two separate vectors
# Alternatively, we can again do this automatically. First we create a matrix in 
# which we store the intercepts and coefficients
Mean <- matrix(NA, ncol = 2, nrow = 12) 
# And then we specify the column names
colnames(Mean) <- c("Popular", "TeacherExperiece") 
# Then, we automatically run lm on all the 12 classes and store the results
for (i in 1:12) {
  Mean[i, 1] <- mean(eval(parse(text = paste("Class", i, sep = "")))$Popular)
  Mean[i, 2] <- mean(eval(parse(text = paste("Class", i, sep = "")))$teacherExp)
}

Mean

# The regression analysis can subsequently be run using
ResultsLevel2 <- lm(Mean[,1]~1+Mean[,2])
ResultsLevel2 <-lm(Mean$Popular ~ 1+ Mean$TeacherExperiece)
summary(ResultsLevel2)
# Note that the sample size is equal to the number of Classes; that is, 12.


#### Assignment 5 --------------------------------------------------------------
# Compare the intercept and regression coefficients of gender and extraversion 
# from a multilevel analysis to the average estimates across the 12 separate 
# analyses. Are they the same? Why/Why not?
ResultsLmer_Gender_Ex <- lmer(Popular ~ 1 + Gender + Extraversion + (1 | Class), 
                              data = Total)

summary(ResultsLmer_Gender_Ex)

# You can calculate the mean intercept and regression coefficients by hand, but 
# if you stored them in a matrix, you can use the following code
apply(Coefficients, 2, mean, na.rm = TRUE)

# You see that the mean estimates across the separate analyses are not identical
# to the multilevel estimates. This is because multilevel analysis assumes that 
# the inter-class differences in the intercept and coefficients are normally 
# distributed whereas the estimates from the separate analyses don't impose a 
# distribution on the individual estimates. As a result the multilevel estimates 
# are "pulled" towards the mean parameter value across classes.


# What about the multilevel regression coefficient for teacherExp? Is it the 
# same as in the regression analysis you ran on the mean popularity and mean 
# teacherExp scores? Why/Why not?

ResultsLmer_teacherExp <- lmer(Popular ~ 1 + teacherExp + (1 | Class), Total)

summary(ResultsLmer_teacherExp)
summary(ResultsLevel2)

# No, the estimate for the effect of teacher Exp is not exactly the same. This  
# is because the classes are not all the same size. Some have more pupils
# than others. The multilevel estimates are weighted for these class-size 
# differences while the regression analysis on the mean scores was not.    



#### Assignment 6 --------------------------------------------------------------
# Run a multilevel regression analysis on the data of all classes 
# simultaneously (use the "Total" dataframe for this) using the lmer command. 
# Add the first level variables extraversion and gender to the model as fixed 
# effects, and also add  the second level variable teacher experience

FixedEffects <- lmer(Popular ~ 1 + Gender + Extraversion +
                       teacherExp + (1 | Class), Total)
summary(FixedEffects)

#### Assignment 7 --------------------------------------------------------------
# Now allow the relation between the first level predictors and popularity to   
# not be the same across classes.
# What type of effect do you need to add? 
#
# You will notice an warning when running the code below, this has to do with
# the random effects in the model, can you figure out what the issue is?
#
RandomEffects <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                        (1 + Gender + Extraversion | Class), Total)
summary(RandomEffects)
rand(RandomEffects)

# Also run the model using brms. What warning do you get now? And can you figure
# out the problem using the pairs plots?

RandomEffects_Bayes <- brm(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                        (1 + Gender + Extraversion | Class), Total)
summary(RandomEffects_Bayes)
plot(RandomEffects_Bayes)

#pairs(RandomEffects_Bayes)
pairs(RandomEffects_Bayes, variable = c("sd_Class__Intercept", "b_Intercept"))
pairs(RandomEffects_Bayes, variable = c("sd_Class__Gender", "b_Gender"))
pairs(RandomEffects_Bayes, variable = c("sd_Class__Extraversion", "b_Extraversion"))

# Now run the model with the suggested djustement to adapt_delta, which
# makes the sampler more "fine grained" and check results. What is your
# conclusion

RandomEffects_Bayes2 <- brm(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                             (1 + Gender + Extraversion | Class), 
                           control = list(adapt_delta = 0.9), Total)
summary(RandomEffects_Bayes2)

# Both random slopes are non-significant/very small. Therefore, we do not have to add these
# effects to our model.

# Now check the normality of the errors in your final model. 
# Tip: Lookup the residual and ranef commands

# For level 1
hist(residuals(RandomEffects))
qqnorm(residuals(RandomEffects))

# For level 2
hist(ranef(RandomEffects)$Class[,1])
hist(ranef(RandomEffects)$Class[,2])
hist(ranef(RandomEffects)$Class[,3])

qqnorm(ranef(RandomEffects)$Class[,1])
qqnorm(ranef(RandomEffects)$Class[,2])
qqnorm(ranef(RandomEffects)$Class[,3])


#==============================================================================#
# Day 3: Practical Part 6
#==============================================================================#

# For the practical, you have to load Workspace-TSkB-Practical-Day3-Def.RData.

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
#install.packages("mlVAR")

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
library(mlVAR)
library(tidyverse)

#------------------------------------------------------------------------------#
### Exercise 6a Longitudinal Data-----------------------------------------------
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# The dataset "gpa"  contains longitudinal data set on 200 college students. 
# The students' grade point average (GPA) has been recorded for six successive 
# semesters. At the same time, it was recorded whether the student held a job 
# in that semester, and for how many hours. This is recorded in a variable 
# 'job' (= hours worked). Finally, we also have the student-level variables 
# high school GPA and gender (0 = male, 1 = female)
#------------------------------------------------------------------------------#

as_tibble(gpa)

#### Assignment 1 --------------------------------------------------------------
# Go through all the steps of a multilevel analysis (yes, even though you should
# use a maximum model). Calculate the ICC and the  amount of explained variance 
# in each step.

# Start with the intercept only model in which you use Popular as the dependent 
# variable, and calculate the ICC. Also determine if multilevel analysis is 
# necessary. If needed, ask for instruction using ?lme

IO <- lm(gpa ~ 1, gpa)
IO_ML <- lmer(gpa ~ 1 + (1 | student), gpa)
summary(IO_ML)
rand(IO_ML)# Or, instead of rand(), you can use:
anova(IO_ML,IO)

summary(IO_ML)
# The ICC is equal to 0.05714 /(0.05714 + 0.09759) = .3692
# We can check this with 
icc(IO_ML)

# Now, add the first level variable time to the model as a fixed effect. 
# What is the explained variance on level 1 and level 2? What "weird result" 
# do you notice? What could cause this? What is your conclusions about this 
# predictor?

Lvl1 <- lmer(gpa ~ 1 + time + (1 | student), gpa)
summary(Lvl1)
# Time  is significantly related to gpa.

# Now, we calculate the explained variances on Level 1 and 2:
VarianceIO <- as.data.frame(VarCorr(IO_ML))
VarianceLv1 <- as.data.frame(VarCorr(Lvl1))

# Explained Variance on Level 1                                                 #explained by time
(VarianceIO[2,4] - VarianceLv1[2,4])/ VarianceIO[2,4]
# .4047

# Explained Variance on Level 2
(VarianceIO[1,4] - VarianceLv1[1,4])/ VarianceIO[1,4]
# -.1152, explained variance can't be negative! This is because we have fixed
# measurement occasions! There is way less variance between students in 
# their time-scores than would be expected based on random sampling. Note
# that with random measurement occasions, this would not be a problem

# What should you do now?                                                       
# When adding more level 1 predictors, use the model WITH time as your reference 
# model, and NOT the intercept only model! The same holds for calculating the
# ICC.

# Calculate the ICC
summary(Lvl1)
# The ICC is equal to 0.06372/(0.06372 + 0.05809) = .5231                       #clarify: is the previous ICC incorrect then?
icc(Lvl1) # Always look at the "Adjusted ICC"                                   #add a "because"?

# Now, add the first level variable job to the model as a fixed effects. 
# What is the explained variance on level 1 and level 2?
# What is  your conclusions about this  predictor

Lvl1_2 <- lmer(gpa ~ 1 + time + job + (1 | student), gpa)
summary(Lvl1_2)
# job  is significantly related to gpa

VarianceLv1_2 <- as.data.frame(VarCorr(Lvl1_2))

# Explained Variance on Level 1                                                 #same as in lab 2: "additional" one? or the variance explained by job?
(VarianceLv1[2,4] - VarianceLv1_2[2,4])/ VarianceLv1[2,4]
# .049

# Explained Variance on Level 2
(VarianceLv1[1,4] - VarianceLv1_2[1,4])/ VarianceLv1[1,4]
# .172

# Now, add the second level variables to the model.
# What is the explained variance on level 2, and what are your 
# conclusions about these predictors?

FixedEffects <- lmer(gpa ~ 1 + time + job + sex + highgpa + admitted +
                       (1 | student), gpa)
summary(FixedEffects)
# All level 2 variables except highgpa are significantly related to gpa
VarianceFE <- as.data.frame(VarCorr(FixedEffects))

# Explained Variance on Level 2                                                 #add a not that reference could also be the model including all predictors.
(VarianceLv1[1,4] - VarianceFE[1,4])/ VarianceLv1[1,4]
# .6934

# Now, check if the relation between the first level predictors and gpa 
# is the same across students What type of effect do you need to add for test 
# this hypotheses? And is this effect significant?

# You need to add random slopes
RandomEffects <- lmer(gpa ~ 1 + time + job + sex + highgpa + admitted +
                              (1 + time + job | student), gpa)
rand(RandomEffects) 
summary(RandomEffects)# or extract the relevant results with
VarCorr(RandomEffects) #this gives you the random effects
coef(summary(RandomEffects)) #this gives you the fixed effects

# Both level 1 predictors have a random slope, but the amount of variance       #so it's a matter f sample size?
# across students is very small.

# Finally, should we add a cross-level interaction?
# You can! There are random slopes.   

# Run a final model where you use gender to explain differences in the slopes
# of time and job across students. What is your conclusion?
CrossLevel <- lmer(gpa ~ 1 + time + job + sex + highgpa + admitted +
                        time:sex + job:sex +
                        (1 + time + job | student), gpa)
summary(CrossLevel)

# There is a gender difference in the increase of gpa across time with girls    #say something on why ignoring the time/sex interaction?
# improving more. Gender does not influence the effect of job on gpa.

#### Assignment 2 --------------------------------------------------------------
# Check the assumptions for the model including cross-level 
# interactions.

# For level 1
hist(residuals(CrossLevel))
qqnorm(residuals(CrossLevel))

# For level 2
hist(ranef(CrossLevel)$student[,1])
hist(ranef(CrossLevel)$student[,2])
hist(ranef(CrossLevel)$student[,3])

qqnorm(ranef(CrossLevel)$student[,1])
qqnorm(ranef(CrossLevel)$student[,2])
qqnorm(ranef(CrossLevel)$student[,3])


# Now, let's approach this like we normally would. Start with figures.

# J-P and Niall plots
ggplot(data      = gpa,
       aes(x     = time,
           y     = gpa,
           col   = student,
           group = student))+ #to add the colors for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  geom_smooth(method = lm,                                                      #easier like this then estimating model first?
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  labs(title    = "GPA vs. Time",
       subtitle = "add colours for different students and regression lines")


# Some variation in time evident
ggplot(data      = gpa,
       aes(x     = job,
           y     = gpa,
           col   = student,
           group = student))+ #to add the colors for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  labs(title    = "GPA vs. Job",
       subtitle = "add colours for different students and regression lines")

# Something is clearly weird with job. It appears to be an ordinal variable 
# more than a continuous one. There is also some evidence of different slopes.  #check/why split?
# We've been assuming job has a monotone effect but let's do it properly

gpa$job_cat <- as.ordered(gpa$job)

#### Assignment 3 --------------------------------------------------------------
# Maximum approach: Run the full model in one go (using the ordinal variable 
# job_cat). Start with checking the assumptions, and then check for explained 
# variances etc. Does your conclusion differ from the one you drew based on 
# Assignment 1?

MaximumModel_Bayes <- brm(gpa ~ 1 + time + mo(job_cat) + sex + highgpa + 
                            admitted +(1 + time + mo(job_cat) | student), 
                          iter = 5000,
                          control = list(adapt_delta = 0.8), gpa)

# Check level 1 Residuals
hist(residuals(MaximumModel_Bayes))
qqnorm(residuals(MaximumModel_Bayes))

# Check level 2 Residuals
hist(ranef(MaximumModel_Bayes)$student[,,1][,1])
hist(ranef(MaximumModel_Bayes)$student[,,2][,1])
hist(ranef(MaximumModel_Bayes)$student[,,3][,1])

qqnorm(ranef(MaximumModel_Bayes)$student[,,1][,1])
qqnorm(ranef(MaximumModel_Bayes)$student[,,2][,1])
qqnorm(ranef(MaximumModel_Bayes)$student[,,3][,1])

# Looks okay. Now, look at results.
summary(MaximumModel_Bayes)

# Results are pretty similar to the lmer results, but there really is hardly 
# any variation in effect of job. Could now indeed at crosslevel interactions
# for time


#------------------------------------------------------------------------------#
## Exercise 6b AR-Residuals-----------------------------------------------------
#------------------------------------------------------------------------------#

# For speed purposes, let's only focus on time and highgpa for the following
# analyses :)

# First, let's run a normal growth model
GrowthModel_Bayes <- brm(gpa ~ 1 + time + highgpa +
                            (1 + time | student), 
                          iter = 2000,
                          control = list(adapt_delta = 0.8), gpa)

summary(GrowthModel_Bayes)


# And now one with AR residuals. What do you notice?

##Alternative but outdated code
#GMAR <- bf(gpa ~ 1 + time + highgpa + (1 + time | student), 
#           autocor = ~ ar(p = 1))

#GrowthModel_Bayes_ARRes <- brm(GMAR, iter = 2000,
#                            control = list(adapt_delta = 0.8), gpa)


GrowthModel_Bayes_ARRes <- brm(gpa ~ 1 + time + highgpa + ar(p = 1) +
                           (1 + time | student), 
                         iter = 2000,
                         control = list(adapt_delta = 0.8), gpa)

summary(GrowthModel_Bayes_ARRes)

# There is no mention of the AR at all!! Because it's considered a nuisance
#  It is there though!


mean(GrowthModel_Bayes_ARRes$fit@sim$samples[[4]]$`ar[1]`) # .08
sd(GrowthModel_Bayes_ARRes$fit@sim$samples[[4]]$`ar[1]`) # .08

# So we see there is little autocorrelation in this data (which makes sense 
# given the sampling frequency in the data). For Intensive longitudinal data
# the AR will tend to be higher :).

# If we want to explicitly add the AR effect, we need to lag the dependent
# variable and add it as a predictor.

gpa <- gpa %>%
       group_by(student) %>%
       mutate(gpa_L = lag(gpa, order_by=student))


GrowthModel_Bayes_AR <- brm(gpa ~ 1 + time + highgpa + gpa_L +
                                 (1 + time | student), 
                               iter = 5000,
                               control = list(adapt_delta = 0.8), gpa)

summary(GrowthModel_Bayes_AR)

# Note that the AR value here and in the previous model don't match....that's
# because we are fitting a very "weird"  called the ALT-model (and adding 
# level 2 predictors on top of that). In this model the interpretation of
# parameters is complicated! I wrote an article on it with Ellen Hamaker
# years ago, but main message is....be very careful when modeling systematic
# change and AR relationsships in one model, and maybe see a statistician if
# you really want to ;).


#------------------------------------------------------------------------------#
### Exercise 6c Intensive Longitudinal Data-------------------------------------
#------------------------------------------------------------------------------#

# Data on 2 variables for 100 individuals each measured 50 times.
# the means of the 2 variables are 4 and 4.5 respectively, with se's of .5.
# The correlation between the two variables is .3. The AR parameters are .4 and
# .3 respectively, while the two lagged effects are .2 and .1. All lagged
# parameters have se's of .1. The residuals are standard normally distributed

# Let's start with a growth curve model again

# This model takes a longtime to run, so the results are already in the
# R workspace

GrowthModel_Bayes_ARRes2 <- brm(Y1 ~ 1 + time + ar(p = 1) +
                                (1 + time | individual), 
                               iter = 2000,
                               control = list(adapt_delta = 0.8), VARData)

summary(GrowthModel_Bayes_ARRes2)

mean(GrowthModel_Bayes_ARRes2$fit@sim$samples[[4]]$`ar[1]`) # .51
sd(GrowthModel_Bayes_ARRes2$fit@sim$samples[[4]]$`ar[1]`) # .03

# Clearly there is no systematic change over time, but there is a substantial
# amount of autocorrelation. Since there is no trend, we can use AR models.
# If there were a trend, we would have to "get rid of it" first, by subtracting
# the predicted scores from the observed ones. Kind of like cnetering, but now
# with predicted scores (based on the regression line) instead of simple means.

# Now, let's use a simple AR model again. We'll use variable Y1 

AR1 <- brm(Y1 ~ Y1lag + (1 + Y1lag | individual), 
           iter = 5000, data = VARData)
summary(AR1)

# You'll see that the intercept isn't close to the actual value of 4!
# This is because the intercept in AR models can't be interpreted as a mean.
# If we want means, we need to group-mean center.

VARData <- VARData %>%
  group_by(individual) %>%
  mutate(Y1lag_c = Y1lag - mean(Y1lag, na.rm=T),
         Y2lag_c = Y2lag - mean(Y2lag, na.rm=T))


# Let's rerun the model with group-mean centered predictor now

AR1_c <- brm(Y1 ~ Y1lag_c + (1 + Y1lag_c | individual ), 
             iter = 5000, data = VARData)
summary(AR1_c)

# Much better ;). Although using sample means to group mean center isn't ideal
# We can use estimates of the individual means, but than we have to right
# our own code...or you should come see me ;)


# Now, we typically want to model several longitudinal variables at the same 
# time. This can be done with VAR models, which are  multivariate
# AR models.

# First, we create a model to model Y1 and Y2 simultaneously! We use the mvbind
# command for that. We also let the intercepts vary across individuals. We use
# the "p" notation because we want the random intercepts for both variables to
# also be correlated with each other. It's likely that if you score higher on 
# one, you score higher on the other for example.

bform1 <- 
  bf(mvbind(Y1, Y2) ~ Y1lag_c + Y2lag_c + (1|p|individual))

# This model takes some time to run, so the result is already included ;)
VAR <- brm(bform1, data = VARData, iter = 5000, chains = 2, cores = 2)
summary(VAR)

### Finally, let's look at longitudinal network models....and let's just use 
# our VAR data. This could be seen as a network of only two nodes after 
# all. Do you see a difference between the network and the VAR model?


LongNet <- mlVAR(VARData[, c(1:3)], vars = c("Y1", "Y2"), 
                 idvar = c("individual"), 
                 lags = 1, temporal = "correlated")

summary(LongNet)
plot(LongNet)
  
# The network model isn't doing "true" multilevel! It's calculating person 
# means based on the sample data and subtracting those from the observations
# to person-center the data. Then a network is fitted to the group-means to
# model level 2 (but without taking the amount of observations an individual 
# has into account. A mean of someone with 10 observations is treated the same
# as the mean of someone with 100 observations). The group-mean centered data
# is used to fit two models: a longitudinal network to lagged versions of the
# variables (think Y1lag and Y2lag) and a "within time point" or contemporaneous
# network to model relationships between variables at the same time point
# (e.g. Y1 at T1, on Y2 at T2).
# Can you tell where these 3 networks are located in the VAR output?
#
# Lagged is in the lagged effect
# Contemporaneous is the correlation between residuals
# Between is the correlation between intercepts/means


# The VAR model has some benefits over the network approach since it uses
# true multilevel analysis, and the parameters are easier to interpret (i.e.,
# we get information on "significance" etc), but it scales less nicely to 
# larger sample sizes and larger number of variables (although there are 
# ways around that). Networks also have nicer visualization out of the box.
# Which method you choose again comes down to modeling the same data in slightly
# different (but highly similar) ways, and which methods is best depends on
# you research question etc. Even if you go for a VAR model though, networks
# are always a good place to start because of their nice visualization of the
# data (in ways relevant to VAR models)
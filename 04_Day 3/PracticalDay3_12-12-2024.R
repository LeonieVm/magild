#==============================================================================#
# Day 3 Practical Parts 5 & 6
#==============================================================================#

# Load PracticaDay2.RData (don't forget to first set your working directory)
load("PracticalDay3.RData")

### Packages required for Practical --------------------------------------------

#install.packages("brms")
#install.packages("mlVAR")
#install.packages("tidyverse")

library(brms)
library(mlVAR)
library(tidyverse)

###################### Parts 5 & 6: Mean Change and AR #########################
#------------------------------------------------------------------------------#
## Part 5: Longitudinal Data--------------------------------------------------
#------------------------------------------------------------------------------#
# In intensive longitudinal data, observations are very close together in time.
# Because of that, confounders that impact a measurement occasions are probably
# going to influence some successive measurement occasions as well. To model 
# this, we can add AR relations between the residuals. In normal longitudinal 
# data, confounders are less likely to effect multiple measurements, but it is
# always good to check what the autocorrelation is.
# For speed purposes, let's only focus on time and highgpa for the following    
# analyses

# - Using the brm function, run a normal mean-change model with time and 
# highgpa as predictors (allow for random intercepts and time effects).

# - Run the same model again but also include AR residuals by adding 
# the term ar(p = 1) to the model. What do you notice?                          

# The models take a bit longer to run. Thus, the results are already in the 
# R workspace under the names `MeanChange_Bayes` and `MeanChange_Bayes_ARRes`. 
# The code to run the models would be:

# Mean change
# MeanChange_Bayes <- brm(gpa ~ 1 + time + highgpa +
#                            (1 + time | student), gpa) 

# Growth model with AR residuals
# MeanChange_Bayes_ARRes <- brm(gpa ~ 1 + time + highgpa + ar(p = 1) +
#                                  (1 + time | student), gpa)

# Note that you (most likely) get a warning from the Bayesian analyses as well 
# now (allthough occasionally things are fine, purely based on how the repeated 
# sampling in the Bayesian analysis happens to proceeds)! To see what's going 
# on, we'll ask for pairs plots that show us how parameters
# are related to each other.

pairs(MeanChange_Bayes_ARRes)

# There is a pretty strong relation between the intercept and the regression
# coefficient of highgpa, but no serious issues (which would show up as red 
# dots). If we make the estimation a bit more sensitive and run it for a little 
# more iterations, things will probably be fine. The model results are already 
# stored in `MeanChange_Bayes_ARRes`, but the code would be:

# MeanChange_Bayes_ARRes_upd <- brm(gpa ~ 1 + time + highgpa + ar(p = 1) +
#                               (1 + time | student), iter = 4000,
#                               control = list(adapt_delta = 0.9), gpa)

# We only have a warning about treedepth left. This is ok since it's not a 
# warning about validity but about efficiency. We could make the estimation
# more efficient, but it's not needed so we'll continue.

summary(MeanChange_Bayes)
summary(MeanChange_Bayes_ARRes_upd)                                                

# --> The AR parameter is reported under 'Correlation Structures'.     
# All results are very similar. This is because there isn't a lot of 
# autocorrelation (the AR parameter is only .07).
# So, we see there is little autocorrelation in this data (which makes sense 
# given the sampling frequency in the data). For Intensive longitudinal data
# the AR will tend to be higher.
# If we want to explicitly model the AR effect, we need to lag the dependent    
# variable and add it as a predictor. However, then you should remove time
# from the model. Otherwise, you end up with a very complicated model
# (the ALT-model), which is hard to interpret and therefore seldom used
# in practice. Instead, if there is a visible time-effect, you should first
# de-trend the data (which is just another way to control for time than 
# including it as a predictor). 
# There are different ways of de-trending the data. One could
# run a multilevel regression model with time as the only predictor and
# save the residuals. These residuals are the detrended data and can then be
# analyzed using the AR model. Another method calculates successive differences 
# between observed scores after which these differences can be used as 
# detrended data for the AR model. Finally, more advanced models would include
# the time-trend and AR part simultaneously in a way that the detrending and
# AR modeling of the detrended data is being done simultaneously. 

#------------------------------------------------------------------------------#
### Part 6: Intensive Longitudinal Data --------------------------------------
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# For the final analyses, we will use the following intensive longitudinal data:
# Self-generated data on 2 variables (Y1 and Y2) for 100 individuals each 
# measured 50 times. The means of the 2 variables are 4 and 4.5 respectively, 
# with SEs of .5. The correlation between the two variables is .3. The AR 
# parameters are .4 and .3 respectively, while the two cross-lagged effects are 
# .2 and .1. All cross-lagged parameters have SEs of .1. The residuals are 
# standard normally distributed. Note that the lagged version of variable Y1 and
# Y2 are also included in the data (Y1lag and Y2lag).
#------------------------------------------------------------------------------#

#### Assignment 1 --------------------------------------------------------------
# Run a multilevel model with time as a predictor and include AR residuals.                                     

# This model also takes a long time to run, so the results are already in the
# R workspace (MeanChange_Bayes_ARRes2). 
# Is there systematic change over time? What is the amount of autocorrelation?

# The code to run the model would be
# MeanChange_Bayes_ARRes2 <- brm(Y1 ~ 1 + time + ar(p = 1) +                   
#                                 (1 + time | individual), iter = 4000,
#                                 control = list(adapt_delta = 0.9), VARData)

summary(MeanChange_Bayes_ARRes2)


# --> Clearly there is no substabtial systematic change over time, but there is           
#     a substantial amount of autocorrelation (ar[1] = .51 (.02)). Since there 
#     is no trend, we can use AR models. If there were a trend, you should 
#     control for it using one of de-trending methods mentioned above.


#### Assignment 2 --------------------------------------------------------------
# We continue with an AR model.

# - Run the regular AR model. Is the intercept close to the true value of 4?

# - Rerun the model with person-mean centered predictor. Is the intercept close 
# to the true value of 4?

# Again, to save time, the model results are already stored in the R workspace 
# (`AR1` and `AR1_c`). The code to run the models is below.

# Regular AR model
# AR1 <- brm(Y1 ~ Y1lag + (1 + Y1lag | individual), 
#            iter = 5000, data = VARData,
#            control = list(adapt_delta = 0.9))

summary(AR1)
# --> No. This is because the intercept in AR models can't be interpreted as 
#     a mean. If we want means, we need to group-mean center.

# AR model with person-mean centered predictor
VARData <- VARData %>%
  group_by(individual) %>%
  mutate(Y1lag_c = Y1lag - mean(Y1lag, na.rm=T),
         Y2lag_c = Y2lag - mean(Y2lag, na.rm=T))

# AR1_c <- brm(Y1 ~ Y1lag_c + (1 + Y1lag_c | individual ), 
#              iter = 5000, data = VARData,
#              control = list(adapt_delta = 0.9))
summary(AR1_c)

# --> Yes. Much better. Note, however, that using sample means to person-mean   
# center isn't ideal since it doesn't acknowledge the uncertainty in the
# means. "Latent-mean" centering (where means are treated as random variables
# with distributions and thus uncertainty) is preferred. This is not 
# available in R. We would have to write our own code (e.g., see the following 
# STAN code
# https://experienced-sampler.netlify.app/post/stan-hierarchical-ar/#model) or
# use Mplus.

#### Assignment 3 --------------------------------------------------------------
# We typically want to model several longitudinal variables at the same 
# time. This can be done with VAR models, which are multivariate
# AR models. Just go through the following code and see if you understand it.   

# First, we create a model to model Y1 and Y2 simultaneously! We use the mvbind
# command for that. We also let the intercepts vary across individuals. We use
# the "p" notation because we want the random intercepts for both variables to
# also be correlated with each other. It's likely that if you score higher on 
# one, you score higher on the other for example.

bform1 <- 
  bf(mvbind(Y1, Y2) ~ Y1lag_c + Y2lag_c + (1|p|individual))                     

# The following model takes some time to run, so the results are again already
# included under `bform1`. The code is:
# VAR <- brm(bform1, data = VARData, iter = 5000, chains = 2, cores = 2,
#           control = list(adapt_delta = 0.9))
summary(VAR)

#### Assignment 4 --------------------------------------------------------------

# Finally, we look at longitudinal network models. We just use 
# our VAR data. 
# The network model isn't doing "true" multilevel! It's calculating person 
# means based on the sample data and subtracting those from the observations
# to person-center the data. Then, a network is fitted to the group-means to
# model level 2 (but without taking the amount of observations an individual 
# has into account. A mean of someone with 10 observations is treated the same
# as the mean of someone with 100 observations). The group-mean centered data
# is used to fit two models: a longitudinal network to lagged versions of the
# variables (think Y1lag and Y2lag) and a "within time point" or contemporaneous
# network to model relationships between variables at the same time point
# (e.g. Y1 at T1, on Y2 at T2).

# - Go through the code and see if you understand it.

# - Can you tell where the 3 networks described above are located in the VAR 
# output?

# - Do you see a difference between the results of the network and the VAR model?

# Also the results for this model are already stored in the R workspace 
# (`LongNet`). The code is:

# Network model
# LongNet <- mlVAR(VARData[, c(1:3)], vars = c("Y1", "Y2"), 
#                  idvar = c("individual"), 
#                  lags = 1, temporal = "correlated")

summary(LongNet)
plot(LongNet)

# Recall the VAR model
summary(VAR)

# --> The Temporal effects are the lagged effect; the contemporaneous network is
# the correlation between residuals; the between network is the correlation 
# between intercepts/means.

# --> The results of both analyses are very similar, but with the networks
# no inferences (e.g., significance tests) are available.


# Final remark on VAR modeling vs. the network approach:                        
# The VAR model has some benefits over the network approach since it uses
# true multilevel analysis, and the parameters are easier to interpret (i.e.,
# we get information on "significance" etc.), but it scales less nicely to 
# larger sample sizes and larger number of variables (although there are 
# ways around that). Networks also have nicer visualization out of the box.
# Which method you choose again comes down to modeling the same data in slightly
# different (but highly similar) ways, and which methods is best depends on
# you research question etc. Even if you go for a VAR model though, networks
# are always a good place to start because of their nice visualization of the
# data (in ways relevant to VAR models).
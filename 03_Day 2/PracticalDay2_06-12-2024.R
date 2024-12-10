#==============================================================================#
# Day 2: Practical Parts 3 and 4
#==============================================================================#

# For the three practical parts, load PracticaDay2.RData (don't forget to
# first set your working directory)
load("PracticalDay2.RData")

### Packages required for Practical --------------------------------------------
library(lmerTest)# important because otherwise you don't get p-values!
library(brms)
library(tidybayes)
library(dplyr)

###### Part 3: Assumptions, Data Centering, Complete Multilevel Analysis  ######

#------------------------------------------------------------------------------#  
# The assignments for Part 3 are about a study on the popularity of highschool 
# students. A total of 246 students from 12 different classes took part in this 
# study in which we determined how extraversion, gender, and teacher experience 
# influenced a student's popularity.

# The data for the 12 different classes are stored in de the dataframes 
# Class1 - Class12. The dataframe Total contains the combined data of all 
# classes.

#List of all the variables:                                                     
#- pupil:pupil identification variable
#- class:class identification variable
#- student-level independent variables: 
#       extraversion (continuous; higher scores mean higher extraversion) and 
#       gender (dichotomous; 0 = male, 1 = female)
#- class-level independent variables: 
#       teacher experience (in years)
#- outcome variable: 
#       popular (continuous: higher scores indicate higher popularity)
#------------------------------------------------------------------------------#

#### Assignment 1 --------------------------------------------------------------
# Run a Multilevel Analysis with both the level-1 and level-2 predictors, and a 
# random effects for the level-1 predictors.

# As always we should start with exploring and visualizing our data, but since
# we already visualized this data on Day 1, and to keep the assignment short,
# we will only look at the assumptions of normality of errors and linearity 
# here. The reason is that these are the two assumptions that require some more 
# work in a multilevel analyses since we have multiple residual (on level 1 and 
# level 2) and have relationships at both level 1 and level 2. So:

# - Check the normality of the errors for this model.
# Tip: Look up the residual and ranef commands

# - Check the assumptions of linearity for all predictors. Tip: Remember what 
# the level-2 DV is, and see the assignment from day 1 on how to calculate it. 


# Running the multilevel regression analysis
Regr6_ML <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                   (1 + Gender + Extraversion | Class), data = Total)

# Note that you get a warning message with this analysis. As mentioned, 
# multilevel models can get quite complex quickly which can be an issue for 
# frequentist analyses. In addition, frequentist analysis is not great at 
# indicating if a problem is a big problem or not. We therefore do not know
# if we can trust our results now. 

# Bayesian analysis performs better in this regard. It can handle complex models
# more easily and clearly tells us if there is an important issue (and allows
# for diagnosing the problem and adjusting the calculations accordingly). We 
# will therefore also run a Bayesian analysis on these data.

Regr6_Bayes <- brm(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                   (1 + Gender + Extraversion | Class), data = Total)

# The Bayesian analysis finishes without issues. Comparing the frequentist
# and Bayesian results allows us to see if the frequentist results are also
# reliable.

summary(Regr6_ML) 
summary(Regr6_Bayes) 

# Results are quite close although the standard errors (SEs) differ a bit. 
# In the frequentist analysis, for example, the intercept has an SE of 0.39, 
# while in the Bayesian analysis, this SE is equal to .56. Other SEs are quite 
# similar. We can do two things now: 1) conclude the frequentists analysis is 
# reliable enough and continue with it, or 2) continue with the Bayesian 
# analysis. To decide, let's see if we can figure out why we get the warning 
# with the frequentists analysis. As mentioned, Bayesian analysis let's you 
# explore your model and its compuation in more detail, so let's look at the 
# distributions of the model parameters from the Bayesian analysis.

plot(Regr6_Bayes)

# Note that between class differences in Gender (the between class SD of Gender,
# sd_Class_Gender) is very skewed and has a lot of values really close to 0.
# This is likely the cause of the warning with the frequentist analysis. SDs
# do not get smaller than 0, in statistical terms, 0 is a boundary value for a
# SD. Frequentists analyses do not like parameter values close to boundaries
# (0 for SDs/variances, +/- 1 for correlations) and give warnings when this 
# happens. From our visualizations, we know that the proportion of males and 
# females is quite equal in all classes, so a small between-class SD for this 
# variable makes sense here. Because of that, we decide that continuing with the
# frequentist analysis is also a good option, and we move on to checking 
# normality and linearity for this analysis type.

# Checking the normality of the errors
# For level 1
hist(residuals(Regr6_ML))
qqnorm(residuals(Regr6_ML))
# --> looks fine                                                             

# For level 2
ranef(Regr6_ML) # gives the random effects

hist(ranef(Regr6_ML)$Class[,1]) # intercept                                     
hist(ranef(Regr6_ML)$Class[,2]) # gender
hist(ranef(Regr6_ML)$Class[,3]) # extraversion

qqnorm(ranef(Regr6_ML)$Class[,1]) # intercept
qqnorm(ranef(Regr6_ML)$Class[,2]) # gender
qqnorm(ranef(Regr6_ML)$Class[,3]) # extraversion

# --> these look okay. The histograms look non-normal but the qq-plots are 
# decent. Remember that we have a very small sample size on level 2 of only 12 
# classes. Because of this, the figures will always look less nice. Since the 
# qq-plots look okay and since the analyses are quite robust against some 
# violation of the normality assumption, we conclude that this is all fine.

# Checking the assumptions of linearity
# We already did this on day 1 with ggpairs, but can also use separate scatter
# plots to test this assumption, especially if the number of predictors, and 
# therefore the number of plots, is not too large. In case of many predictors
# looking at the residual for violations of linearity for your model as a whole
# is easier.

# For level 1
plot(Total$Extraversion, Total$Popular)                                         
plot(Total$Gender, Total$Popular)                                               

# For level 2
# First we create a matrix in which we store the intercepts and coefficients
Mean <- matrix(NA, ncol = 2, nrow = 12) 
colnames(Mean) <- c("Popular", "TeacherExperience") 

# Then, we automatically run lm on all the 12 classes and store 
# the results.
for (i in 1:12) {
  Mean[i, 1] <- mean(eval(parse(text = paste("Class", i, sep = "")))$Popular)
  Mean[i, 2] <- mean(eval(parse(text = paste("Class", i, sep = "")))$teacherExp)
}

plot(Mean[,2],Mean[,1])       

# Because of the small level-2 sample size, the plot again does not look great,
# but there is also no clear non-linear trend visible, so we conclude that all
# is fine.

#### Assignment 2 --------------------------------------------------------------
# Grand center the continuous predictors and rerun the model. What do you 
# notice when comparing the results to the model with the uncentered predictors?

# Grand center the continuous level-1 predictor
Total$Extraversion_c <- Total$Extraversion - mean(Total$Extraversion)

# Grand Center the level-2 predictor
# We're going to do this in two steps, because we can't use the raw teacherExp 
# scores. The reason for that is that each student has a teacherExp score in 
# the datafile, while teacherExp obviously is a class characteristic. So we'll 
# first determine the average teacherExp for each class (which is just the 
# teacherExp score for that class), and then calculate the average of those 
# class-scores for centering.
Teacher_Exp <- matrix(NA, ncol = 1, nrow = 12)

for (i in 1:12) {
  Teacher_Exp[i,] <- mean(Total$teacherExp[Total$Class==i])
}

Teacher_Exp
mean(Teacher_Exp)

Total$TE_c <- Total$teacherExp - mean(Teacher_Exp)

# Rerun the model
Regr6_ML2 <- lmer(Popular ~ 1 + Gender + Extraversion_c + TE_c + 
                    (1 + Gender + Extraversion_c | Class), data = Total)
summary(Regr6_ML2)

# Compare to the results of the model with the uncentered predictors
summary(Regr6_ML)

# Or to make things easier compare the fixed and random effects separately
coef(summary(Regr6_ML)) #this gives you the fixed effects estimates
coef(summary(Regr6_ML2))

VarCorr(Regr6_ML) #this gives you the random effects Std.Dev
VarCorr(Regr6_ML2)

# Only the intercept and its SE change because of the centering.
# This makes sense since the intercept is the predicted score for someone with
# a score of 0 on the predictors, and grand centering just changes where the 0 
# is on a predictor. Everything else including the random effects stay the same.

# Also note, at the bottom of the results there is a section called
# 'Correlation of Fixed Effect', and there numbers change a bit too.
# You can ignore this output. It shows you how the different parameter values
# are related over repeated samples. So if I take 100 samples and fit the model
# each time (and therefore get 100 intercepts and slope estimates), this part of
# the output gives me the correlations between the sets of 100 different 
# parameter values. This is not information you typically use or is meaningful
# so we don't look at it in this course. The fact that the numbers here change
# is also simply because our intercept changes because of centering.


#### Assignment 3 --------------------------------------------------------------
# Go through all the steps of a multilevel analysis. Calculate the ICC in step 1
# and the amount of explained variance in each step.

##### Step 1--------------------------------------------------------------------

# Start with the intercept only model in which you use Popular as the dependent 
# variable, and calculate the ICC. Also determine if multilevel analysis is 
# necessary. If needed ask for instruction using ?lme.

IO <- lm(Popular ~ 1, Total)
IO_ML <- lmer(Popular ~ 1 + (1 | Class), Total)
rand(IO_ML)# Or, instead of rand(), you can use:
anova(IO_ML,IO)

summary(IO_ML)
# --> the ICC is equal to 0.223/(0.223 + 1.016) = 0.18

##### Step 2--------------------------------------------------------------------
# Add the first level variables extraversion and gender to the model as 
# fixed effects. 

# - What is the explained variance on level 1 and level 2, what "weird result" 
# do you notice? What could cause this?

# - What are your conclusions about the predictors?

Lvl1 <- lmer(Popular ~ 1 + Gender + Extraversion + (1 | Class), Total)

summary(Lvl1)
# --> Both predictors are significantly related to Popularity (do not forget to 
#load the library 'lmerTest' to see the p-values)          

# Now, we calculate the explained variances on level 1 and 2:
VarianceIO <- as.data.frame(VarCorr(IO_ML))
VarianceLv1 <- as.data.frame(VarCorr(Lvl1))

# Explained variance on level 1
(VarianceIO[2,4] - VarianceLv1[2,4])/ VarianceIO[2,4]
# --> .5183

# Explained variance on level 2
(VarianceIO[1,4] - VarianceLv1[1,4])/ VarianceIO[1,4]
# --> -.2370, explained variance can't be negative! The level-1 explained 
# variance is .5183. The level-2 explained variance is -.2370. Explained 
# variance can't be negative! In a multilevel model, the total variance 
# (at level-1 and level-2) is initially split based on assumptions of random 
# sampling—for instance, that schools are randomly drawn and students within 
# them are also randomly drawn. The model thus expects random variation in 
# level-1 predictors (like gender proportions) across groups (e.g., classes). 
# However, when adding a level-1 predictor that doesn’t show this expected
# random variation (e.g., all classes have an even boy/girl split due to 
# sampling design), the model 'notices' this and reallocates some variance 
# initially attributed to level-1 (in the intercept-only model) to level-2 
# (in the model with gender). This happens because the total variance that must 
# be distributed between level-1 and level-2 remains constant, so any 
# unaccounted variance shifts to level-2. As a result, the level-2 variance 
# appears artificially inflated after including a non-random level-1 predictor, 
# creating the artifact of negative explained variance. The best approach in 
# this situation is to disregard changes in level-2 R-squared and continue with 
# the predictor in the model, using it for comparisons in subsequent steps as 
# usual.

##### Step 3--------------------------------------------------------------------

# Add the second level variable teacher experience to the model.

# - What are your conclusions about this predictor?

# - What is the explained variance on level 1 and level 2?

FixedEffects <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                       (1 | Class), Total)
summary(FixedEffects)
# --> Teacher experience is not significantly related to popularity.

VarianceFE <- as.data.frame(VarCorr(FixedEffects))

# Explained variance on level 1 (compared to model with just level-1 predictors)
(VarianceLv1[2,4] - VarianceFE[2,4])/ VarianceLv1[2,4]
# --> 0. No explained variance since you only added a level-2 predictor

# Explained variance on level 2 (compared to model with just level-1 predictors)                                   
(VarianceLv1[1,4] - VarianceFE[1,4])/ VarianceLv1[1,4]
# --> .1662

##### Step 4--------------------------------------------------------------------
# Check if the relation between the first-level predictors and popularity 
# is the same across classes by allowing for random effects (one by one). 
# Are the effects significant?

# Add random slopes one by one
RandomEffectsGender <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                              (1 + Gender | Class), Total)
anova(RandomEffectsGender, FixedEffects)

RandomEffectsExtraversion <- lmer(Popular ~ 1 + Gender + Extraversion + 
                                    teacherExp + 
                                    (1 + Extraversion | Class), Total)
anova(RandomEffectsExtraversion, FixedEffects)

# --> Neither of the two level-1 predictors has a significant random slope.

##### Step 5--------------------------------------------------------------------
# Finally, decide if you need to add a cross-level interaction?
# No! There is no random slope, so you should not add a cross-level 
# interaction! There are no differences to explain so adding a predictor will
# lead to unintepretable nonsense results.

# To get some intuition about what happens if you do add a cross-level 
# interaction when there is no random slope, consider the following:

# If there is no random slope, we say that the slope has the exact same value
# for each class (for example, 0.4 for each class). If we add a level-2
# predictor for the slope even though there is no between class variance, we 
# basically try to rewrite the value 0.4 as a mathematical equation, comparable
# to saying that 0.4 is equal to 0.1 + 0.3. Instead of a simple equation like
# 0.4 = 0.1 + 0.3 we use the way more complex equation 
# 0.4 = intercept + regression_coefficient*level2_predictor + residual. But the 
# basic idea is the same: we try to add things together (in our case, the 
# intercept, the term regression_coefficient*predictor, and the residual) in 
# such a way that they result in the fixed value 0.4. The intercept and 
# regression coefficient are therefore estimated in such a way to make this 
# equation actually add up to 0.4, even though that does not make any sense. 
# This will likely lead to values for the intercept and regression coefficient 
# that are different from 0 and thus "significant". But they are not significant 
# because there is actual relation between the level-2 predictor and the level-1 
# slope! After all, there was no between class difference in the level-1 slope 
# so there is nothing to be predicted by the level-2 predictor. It's just 
# non-sense.

# Our final model is thus the model from step 2:
# Lvl1 <- lmer(Popular ~ 1 + Gender + Extraversion + (1 | Class), Total)

#### Assignment 4 --------------------------------------------------------------
# In Assignment 3, we built the model step by step. As mentioned in 
# the lecture, this is one way of running an analysis. Alternatively, you can 
# run the full model immediately. This is called the maximum approach.
# Run the full model in one go. Does your conclusion differ from the conclusion
# you drew based on Assignment 3?

MaximumModel <- lmer(Popular ~ 1 + Gender + Extraversion +                      
                       teacherExp + 
                       (1 + Gender + Extraversion | Class), Total)


# The maximum approach can be difficult for frequentist estimation because of
# the complexity of the model, and again we get a warning here. Bayesian 
# analyses may be preferred with this approach therefore since Bayesian 
# estimation is more stable, and if there is a problem, Bayesian analysis makes
# it's easier to pinpoint.

MaximumModel_Bayes <- brm(Popular ~ 1 + Gender + Extraversion + 
                            teacherExp + 
                            (1 + Gender + Extraversion | Class), Total)


summary(MaximumModel)
summary(MaximumModel_Bayes)

# As before the reason for the warning with the frequentists analysis is the
# small between class variance/SD for Gender and the results are quite similar
# between the two analyses (with the exception of the SE of the intercept). We
# therefore continue with the frequentist analysis again.

# Results show that there is no main effect of Teacher Experience just as we
# found in the step-wise approach. 

# To test the random effects, you can use the rand() command.
rand(MaximumModel)

# Results show that both there is no random slope for either Gender (p = .836) 
# or Extraversion (p = .330), also exactly like we found in the step-wise
# approach.

# So with both methods we would end up with the same final model.
# A model with fixed effects for the level-1 predictors and no random slopes:

# Lvl1 <- lmer(Popular ~ 1 + Gender + Extraversion + (1 | Class), Total)


#### Assignment 5 --------------------------------------------------------------
# With the Bayesian analysis, we can extract distributions of 
# random-intercept and random-slope SDs and based on these decide
# whether the variance is negligible or not.

# - Extract entire distribution of the random-intercept SD and check the
# probability that the intercept SD is smaller than the smallest
# value of interest (which would be based on theoretical or clinical insights, 
# e.g., .02). Is the variance negligible?  

# - Extract entire distribution of the random-slope SD for gender. Again, 
# check the probability that the SD is smaller than the smallest
# value of interest (e.g., .02). Is the variance negligible?

# - Extract entire distribution of the random-slope SD for extraversion. 
# Again, check the probability that the SD is smaller than the 
# smallest value of interest (e.g., .02). Is the variance negligible?

get_variables(MaximumModel_Bayes)# Get names of parameters in the model

# random-intercept variance
SdInt<- MaximumModel_Bayes %>%
  spread_draws(sd_Class__Intercept)

sum((SdInt$sd_Class__Intercept)^2 < .02)/length(SdInt$sd_Class__Intercept)
# --> There is a 0% chance that the intercept-variance is smaller than .02, 
# so there is definitely variance in the intercept that should be considered
# in further analyses.

# random-slope variance for gender
SdGender <- MaximumModel_Bayes %>%
              spread_draws(sd_Class__Gender)

sum((SdGender$sd_Class__Gender)^2 < .02)/length(SdGender$sd_Class__Gender)
# --> There is approximately a 67% chance that the slope-variance is smaller 
# than .02. Thus, the random-slope variance is very small and does not have
# to be considered in further analyses.

# random-slope variance for extraversion
SdExtra <- MaximumModel_Bayes %>%
              spread_draws(sd_Class__Extraversion)

sum((SdExtra$sd_Class__Extraversion)^2 < .02)/
  length(SdExtra$sd_Class__Extraversion)
# --> There is approximately a 40% chance that the slope-variance is smaller 
# than .02. Thus, the random-slope variance is very small and does not have
# to be considered in further analyses.                                                               

################ Part 4: Small level-2 N: Fixed Effects Models #################

#### Assignment 1 --------------------------------------------------------------
# So far we've been fitting multilevel models to the data of our running example
# but we do not have a very large level-2 sample size (only 12 classes).
# Because of this, using multilevel analysis might not be the best approach.  
# Estimating level-2 variances requires a decent sample size on level 2 after 
# all.

# - Analyse the data using a fixed effects model that is analogous to our final
# multilevel above (a model with main effects of the level-1 predictors and no
# random slopes). Why do you have to include a "0" in the equation after 
# the "~" sign?

# - Also compare the individual intercepts of the multilevel model to the dummy 
# scores for the full model only from the fixed effect model. What do you 
# notice?

# Fixed effects model
FE_SN <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion, 
            data = Total)
summary(FE_SN)
# --> We use the "0" to remove the intercept and instead get estimates of the 
# means of each class in the output.

# Compare to multilevel model (slopes)
summary(Lvl1)
# --> The slopes for Gender and Extraversion are similar to the ones we obtained
# with multilevel analysis.

# Compare to multilevel model (intercepts)
InterceptsML <- coef(Lvl1)$Class[,1]
InterceptsFE <- as.numeric(FE_SN$coefficients[1:12])

InterceptsFE - InterceptsML
cor(InterceptsML, InterceptsFE)

# --> The intercept values from the FE model are larger because it doesn't 
# partially pool, but the rank ordering is very similar.

#### Assignment 2 --------------------------------------------------------------
# Add the level-2 predictor teacherExp to the fixed effect model, making it
# comparable to the FixedEffects model from step 3 above. What do you notice in 
# the fixed effects model and why is that?

# Fixed effects model with level-2 predictor.
FE_SN2 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion + teacherExp,
             data = Total)
summary(FE_SN2)

# --> There is no estimate for teacherExp. The dummies capture all level-2 
# differences, including the ones caused by differences in teacherExp, 
# so the predictor is perfectly colinear with the class dummies. 
# In other words, all differences between classes due to differences in 
# teacherExp are already accounted for by the differences between the estimated 
# means of each class. Adding the teacherExp to the model therefore is like 
# adding the same predictor twice.

#### Assignment 3 --------------------------------------------------------------
# Check for "random slopes" for Extraversion and Gender using the fixed
# effects model. 

# - What do you conclude?

# - If there is a "random slope", add a cross-level interaction with teacherExp
# to the fixed effects model. What do you notice?                                                                         

# Fixed effects model with random slopes
FE_SN3 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion  
             + Gender:factor(Class) + Extraversion:factor(Class), data = Total)   
anova(FE_SN3)

# --> We conclude there the effect of Gender does not vary across classes but 
# the effect of Extraversion does. We can try to explain this difference using
# teacherExp.

# Fixed effects model with cross-level interaction
FE_SN4 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion  
             + Extraversion:factor(Class):teacherExp, data = Total)   
anova(FE_SN4)

# --> Teacher Experience indeed influences the differences between classes in 
# the effect of Extraversion.

# Notice that we add an interaction term between Extraversion and teacherExp 
# without also adding the main effect of teacherExp (explicitly). This is weird 
# and something you normally should not do! But...the main effect of
# teacherExp is in the model! Remember that it is part of the difference
# between the means of each class. Be sure to add the interaction using 
# "Extraversion:factor(Class):teacherExp" instead of 
# "Extraversion*factor(Class)*teacherExp", to make sure we do not add the main 
# effect of teacherExp.



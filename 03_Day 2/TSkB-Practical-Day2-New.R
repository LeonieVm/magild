#==============================================================================#
# Day 2: Practical Parts 3, 4, and 5
#==============================================================================#

# For the three practical parts, load Workspace-TSkB-Practical-Day2-Def.RData.

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

#################### Part 3: Complete Multilevel Analysis ######################

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
#       gender (dichotomous; 0=male, 1 =female)
#- class-level independent variables: 
#       teacher experience (in years)
#- outcome variable: 
#       popular (continuous: higher scores indicate higher popularity)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
### Exercise 3a Centering------------------------------------------------------- 
#------------------------------------------------------------------------------#

#### Assignment 1 --------------------------------------------------------------
# Run a Multilevel Analysis with both the level 1 and level 2 predictors, and a 
# random effects for the level 1 predictors. 

Regr6_ML <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                   (1 + Gender + Extraversion | Class), data = Total)
summary(Regr6_ML)

# Check the normality of the errors for this model.
# Tip: Look up the residual and ranef commands

# For level 1
hist(residuals(Regr6_ML))
qqnorm(residuals(Regr6_ML))

# For level 2
hist(ranef(Regr6_ML)$Class[,1])
hist(ranef(Regr6_ML)$Class[,2])
hist(ranef(Regr6_ML)$Class[,3])

qqnorm(ranef(Regr6_ML)$Class[,1])
qqnorm(ranef(Regr6_ML)$Class[,2])
qqnorm(ranef(Regr6_ML)$Class[,3])

# Check the assumptions of linearity for all predictors. Tip: Remember what the 
# level 2 DV is, and see the assignment from day 1 on how to calculate it. We 
# already did this with GGpairs, but if you want to check for specific 
# variables, this how you do it.

# For level 1
plot(Total$Extraversion, Total$Popular)
plot(Total$Gender, Total$Popular)
plot(Total$teacherExp, Total$Popular)

# For level 2
Mean <- matrix(NA, ncol = 2, nrow = 12) # First we create a matrix in which we 
# store the intercepts and coefficients
colnames(Mean) <- c("Popular", "TeacherExperience") # And then we specify the 
# column names. Then, we automatically run lm on all the 12 classes and store 
# the results.
for (i in 1:12) {
  Mean[i, 1] <- mean(eval(parse(text = paste("Class", i, sep = "")))$Popular)
  Mean[i, 2] <- mean(eval(parse(text = paste("Class", i, sep = "")))$teacherExp)
}

plot(Mean[,2],Mean[,1])

#### Assignment 2 --------------------------------------------------------------

# Grand center the continuous predictors and rerun the model. What do you 
# notice when comparing the results to the model with the uncentered predictors?

# Grand center the continuous level 1 predictor
Total$Extraversion_c <- Total$Extraversion - mean(Total$Extraversion)

# Grand Center the level 2 predictor
# We're going to do this in two steps, because we can't use the raw teacherExp scores.
# The reason for that is that each student has a teacherExp score in the datafile, while
# teacherExp obviously is a class characteristic. So we'll first determine the
# average teacherExp for each class (which is just the teacherExp score for that class),
# and then calculate the average of those class-scores for centering.
Teacher_Exp <- matrix(NA, ncol = 1, nrow = 12)

for (i in 1:12) {
  Teacher_Exp[i,] <- mean(Total$teacherExp[Total$Class==i])
}

Teacher_Exp
mean(Teacher_Exp)

Total$TE_c <- Total$teacherExp - mean(Teacher_Exp)

Regr6_ML2 <- lmer(Popular ~ 1 + Gender + Extraversion_c + TE_c + 
                    (1 + Gender + Extraversion_c | Class), data = Total)
summary(Regr6_ML2)

# Compare to the results of the model with the uncentered predictors
summary(Regr6_ML)

# You can also ask for only part of the summary output, which will make
# the comparison easier:
VarCorr(Regr6_ML) #this gives you the random effects
VarCorr(Regr6_ML2)

coef(summary(Regr6_ML)) #this gives you the fixed effects
coef(summary(Regr6_ML2))

#------------------------------------------------------------------------------#
### Exercise 3b A Complete Multilevel Analysis----------------------------------
#------------------------------------------------------------------------------#

#### Assignment 1 --------------------------------------------------------------
# Go through all the steps of a multilevel analysis. Calculate the ICC and the 
# amount of explained variance in each step.

# Start with the intercept only model in which you use Popular as the dependent 
# variable, and calculate the ICC. Also determine if multilevel analysis is 
# necessary. If needed ask for instruction using ?lme

IO <- lm(Popular ~ 1, Total)
IO_ML <- lmer(Popular ~ 1 + (1 | Class), Total)
rand(IO_ML)# Or, instead of rand(), you can use:
anova(IO_ML,IO)

summary(IO_ML)
# The ICC is equal to 0.223/(0.223 + 1.016) = 0.18

# Now, add the first level variables extraversion and gender to the model as 
# fixed effects. What is the explained variance on level 1 and level 2, what 
# "weird result" do you notice? 
# What could cause this?
# What are your conclusions about these predictors?

Lvl1 <- lmer(Popular ~ 1 + Gender + Extraversion + (1 | Class), Total)

summary(Lvl1)# Both predictors are significantly related to Popularity

# Now, we calculate the explained variances on Level 1 and 2:
VarianceIO <- as.data.frame(VarCorr(IO_ML))
VarianceLv1 <- as.data.frame(VarCorr(Lvl1))

# Explained Variance on Level 1
(VarianceIO[2,4] - VarianceLv1[2,4])/ VarianceIO[2,4]
# .5159

# Explained Variance on Level 2
(VarianceIO[1,4] - VarianceLv1[1,4])/ VarianceIO[1,4]
# -.2431, explained variance can't be negative! This is likely because there is 
# less variance between classes in either extraversion, gender, or both, than 
# would be expected based on random sampling.

# Now, add the second level variable teacher experience to the model.
# What is the explained variance on level 1 and level 2, and what are your 
# conclusions about this predictor?

FixedEffects <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                       (1 | Class), Total)
summary(FixedEffects)# Teacher experience is significantly related to popularity.

VarianceFE <- as.data.frame(VarCorr(FixedEffects))

# Explained Variance on Level 1 (compared to model with just level 1 predictors)
(VarianceLv1[2,4] - VarianceFE[2,4])/ VarianceLv1[2,4]
# 0. No explained variance since you only added a level 2 predictor

# Explained Variance on Level 2 (compared to model with just level 1 predictors)                                   
(VarianceLv1[1,4] - VarianceFE[1,4])/ VarianceLv1[1,4]
# .2920

# Now, check if the relation between the first level predictors and popularity 
# is the same across classes. What type of effect do you need to add for testing 
# this hypothesis? And is this effect significant?

# You need to add random slopes.
RandomEffectsGender <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                              (1 + Gender | Class), Total)
anova(RandomEffectsGender, FixedEffects)

RandomEffectsExtraversion <- lmer(Popular ~ 1 + Gender + Extraversion + 
                                    teacherExp + 
                                    (1 + Extraversion | Class), Total)
anova(RandomEffectsExtraversion, FixedEffects)# Or, use
rand(RandomEffectsExtraversion)

# Neither level 1 predictor has a random slope.

# Finally, decide if you need to add a cross-level interaction?
# No! There is no random slope, so you don't need to add a cross-level 
# interaction.

# # What happens if you do add a cross-level interaction when there is no
# # random slope?


#### Assignment 2 --------------------------------------------------------------
# We choose the FixedEffects model as the final model since there are no random       
# slopes. Check if the assumptions for the model are met .        

# For level 1
hist(residuals(FixedEffects))
qqnorm(residuals(FixedEffects))

# For level 2
hist(ranef(FixedEffects)$Class[,1])
qqnorm(ranef(FixedEffects)$Class[,1])

#### Assignment 3 --------------------------------------------------------------
# In Assignment 1, we built the model step by step. As mentioned in 
# the lecture, this is one way of running an analysis. Alternatively, you can 
# run the full model immediately. This is called the maximum approach.
# Run the full model in one go. Does your conclusion differ from the conclusion
# you drew based on Assignment 1?

MaximumModel <- lmer(Popular ~ 1 + Gender + Extraversion +                      
                       teacherExp + 
                       (1 + Gender + Extraversion | Class), Total)
summary(MaximumModel)

# The maximum approach can be difficult for frequentist estimation because of
# the complexity of the model. This is why I prefer to use Bayesian statistics
# with it. It's more stable, and if there is a problem, it's easier to pinpoint

MaximumModel_Bayes <- brm(Popular ~ 1 + Gender + Extraversion + 
                            teacherExp + 
                            (1 + Gender + Extraversion | Class), 
                          control = list(adapt_delta = 0.9), Total)
summary(MaximumModel_Bayes)

                                                                                #add code posterior/conclusion

################# Part 4: Common Issues and Three-Level Models #################

#------------------------------------------------------------------------------#
### Exercise 4a Small level 2 N: Fixed Effects Models---------------------------
#------------------------------------------------------------------------------#

#### Assignment 1 --------------------------------------------------------------
# Let's first run an multilevel analysis on data with only 3 level 2 units.
# For this we'll just use the data of the first three classes from the 
# Total data.
Multilevel_SN <- lmer(Popular ~ 1 + Gender + Extraversion + 
                        (1 | Class), Total_SN)                                 

summary(Multilevel_SN)
# Why is the above analysis not recommended?
# The level 2 variance is calculated on only 3 observations.

# Now, let's analyse the data using a fixed effects model
FE_SN <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion, 
            data = Total_SN)
summary(FE_SN)

# What does the "0" do in the code above?  
# We use this approach to get estimates of the means of each class in our
# output.

#### Assignment 2 --------------------------------------------------------------
# Now, let's add a level 2 predictor for both the multilevel analysis and the     
# fixed effect model.

# Multilevel model
Multilevel_SN2 <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                         (1 | Class), data = Total_SN)
summary(Multilevel_SN2)

# Fixed effects model
FE_SN2 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion + teacherExp,
             data = Total_SN)
summary(FE_SN2)

# What do you notice in the fixed effects model?
# There is no estimate for teacherExp.

# Can you explain why?
# The dummies capture all level 2 differences, including the ones caused by
# differences in teacherExp, so the predictor is perfectly collinear with the
# class dummies. In other words, all differences between classes due to 
# differences in teacherExp are already accounted for by the differences
# between the estimated means of each class. Adding the teacherExp
# to the model therefore is like adding the same predictor twice.

#### Assignment 4 --------------------------------------------------------------
#Now, let's add cross-level interaction.

# Multilevel model
Multilevel_SN3 <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                         Extraversion:teacherExp + (1 + Extraversion | Class), 
                       Total_SN)
summary(Multilevel_SN3)

# Fixed effects model
FE_SN3 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion  
             + Extraversion:teacherExp, data=Total_SN)
summary(FE_SN3)

# Let's also run both options for the cross-level interaction on the larger 
# dataset with 12 classes.                                                                  

# Multilevel model
Multilevel_SN4 <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                         Extraversion:teacherExp + (1 + Extraversion | Class), 
                       Total)
summary(Multilevel_SN4)
# Fixed effects model
FE_SN4 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion                
             + Extraversion:teacherExp, data=Total)
summary(FE_SN4)

# Notice that we add an interaction term between Extraversion and teacherExp 
# without also adding the main effect of teacherExp (explicitly). This is weird 
# and something you normally should not do! But...the main effect of
# teacherExp is in the model! Remember that it is part of the difference
# between the means of each class.

# Compare the individual intercepts of the multilevel model to the dummy scores
# from the fixed effect model. What do you notice?
InterceptsML <- coef(Multilevel_SN4)$Class[,1]
InterceptsFE <- as.numeric(FE_SN4$coefficients[1:12])

cor(InterceptsML, InterceptsFE)
InterceptsFE - InterceptsML

# The values from the FE model are larger because it doesn't partially pool,
# but the rank ordering is pretty similar.

#------------------------------------------------------------------------------#
### Exercise 4b 3-level data----------------------------------------------------
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# The dataset "nurses" holds data from nurses working in wards nested within 
# hospitals. In each of 25 hospitals, four wards are selected and randomly 
# assigned to an experimental and control condition. In the experimental 
# condition, a training program is offered to all nurses to cope with job
# related stress. After the program is completed, a sample of about 10 nurses 
# from each ward is given a test that measures job-related stress. Additional 
# variables are: nurse experience (years), nurse gender (0 = male, 1 = female), 
# type of ward (0 = general care, 1 = special care), and hospital size 
# (0 = small, 1 = medium, 2 = large). This is an example of an experiment where 
# the experimental intervention is carried out at the group level. In 
# biomedical research this design is known as a cluster randomized trial. They 
# are quite common also in educational and organizational research, where 
# entire classes or schools are assigned to experimental and control
# conditions. Since the design variable Experimental versus Control group 
# (condition) is manipulated at the second (ward) level, we can study whether 
# the experimental effect is different in different hospitals, by defining the 
# regression coefficient for the condition variable as random at the hospital 
# level.
#------------------------------------------------------------------------------#

as_tibble(nurses)

#### Assignment 1 --------------------------------------------------------------
# Run an intercept-only model. Use:                                               
IO_3lv <- lmer(stress ~ 1  + (1 | hospital/wardid), nurses)
summary(IO_3lv)
# OR
IO_3lv2 <- lmer(stress ~ 1  + (1 | hospital:wardid) + (1 | hospital), nurses)
summary(IO_3lv2)

# Can you tell how the level 3 and level 2 units are identified?
# The variable before the "/" or the ":" is the level 3 units. When
# using the "/", lmer does everything for you, but when using the ":", you
# have to separately indicate the random intercept at level 2 
# ((1 | hospital:wardid)) and level 3 ((1 | hospital))

# Do we need to take the higher levels into account?
rand(IO_3lv) # Yes, there is significant variance on both levels.

# Calculate the intra-class correlation. How do you interpret these ICCs?
Variance_3lv <- as.data.frame(VarCorr(IO_3lv))

# ICC for Level 2
(Variance_3lv[1,4])/(Variance_3lv[1,4] + Variance_3lv[2,4] + Variance_3lv[3,4])
# 50.68% of the variance is on the ward level (level 2)

# ICC for Level 3
(Variance_3lv[2,4])/(Variance_3lv[1,4] + Variance_3lv[2,4] + Variance_3lv[3,4])
# 18.07% of the variance is on the hospital level (level 3)

# ICC for both higher levels simultaneously
(Variance_3lv[1,4] + Variance_3lv[2,4] )/(Variance_3lv[1,4] + Variance_3lv[2,4] 
                                          + Variance_3lv[3,4])
# 68.76% of the variance is on the ward and hospital levels
performance::icc(IO_3lv)

#### Assignment 2 --------------------------------------------------------------
# Test if there is an effect of condition on stress controlling for all other
# predictors in the dataset.
ML_3lv <- lmer(stress ~ 1 + gender + experien + condition + wardtype + 
                 hospsize + (1 | hospital:wardid) + (1 | hospital), nurses)
summary(ML_3lv)

# Are all predictors correctly assigned to the different levels? So, gender and
# experience as level 1 predictors, condition and wardtype as level 2 predictors
# and hospsize as level 3 predictor?
# Yes, you can tell by looking at the df of the effects.                        

# Test if there is an effect of condition on stress (controlling for all other
# predictors in the dataset) and if the effect is different for different 
# hospitals. Does the effect differ between hospitals?
ML_3lv2 <- lmer(stress ~ 1 + gender + experien + condition + wardtype + 
                  hospsize + (1 | hospital:wardid) + (1 + condition | hospital),
                nurses)
summary(ML_3lv2)
rand(ML_3lv2)# Yes it does, as the random-slope variance in the effect of 
# condition is significantly different from 0.

# Now, run the maximum-model (like you should ;)), what are your conclusions?    #is something missing here?

MaximumModel_3lv <- brm(stress ~ 1 + gender + experien + condition + wardtype + 
                          hospsize 
                        + (1 + gender + experien| hospital:wardid) 
                        + (1 + gender + experien + condition 
                        + wardtype| hospital),
                        control = list(adapt_delta = 0.9), nurses)
summary(MaximumModel_3lv)

# No differences between hospitals in the effects of gender or experience,
# but there are differences in average stress and the effects of condition and 
# wardtype

# No differences between wards in the effects of gender or experience,
# but there are differences in average stress.

# Women experience less stress than men, more experience leads to less
# stress, and the experimental condition leads to less stress.


########################### Part 5: Cross-Nested Data ##########################  

#------------------------------------------------------------------------------#
# In the dataset pupcross, we have data from 1000 pupils who have attended 100 
# different primary schools, and subsequently went on to 30 secondary schools. 
# We have a cross-classified structure with pupils are nested within primary 
# and within secondary schools, with primary and secondary schools crossed. 
# In other words: pupils are nested within the crossclassification
# of primary and secondary schools. Our response variable achievement which is 
# measured in secondary school. We have two explanatory variables at the pupil 
# level: pupil gender (0 = male, 1 = female) and a six-point scale for pupil 
# socioeconomic status, pupil ses. At the school level a dichotomous
# variable that indicates if the school is public (denom = 0) or 
# denominational (denom = 1). Since we have both primary and secondary schools, 
# we have this information stored in two variables, one for primary- and on for
# secondary schools,  named pdenom for the primary school and sdenom for the 
# secondary school.
#------------------------------------------------------------------------------#

as_tibble(pupcross)

#### Assignment 1 --------------------------------------------------------------
# Run an intercept-only model and check the amount of variance on the primary 
# and secondary school level.

IO_CR <- lmer(ACHIEV ~ 1  + (1 | PSCHOOL) + (1 | SSCHOOL), pupcross)
summary(IO_CR)

# Do we need to take the higher levels into account?
rand(IO_CR) # Yes, significant variance on both levels

Variance_CR <- as.data.frame(VarCorr(IO_CR))

# ICC for Primary Schools
(Variance_CR[1,4])/(Variance_CR[1,4] + Variance_CR[2,4] + Variance_CR[3,4])
# 22.86% of the variance is on the primary school "level"

# ICC for Secondary Schools
(Variance_CR[2,4])/(Variance_CR[1,4] + Variance_CR[2,4] + Variance_CR[3,4])
# 8.87% of the variance is on the secondary school "level"  

# Total variance on the higher levels
performance::icc(IO_CR) # 31.70%

# Can you tell how cross-classification is specified differently than nested
# levels?
# With multiple nested levels we use the "/" or the ":" to identify 
# the level 2 and 3 units. With cross-classification we use a separate term 
# between brackets for each cross-nested level

#### Assignment 2 --------------------------------------------------------------
# Run an Intercept-only model on the nurses data (described under Exercise 4b)
# with both the cross-classified and the proper 3-level notation. What do you 
# see? Why?

IO_3lv <- lmer(stress ~ 1  + (1 | hospital/wardid), nurses)
summary(IO_3lv)

IO_3lv_Wrong <- lmer(stress ~ 1  + (1 | hospital) + (1 | wardid), nurses)
summary(IO_3lv_Wrong)

# There is no difference!! How can that be?
table(nurses$hospital, nurses$wardid)

# Each ward ID only occurs in combination with 1 hospital ID! So ward 11 only
# exists in hospital 1, while ward 21 only occurs in hospital 2. Because
# of this R can't make a mistake, BUT if the same  level 2 ID's occur in
# multiple level 3 ID's, you need to use the correct specification!!

#### Assignment 3 --------------------------------------------------------------
# Run the maximum-model on the cross-nested data, what are your conclusions?

MaximumModel_CR <- brm(ACHIEV ~ 1  + PUPSEX + PUPSES + PDENOM + SDENOM +
                         (1 + PUPSEX + PUPSES | PSCHOOL) + 
                         (1 + PUPSEX + PUPSES | SSCHOOL), 
                       control = list(adapt_delta = 0.9), iter = 5000, pupcross)
summary(MaximumModel_CR)

# Variation in achievement across both primary and secondary schools, the 
# effect of gender on achievement  appears to be different across primary
# but not secondary schools, while the effect of SES on achievement doesn't
# appear to differ across primary or secondary schools.

# Girls have higher achievement than boys and higher SES is associated with
# higher achievement. There is no "significant"  effect of denomination for 
# either primary or secondary schools.





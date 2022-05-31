#==============================================================================#
# Day 2: Practical 1.1
#==============================================================================#

#------------------------------------------------------------------------------#
## Exercise 2a Steps of a multilevel analysis
#------------------------------------------------------------------------------#

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

#------------------------------------------------------------------------------#
## Exercise 1a Cross-level Interactions
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


# Run a Multilevel Analysis with both the level 1 and level 2 predictor, and a 
# random effects for the level 1 predictors. NOTE!! You might want to add the 
# random effects one at a time, so first Extraversion, than Gender.

Regr4_ML1 <- lmer(Popular ~ 1 + Extraversion + Gender + teacherExp + 
                    (1 + Extraversion |Class), data=Total)
summary(Regr4_ML1)
rand(Regr4_ML1)


Regr4_ML2 <- lmer(Popular ~ 1 + Extraversion + Gender + teacherExp + 
                    (1 + Gender |Class), data=Total)
summary(Regr4_ML2)
rand(Regr4_ML2)


Regr4_ML <- lmer(Popular ~ 1 + Extraversion + Gender + teacherExp + 
                   (1 + Extraversion + Gender |Class), data=Total)
summary(Regr4_ML)

# Check Random Effects
rand(Regr4_ML)


# Should you add a cross-level interaction?
# No, there are no  random effects? So stop! Don't add cross-level interactions!!


# What happens if you do add a cross-level interaction when there is no
# random slope?


# Below we just add the cross-level interactions to illustrate the code!!

# Run a multilevel model with cross-level interaction
Regr5_ML <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                   Gender:teacherExp + Extraversion:teacherExp +
                   (1 + Gender + Extraversion | Class), data=Total)
summary(Regr5_ML)

# Check the normality of the errors in your final model. Tip: Lookup the 
# residual and ranef commands
Regr6_ML <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                   (1 + Gender + Extraversion | Class), data=Total)
summary(Regr6_ML)

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
# store the inetercepts and coefficients
colnames(Mean) <- c("Popular", "TeacherExperience") #  And then we specify the 
# columnnames. Then we automatically run lm on all the 25 classes and store the 
# results
for (i in 1:12) {
  Mean[i, 1] <- mean(eval(parse(text = paste("Class", i, sep = "")))$Popular)
  Mean[i, 2] <- mean(eval(parse(text = paste("Class", i, sep = "")))$teacherExp)
}


plot(Mean[,2],Mean[,1])


# Grand Center the continuous predictors and rerun final model, what do you 
# notice?


# Grand Center the continuous level 1 predictor
Total$Extraversion_c <- Total$Extraversion - mean(Total$Extraversion)

# Grand Center the level 2 predictor

Teacher_Exp <- matrix(NA, ncol = 1, nrow = 12)

for (i in 1:12) {
  
  Teacher_Exp[i,] <- mean(Total$teacherExp[Total$Class==i])
}

Teacher_Exp
mean(Teacher_Exp)

Total$TE_c <- Total$teacherExp - mean(Teacher_Exp)

Regr4_ML2 <- lmer(Popular ~ 1 + Extraversion_c + Gender + TE_c + 
                    (1+ Extraversion + Gender|Class), data=Total)
summary(Regr4_ML2)


#------------------------------------------------------------------------------#
## Exercise 1b A Complete Multilevel Analysis
#------------------------------------------------------------------------------#

# For this Assignment use the R-workspace AssignmentWorkspace.rdata

# These assignments are about a study on the popularity of highschool students.
# A total of 515 students from 25 different classes took part in this study in 
# which we determined how extraversion, gender, and teacher experience 
# influenced a student's popularity. The data of this study show a clear 
# hierarchy - or nesting - , with students nested in classes. Since observations
# of students from the same class are most likely not independent, we can't 
# just analyse these data using normal lineair regression. In addition, we are 
# dealing with variables on different levels. We have pupil level variables 
# like extraversion and gender, but we also have class characteristics like 
# teacher experience. Only with multilevel analysis can we estimate both types 
# of variables in a single analysis. In this assignment we'll start with some 
# regular regression analyseson the data of each separate class (even though 
# the independence assumption is violated!). Because this will help you get the
# hang of R, but also because it will make the multilevel analyses more 
# insightful. Then, given the hierarchical nature of the data, we run a 
# multilevel regression analyis on the complete data.

# Assignment 2:
# Go through all the steps of a multilevel analysis. Calculate the ICC and the 
# amount of explained variance in each step.

# Start with the intercept only model in which you use Popular as the dependent 
# variable, and calculate the ICC. Also determine if multilevel analysis is 
# necessary. If needed ask for instruction using ?lme

IO <- lm(Popular ~ 1, Total)
IO_ML <- lmer(Popular ~ 1 + (1 | Class), Total)
summary(IO_ML)
rand(IO_ML)

# Or instead of rand() you can use:
anova(IO_ML,IO)

summary(IO_ML)
# The ICC is equal to 0.4243/(1.1337+0.4243) = .272


# Now add the first level variables extraversion and gender to the model as 
# fixed effects. What is the explained variance on level 1 and level 2, what 
# "weird result" do you notice? 
# What could cause this?
# What are your conclusions about these predictors.

Lvl1 <- lmer(Popular ~ 1 + Gender + Extraversion + (1 | Class), Total)

summary(Lvl1)
# Both predictors are significantly related to Popularity

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

# Now add the second level variable teacher experience to the model.
# What is the explained variance on level 1 and level 2, and what are your 
# conclusions about this predictor

FixedEffects <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                       (1 | Class), Total)
summary(FixedEffects)
# Teacher experience is significantly related to popularity.
VarianceFE <- as.data.frame(VarCorr(FixedEffects))

# Explained Variance on Level 1
(VarianceLv1[2,4] - VarianceFE[2,4])/ VarianceLv1[2,4]
# 0. No explained variance since you only added a level 2 predictor

# Explained Variance on Level 2
(VarianceLv1[1,4] - VarianceFE[1,4])/ VarianceLv1[1,4]
# .2920

# Now check if the relation between the first level predictors and popularity 
# is the same across classes. What type of effect do you need to add for test 
# this hypotheses? And is this effect significant?

# You need to add random slopes
RandomEffectsGender <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                              (1 + Gender | Class), Total)
anova(RandomEffectsGender,FixedEffects)

RandomEffectsExtraversion <- lmer(Popular ~ 1 + Gender + Extraversion + 
                                    teacherExp + 
                                    (1 + Extraversion | Class), Total)
anova(RandomEffectsExtraversion,FixedEffects)
# Or use
rand(RandomEffectsExtraversion)

# Neither level 1 predictor has a random slope

# Finally decide if you need to add a cross-level interaction?

# No! There is no random slope so you don't need to add a cross-level 
# interaction

# Assignment 10:
# Write down the equation for your final model.


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

# Maximum approach: Run the full model in one go and check for explained
# variances etc. Does your conclusion differ from the one you drew based on 
# the Assignments above?


MaximumModel <- lmer(Popular ~ 1 + Gender + Extraversion + 
                                    teacherExp + 
                                    (1 + Gender + Extraversion | Class), Total)

summary(MaximumModel)


MaximumModel_Bayes <- brm(Popular ~ 1 + Gender + Extraversion + 
                       teacherExp + 
                       (1 + Gender + Extraversion | Class), 
                       control = list(adapt_delta = 0.9), Total)

summary(MaximumModel_Bayes)

performance::icc(MaximumModel_Bayes)
performance::r2(MaximumModel_Bayes)






#------------------------------------------------------------------------------#
## Exercise 1c Small level 2 N: Fixed Effects Models
#------------------------------------------------------------------------------#

# Let's first run an multilevel analysis on data with on 3 level 2 units
Multilevel_SN <- lmer(Popular ~ 1 + Gender + Extraversion + 
                       (1 | Class), Total_SN)

summary(Multilevel_SN)

# Why is the above analysis not recommended?
# The level 2 variance is calculated on only 3 observations

# Now let's analyse the data using a fixed effects model
FE_SN <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion, data=Total_SN)
summary(FE_SN)

# What does the "0" do in the code above? 

### Now lets add a level 2 predictor

Multilevel_SN2 <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                        (1 | Class), Total_SN)
summary(Multilevel_SN2)


FE_SN2 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion + teacherExp 
             , data=Total_SN)
summary(FE_SN2)

# What do you notice?
# There is no estimate for teacherExp.

# Can you explain why?
# The dummies capture all level 2 differences, including the ones caused by
# differences in teacherExp, so the predictor is perfectly colinear with the
# class dummies.

### Now lets add cross-level interaction

Multilevel_SN3 <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                         Extraversion:teacherExp + (1 + Extraversion| Class), 
                       Total_SN)
summary(Multilevel_SN3)


FE_SN3 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion  
             + Extraversion:teacherExp, data=Total_SN)
summary(FE_SN3)


### Let's also run both options on the larger dataset
Multilevel_SN4 <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                         Extraversion:teacherExp + (1 + Extraversion| Class), 
                       Total)
summary(Multilevel_SN4)


FE_SN4 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion  
             + Extraversion:teacherExp, data=Total)
summary(FE_SN4)

# Compare the individual intercepts of the multilevel model to the dummy scores
# from the fixed effect model. What do you notice
InterceptsML <- coef(Multilevel_SN4)$Class[,1]
InterceptsFE <- as.numeric(FE_SN4$coefficients[1:12])

cor(InterceptsML, InterceptsFE)
InterceptsFE - InterceptsML

# The values from the FE model are larger because it doesn't partially pool
# but the rank ordering is pretty similar.

#------------------------------------------------------------------------------#
## Exercise 1d 3-level data
#------------------------------------------------------------------------------#

as_tibble(nurses)

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

# Run an intercept-only model and calculate the intra-class correlation.
# How would you interpret these ICCs?

IO_3lv <- lmer(stress ~ 1  + (1 | hospital/wardid), nurses)

summary(IO_3lv)

# OR

IO_3lv2 <- lmer(stress ~ 1  + (1 | hospital:wardid) + (1 | hospital), nurses)
summary(IO_3lv2)

# Can you tell how the level 3 and level 2 units are identified?
# The variable before the "/" or the ":" is the level 3 units. When
# using the "/" lmer does eveything for you, but when using the ":" you
# have to separately indicate the random intercept at level 2 
# ((1 | hospital:wardid)) and level 3 ((1 | hospital))

# Do we need to take the higher levels into account?
rand(IO_3lv) # Yes, significant variance on both levels

Variance_3lv <- as.data.frame(VarCorr(IO_3lv))

# ICC for Level 2
(Variance_3lv[1,4])/(Variance_3lv[1,4] + Variance_3lv[2,4] + Variance_3lv[3,4])
# 50.68% of the variance is on the ward level (level 2)

# ICC for Level 3
(Variance_3lv[2,4])/(Variance_3lv[1,4] + Variance_3lv[2,4] + Variance_3lv[3,4])
# 18.07% of the variance is on the hospital level (level 3)

# ICC for both higher levels simulataneously
(Variance_3lv[1,4] + Variance_3lv[2,4] )/(Variance_3lv[1,4] + Variance_3lv[2,4] 
                                          + Variance_3lv[3,4])
# 68.76% of the variance is on the ward and hospital levels
performance::icc(IO_3lv)


# Test if there is an effect of condition on stress controlling for all other
# predictors in the dataset.
ML_3lv <- lmer(stress ~ 1 + gender + experien + condition + wardtype + 
                 hospsize + (1 | hospital:wardid) + (1 | hospital)
               , nurses)
summary(ML_3lv)

# Are all predictors correctly assigned to the different levels? So, gender and
# experience as level 1 predictors, condition and wardtype as level 2 predictors
# and hospsize as level 3 predictor?

# Yes, you can tell by looking at the df of the effects.


# Test if there is an effect of condition on stress (controlling for all other
# predictors in the dataset) and if the effect is different for different 
# hospitals. does the effect differ between hospitals?
ML_3lv2 <- lmer(stress ~ 1 + gender + experien + condition + wardtype + 
                 hospsize + (1 | hospital:wardid) + (1 + condition| hospital)
               , nurses)
summary(ML_3lv2)
rand(ML_3lv2)# Yes it does, as the random-slope variance in the effect of 
# condition is significantly different from 0.


# Now run the maximum-model (like you should ;)), what are your conclusions?

MaximumModel_3lv <- brm(stress ~ 1 + gender + experien + condition + wardtype + 
                 hospsize 
                + (1 + gender + experien| hospital:wardid) 
                + (1 + gender + experien + condition + wardtype| hospital)
                , control = list(adapt_delta = 0.9), nurses)
summary(MaximumModel_3lv)

# No differences between hospitals in the effects of gender or experience,
# but there are differences in average stress and the effects of condition and 
# wardtype

# No differences between wards in the effects of gender or experience,
# but there are differences in average stress.

# Woman experience less stress than males, more experience leads to less
# stress, and the experimental condition leads to less stress.

#------------------------------------------------------------------------------#
## Exercise 1e Cross-Classified Data
#------------------------------------------------------------------------------#

as_tibble(pupcross)


# In the dataset pupcross we have data from 1000 pupils who have attended 100 
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

# Run an intercept-only model and check the amount of variance on the primary 
# and secondary school level.

IO_CR <- lmer(ACHIEV ~ 1  + (1 | PSCHOOL) + (1 | SSCHOOL), pupcross)
summary(IO_CR)

# Do we need to take the higher levels into account?
rand(IO_CR) # Yes, significant variance on both levels

Variance_CR <- as.data.frame(VarCorr(IO_CR))

# ICC for  Primary Schools
(Variance_CR[1,4])/(Variance_CR[1,4] + Variance_CR[2,4] + Variance_CR[3,4])
# 22.86% of the variance is on the primary school "level"

# ICC for Secondary Schools
(Variance_CR[2,4])/(Variance_CR[1,4] + Variance_CR[2,4] + Variance_CR[3,4])
# 8.87% of the variance is on the secondary school "level"  

# Total variance on the higher levels
performance::icc(IO_CR) # 31.70%

# Can you tell how cross-clasification is specified differently then nested
# levels?
# With multiple nested levels we use the "/" or the ":" to identify 
# the level 2 and 3 units. With cross-classification we use a separate term 
# between brackets for each cross-nested level

# Extra: Run an Intercept-only model on the nurses data with both the 
# cross-clasified and the proper 3-level notation. What do you see? Why?

IO_3lv <- lmer(stress ~ 1  + (1 | hospital/wardid), nurses)
summary(IO_3lv)

IO_3lv_Wrong <- lmer(stress ~ 1  + (1 | hospital) + (1 | wardid), nurses)
summary(IO_3lv_Wrong)

# There is no difference!! How can that be?

table(nurses$hospital, nurses$wardid)

# Each ward ID only occurs in combination with 1 hospital ID! So ward 11 only
# exists in hospital 1, while ward 21 only occurs in hospital 2. Because
# of this R can't make a mistake, BUT if the same  level 2 ID's occur in
# multiple level 3 ID's you need to use the correct specification!!


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

# Girls have higher achievement than boys and higher SES is associated with higher
# achievement. There is no "significant"  effect of denomination for either
# primary or secondary schools.





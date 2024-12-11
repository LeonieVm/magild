# 5 steps multilevel: Example for the lecture slides

# Packages for the analyses
library(lme4)# important because otherwise you don't get p-values!!
library(ICC)
library(lmerTest)
library(brms)

# Data
load("LectureExampleDay3.RData")

# Explanation of the Variables:

# The dataset "gpa"  contains longitudinal data set on 200 college students. 
# The students' grade point average (GPA) has been recorded for six successive 
# semesters. At the same time, it was recorded whether the student held a job 
# in that semester, and for how many hours. This is recorded in a variable 
# 'job' (= hours worked). Finally, we also have the student-level variables 
# high school GPA and biological sex (0 = male, 1 = female).


# 1-----------------------------------------------------------------------------
# Check whether multilevel is necessary (using intercept-only models)
IO <- lm(gpa ~ 1, gpa)
IO_ML <- lmer(gpa ~ 1 + (1 | student), gpa)
anova(IO_ML, IO)
summary(IO_ML)

# "Variance student" / ("Variance residual" + "Variance student")
0.05714 /(0.05714 + 0.09759) # ICC = 0.3692884
# --> variance explained by between-person differences

# 2-----------------------------------------------------------------------------
# Step 2: Add first-level variables
## TIME
ML_level1_a <- lmer(gpa ~ 1 + time + (1 | student), gpa)
summary(ML_level1_a)
# --> time is a significant predictor (do not forget to load library(lmerTest))

# Now, we calculate the explained variances on Level 1 and 2 by subtracting the
# residual variance of the large model from the residual variance of the smaller
# model (see "vcov" in the output) and dividing it by the variance of the
# smaller (i.e., initial) model
VarianceIO <- as.data.frame(VarCorr(IO_ML))
VarianceLv1 <- as.data.frame(VarCorr(ML_level1_a))

VarianceIO
VarianceLv1

# Explained Variance on Level 1
(VarianceIO[2, 4] - VarianceLv1[2, 4]) / VarianceIO[2, 4]
# 0.4047695

# Explained Variance on Level 2
(VarianceIO[1, 4] - VarianceLv1[1, 4]) / VarianceIO[1, 4]
# -0.1152277; explained variance can't be negative! This value is negative
# because we have fixed measurement occasions. There is less variance between 
# students in their time scores than would be expected based on random sampling. 
# This would not be a visible with random measurement occasions.
# What should you do now?                                                       
# When adding more level-1 predictors, use the model WITH time as your reference 
# model, and NOT the intercept-only model! The same holds for calculating the
# ICC.

# Update the ICC ("Variance student" / ("Variance residual" + "Variance student"))
summary(ML_level1_a)
0.06372/(0.06372 + 0.05809) # ICC = 0.5231
# --> variance explained by between-person differences

##JOB

ML_level1_b <- lmer(gpa ~ 1 + time + job + (1 | student), gpa)
summary(ML_level1_b)
# --> job is a significant predictor after controlling for the time effect
#(do not forget to load library(lmerTest))

# Now, we re-calculate the explained variances on Level 1 and 2 
VarianceTime <- as.data.frame(VarCorr(ML_level1_a))
VarianceLv1 <- as.data.frame(VarCorr(ML_level1_b))

VarianceTime
VarianceLv1

# Explained Variance on Level 1
(VarianceTime[2, 4] - VarianceLv1[2, 4]) / VarianceTime[2, 4]
# 0.04907393

# Explained Variance on Level 2
(VarianceTime[1, 4] - VarianceLv1[1, 4]) / VarianceTime[1, 4]
# 0.1722895

# 3-----------------------------------------------------------------------------
# Step 3: Add second-level variables
ML_level1_2 <- lmer(gpa ~ 1 + time + job + (1 | student) + highgpa + sex, gpa)  
summary(ML_level1_2)# both person-level variables have significant partial effects

VarianceLV12 <- as.data.frame(VarCorr(ML_level1_2))

VarianceLv1
VarianceLV12

# Explained Variance on Level 1 (compared to model with just level-1 predictors)
(VarianceLv1[2, 4] - VarianceLV12[2, 4]) / VarianceLv1[2, 4]
# 0. No explained variance since you only added a level 2 predictor

# Explained Variance on Level 2 (compared to model with just level 1 predictors)
(VarianceLv1[1, 4] - VarianceLV12[1, 4]) / VarianceLv1[1, 4]
# 0.1311962

# 4-----------------------------------------------------------------------------
# Step 4: Add random slopes

# # you do this per lower level variable;
ML_level1_2_RE_a <- lmer(gpa ~ 1 + time + job + (1 + time | student) + highgpa + sex, gpa)
anova(ML_level1_2_RE_a, ML_level1_2)

ML_level1_2_RE_b <- lmer(gpa ~ 1 + time + job + (1 +   job| student) + highgpa + sex, gpa)
anova(ML_level1_2_RE_b, ML_level1_2)

# you do this per lower level variable;
# ML_level1_2_RE_a <- brm(gpa ~ 1 + time + job + (1 + time | student) + highgpa + sex, gpa)
# anova(ML_level1_2_RE_a, ML_level1_2)
# 
# ML_level1_2_RE_b <- brm(gpa ~ 1 + time + job + (1 +   job| student) + highgpa + sex, gpa)
# anova(ML_level1_2_RE_b, ML_level1_2)                                                      


# --> adding both random slopes improve fit significantly

# combine
ML_level1_2_RE <- lmer(gpa ~ 1 + time + job + (1 +  time + job| student) + highgpa + sex, gpa)
summary(ML_level1_2_RE)
VarCorr(ML_level1_2_RE) #this gives you the random effect SD
coef(summary(ML_level1_2_RE)) #this gives you the fixed effects

# 5-----------------------------------------------------------------------------
# Step 5: Add cross-level interactions (let's only take sex)
ML_level1_2_RE_CL <- lmer(gpa ~ 1 + time * sex + job * sex + 
                            (1 +  time + job| student) + highgpa + sex, gpa)

summary(ML_level1_2_RE_CL)
# only time:sex interaction is significant. Effect of time on gpa is larger for 
# females --> girls improve more over time

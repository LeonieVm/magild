# 5 steps multilevel: Example for the lecture slides

# Packages for the analyses
library(lme4)
library(ICC)
library(lmerTest)# important because otherwise you don't get p-values!!
library(brms)

# Data
# https://rdrr.io/cran/mlmhelpr/man/hsb.html
library(mlmhelpr)
data(hsb)

# Explanation of the Variables:

# "Our data file is a subsample from the 1982 High School and Beyond Survey and
# is used extensively in Hierarchical Linear Models by Raudenbush and Bryk.
# The data file, called hsb, consists of 7185 students nested in 160 schools.
# The outcome variable of interest is the student-level (level 1) math
# achievement score (mathach). The variable ses is the socio-economic status
# of a student and therefore is at the student level. The variable sector is an 
# indicator variable indicating if a school is public or catholic and is 
# therefore a school-level variable. There are 90 public schools (sector=0) and 
# 70 catholic schools (sector=1) in the sample."

# Note: Description copied from
# https://stats.oarc.ucla.edu/other/hlm/hlm-mlm/introduction-to-multilevel-modeling-using-hlm/

# 1-----------------------------------------------------------------------------
# Check whether multilevel is necessary (using intercept-only models)
IO <- lm(mathach ~ 1, hsb)
IO_ML <- lmer(mathach ~ 1 + (1 | id), hsb)
anova(IO_ML, IO)
summary(IO_ML)

# "Variance id" / ("Variance residual" + "Variance id")
8.614 / (8.614 + 39.148) # ICC = 0.1803526
# --> variance explained by school membership

# 2-----------------------------------------------------------------------------
# Step 2: Add first-level variables
ML_level1 <- lmer(mathach ~ 1 + ses  + (1 | id), hsb)
summary(ML_level1)
# --> ses is a significant predictor (do not forget to load library(lmerTest))

# Now, we calculate the explained variances on Level 1 and 2 by subtracting the
# residual variance of the large model from the residual variance of the smaller
# model (see "vcov" in the output) and dividing it by the variance of the
# smaller (i.e., initial) model
VarianceIO <- as.data.frame(VarCorr(IO_ML))
VarianceLv1 <- as.data.frame(VarCorr(ML_level1))

VarianceIO
VarianceLv1

# Explained Variance on Level 1
(VarianceIO[2, 4] - VarianceLv1[2, 4]) / VarianceIO[2, 4]
# 0.0539978

# Explained Variance on Level 2
(VarianceIO[1, 4] - VarianceLv1[1, 4]) / VarianceIO[1, 4]
# 0.4464638

# 3-----------------------------------------------------------------------------
# Step 3: Add second-level variables

ML_level1_2 <- lmer(mathach ~ 1 + ses  + (1 | id) + catholic, hsb)
summary(ML_level1_2)# the sector (catholic) is significantly related to mathach

VarianceLV12 <- as.data.frame(VarCorr(ML_level1_2))

VarianceLv1
VarianceLV12

# Explained Variance on Level 1 (compared to model with just level-1 predictors)
(VarianceLv1[2, 4] - VarianceLV12[2, 4]) / VarianceLv1[2, 4]
# 0. No explained variance since you only added a level 2 predictor

# Explained Variance on Level 2 (compared to model with just level 1 predictors)
(VarianceLv1[1, 4] - VarianceLV12[1, 4]) / VarianceLv1[1, 4]
# 0.2271592

# 4-----------------------------------------------------------------------------
# Step 4: Add random slopes

ML_level1_2_RE <- lmer(mathach ~ 1 + ses  + (1 + ses | id) + catholic, hsb)
anova(ML_level1_2_RE, ML_level1_2)

# you would do this per lower level variable;
# --> here we have only one and adding a random slope for it significantly
#     improves model fit.

summary(ML_level1_2_RE)

# Key Components:
# Groups:
#   This refers to the grouping variable at Level 2. In this case, id
#   represents the grouping by school.
# Name:
#   These are the random effects components. Here you might see (Intercept) for
#   random intercepts and other predictors for random slopes. In this example,
#   ses has a random slope.
# Variance:
#   This column shows the variance of the random effects.
#   - For the random slope: The variance of ses (0.4343 in the example)
#     represents the variability in the slope of ses across schools (id).
#   - For the random intercept: The variance of (Intercept) (3.9646 in the
#     example) represents the variability in the intercept across schools.
# Std.Dev.:
#   The standard deviation is the square root of the variance. It provides a
#   more interpretable measure of variability.
#   - For the random slope: The standard deviation for ses (0.659 in the
#     example) again shows variability of the slope of ses across schools.
# Corr:
#   This is the correlation between the random intercepts and random slopes. A
#   positive or negative value indicates the direction of the relationship
#   between the intercepts and slopes across groups.
# How to Interpret Variance in Random Slopes:
# - High Variance: If the variance of the random slope is large, it means there
#   is substantial variation in the effect of the predictor (ses) across groups
#   (schools). In other words, the relationship between ses and mathach differs
#   substantially from one school to another.
# - Low Variance: A small variance suggests that the effect of the predictor is
#   relatively consistent across groups.


# 5-----------------------------------------------------------------------------
# Step 5: Add cross-level interactions
ML_level1_2_RE_CL <- lmer(mathach ~ 1 + ses * catholic  + (1 + ses | id), hsb)
#--> too complex ("Model failed to converge"); let's use brm instead
ML_level1_2_RE_CL <- brm(mathach ~ 1 + ses * catholic  + (1 + ses | id), hsb)
summary(ML_level1_2_RE_CL)

#--> Look at the credibility intervals: effects are significant if zero is not
# included. Thus, the effect of ses on mathach is lower for catholic schools 
# than for public schools.

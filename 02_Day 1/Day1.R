### Author:   Joran Jongerling & Leonie Vogelsmeier
### Created:  15-05-2022
### Modified: 

#==============================================================================#
# Day 1 Practical With Solutions
#==============================================================================#

#------------------------------------------------------------------------------#
## 7.1 Introduction
#------------------------------------------------------------------------------#

### Packages required for Chapter 7---------------------------------------------

library(gridExtra)
library(knitr)
library(kableExtra)
library(lme4)
library(ICC)
library(knitr)
library(tidyverse)

#------------------------------------------------------------------------------#
## 7.6 Scenario 1: No Covariates
#------------------------------------------------------------------------------#

### Code chapter----------------------------------------------------------------

set.seed(2)  # to get the same simulated results as reported here
pi_1a <- rep(0.5, 24)
count_1a <- rbinom(24, 10, pi_1a)

pi_1b <- rbeta(24,.5,.5)  
count_1b <- rbinom(24, 10, pi_1b)  


### Additional code thought questions------------------------------------------- 

scenario_1 <- tibble(pi_1a, count_1a, pi_1b, count_1b) %>%
  mutate(phat_1a = count_1a / 10, phat_1b = count_1b / 10)

#### Q1: Fit model binomial-----------------------------------------------------
fit1 <- glm(cbind(count_1a, 10 - count_1a) ~ 1, # or use phat_1a
            family = binomial, 
            data = scenario_1) # for details, see Chapter 6
summary(fit1) # "~ 1" because it is an intercept only model
coef(fit1)[1] # 0.06669137 (intercept)

#### Calculate p_head manually
# p_head/(1-p_head) = exp(coef(fit1)[1])
exp(coef(fit1)[1]) # = 1.069295
# p_head/(1-p_head) = 1.069295
# p_head = 1.069295 * (1-p_head)
# p_head = 1.069295 - p_head * 1.069295
# 2.069295 * p_head = 1.069295
# p_head = 1.069295/2.069295
1.069295/2.069295 # = 0.5167436
# p_head = = 0.5167436

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

#### A1: Yes, each pup can be observed as an independent draw from the same 
####     process with p=.5.


#### Q2: Fit model quasibinomial------------------------------------------------

#### A2: No! Pups don't all come from the same process anymore. Some come from 
####     the process with p=.5, some from a process with p=.75, and still others 
####     from a process with p=.25. So, some pups come from a process where the 
####     flip of a fair coin determines if they have a defect, but others are 
####     "assigned" defects by a coin that comes up heads 75% of the time or 
####     just 25% of the time.

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


#### Q3_a: Fit model binomial---------------------------------------------------

#### A3: Yes, genetic variation can influence the probability of birth defects, 
####     as can age, and if dams differ with respect to these factors, the value 
####     of p will differ across them (although the range of variation in p does
####     not have to be as large as in this example).

fit3_a <- glm(cbind(count_1b, 10 - count_1b) ~ 1, # or use phat_1b
              family = binomial, 
              data = scenario_1) # for details, see Chapter 6
summary(fit3_a) # "~ 1" because it is an intercept only model
coef(fit3_a)[1] # 0.06669137 (intercept)


#### Calculate p_head with function (only for intercept only model)
p_head_function(exp(coef(fit3_a)[1]))

#### Calculate CI = exp(parameter -/+ 1.96 * SE)
exp(confint(fit3_a))

#### Calculate CI for p_head
p_head_function(exp(coef(fit3_a)[1] - 1.96 * sqrt(vcov(fit3_a))))
p_head_function(exp(coef(fit3_a)[1] + 1.96 * sqrt(vcov(fit3_a))))


#### Q3_b: Fit model quasibinomial----------------------------------------------

fit3_b <- glm(cbind(count_1b, 10 - count_1b) ~ 1, 
              family = quasibinomial, 
              data = scenario_1) 
summary(fit3_b)
coef(fit3_b)[1] # 0.06669137 (intercept)

#### Calculate p_head with function (only for intercept only model)
p_head_function(exp(coef(fit3_b)[1]))

#### Calculate CI = exp(parameter -/+ 1.96 * ("dispersion * SE"))
exp(confint(fit3_b))

#### Calculate CI for p_head
p_head_function(exp(coef(fit3_b)[1] - 1.96 * sqrt(vcov(fit3_b))))
p_head_function(exp(coef(fit3_b)[1] + 1.96 * sqrt(vcov(fit3_b))))

#### Q4: Obtain values to complete the table------------------------------------
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

#### Q5: Differences between quasibinomial and binomial models------------------

#### A5: In the 1a model, there is less estimated variance than in the 1b model. 
####     Since the overdispersion correction is multiplied with SEs etc., the 
####     overdispersion correction is multiplied with a smaller number in the 1a
####     model than in the 1b model.

#------------------------------------------------------------------------------#
## 7.7 Scenario 2: Dose Effect
#------------------------------------------------------------------------------#


### Additional code thought questions-------------------------------------------
x <- 0:3
p_2 <- exp(-2+4/3*x)/(1+exp(-2+4/3*x))
p_2

set.seed(1)

dose <- c(rep(0,6),rep(1,6),rep(2,6),rep(3,6))

pi_2a <- exp(-2+4/3*dose)/(1+exp(-2+4/3*dose))
count_2a <- rbinom(24, 10, pi_2a)

b <- 2
a <- b*pi_2a / (1-pi_2a)
pi_2b <- rbeta(24, a, b)
count_2b <- rbinom(24, 10, pi_2b)  

scenario_2 <- tibble(dose, pi_2a, count_2a, pi_2b, count_2b)
theoretical_pi <- tibble(x = 1:50000,
                         p1 = rbeta(x, shape1 = 2*p_2[1]/(1-p_2[1]), shape2 = 2),
                         p2 = rbeta(x, shape1 = 2*p_2[2]/(1-p_2[2]), shape2 = 2),
                         p3 = rbeta(x, shape1 = 2*p_2[3]/(1-p_2[3]), shape2 = 2),
                         p4 = rbeta(x, shape1 = 2*p_2[4]/(1-p_2[4]), shape2 = 2))

scenario_2 <- scenario_2 %>%
  mutate(dose = dose,
         phat_2a = count_2a / 10, phat_2b = count_2b / 10,
         logit_2a = log ((count_2a + 0.5) / (10 - count_2a + 0.5)),
         logit_2b = log ((count_2b + 0.5) / (10 - count_2b + 0.5)) )

#### Q6_a2: Fit model binomial--------------------------------------------------!!!!!!! CHECK

#### A6: Probabilities are .517 and .567 for scenarios 1a and 1b, and those 
####     probabilities are assumed to apply to every dam (no matter the dosage 
####     she received). Here we see that those are the probabilities that apply 
####     to an "average dosage" somewhere between dosage=1 and dosage=2. Note
####     that whereas the probability at 1b is larger than the one at 1a, 
####     the probabilities in 2b are always lower than the ones in 2a. This can 
####     be due to the skew in the distributions of p within each Dosage group 
####     (but it doesn't match what they say later.need to check with R-code).

fit6_a1 <- glm(cbind(count_2a, 10 - count_2a) ~ dose, 
              family = binomial, 
              data = scenario_2) 
summary(fit6_a1)
coef(fit6_a1) # -2.017255 (intercept); 1.263172 (dose)
exp(confint(fit6_a1))


#### Q6_a2: Fit model quasibinomial---------------------------------------------

fit6_a2 <- glm(cbind(count_2a, 10 - count_2a) ~ dose, 
              family = quasibinomial, 
              data = scenario_2) 
summary(fit6_a2)
coef(fit6_a2) # -2.017255 (intercept); 1.263172 (dose)
exp(confint(fit6_a2))

#### Q6_b1: Fit model quasibinomial---------------------------------------------

fit6_b1 <- glm(cbind(count_2b, 10 - count_2b) ~ dose, 
              family = binomial, 
              data = scenario_2) 
summary(fit6_b1)
coef(fit6_b1) # -2.407117 (intercept); 1.461178 (dose)
exp(confint(fit6_b1))

#### Q6_b2: Fit model quasibinomial----------------------------------------------

fit6_b2 <- glm(cbind(count_2b, 10 - count_2b) ~ dose, 
              family = quasibinomial, 
              data = scenario_2) 
summary(fit6_b2)
coef(fit6_b2) # -2.407117 (intercept); 1.461178 (dose)
exp(confint(fit6_b2))

#### Q7: Means------------------------------------------------------------------
scenario_2 %>% 
  summarise(mean_2a = mean(count_2a), sd_2a = sd(count_2a),
            mean_2b = mean(count_2b), sd_2b = sd(count_2b) )

#### A7: The probability depends on Dose, so you want to know the probability 
####     for different doses. The average probability is the probability at a 
####     Dose=1.5, which is a score that doesn't exist. So it is hard to 
####     interpret.

#### Q8: CIs--------------------------------------------------------------------

#### A8: The intervals for the b-scenarios will always be wider than those of
####     the a-scenarios because more sources of variance are accounted for. 
####     Similarly, the quasi-binomial intervals will be wider than the binomial 
####     ones within a-scenarios and b-scenarios, again because more variance is 
####     accounted for. The point estimates will be the same between the 
####     binomial and quasi-binomial models, however, within each type of 
####     scenario.

exp(confint(fit6_a1))
exp(confint(fit6_a2))
exp(confint(fit6_b1))
exp(confint(fit6_b2))

#### Q9: Summary of the findings------------------------------------------------

#### A9: See the answer A4.

#### Q10: Why diff. betw. q_bin. and bin. less noticeable in 2a than in 2b------

#### A10: See the answer A5.

#### Q11: Why accounting for correlated data in 2b------------------------------

#### A11: In scenario 2b, pups from the same dam are more alike than pups from 
####      different dams. Pups from a single dam have the same probability of 
####      deformation (which depends on the dosage that dam got and the 
####      characteristics of the dam herself). In contrast, pups from different 
####      dams have different probabilities of deformation.



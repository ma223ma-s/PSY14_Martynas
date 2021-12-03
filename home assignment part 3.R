#load in packages

library(psych)
library(tidyverse)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)


#custom function adapted from: https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}


#load in the dataset


data_pain_3 = read.csv("https://tinyurl.com/b385chpu")

data_pain_4 = read.csv("https://tinyurl.com/4f8thztv")


#adjust variables for ease of analysis


data_pain_3 = data_pain_3 %>% 
  mutate(household_income1000 = household_income / 1000, sex = factor(sex),
                hospital = factor(hospital))


data_pain_4 = data_pain_4 %>% 
  mutate(household_income1000 = household_income / 1000, sex = factor(sex),
                hospital = factor(hospital))



#correct coding errors in the data set (none excluded)

data_pain_corr_2 <- data_pain_3 %>% 
  mutate(sex = replace(sex, sex == "woman", "female"),
         
         household_income = replace(household_income,
                    household_income == -7884, 7884),
      
                        household_income1000 = replace(household_income1000,
                               household_income1000 == -7.884, 7.884))
          
         
levels(data_pain_corr_2$sex) <- c("female", "male", "woman" = "female")

summary(data_pain_corr_2)

summary(data_pain_4)



#building the models

mod_pain_reg <- lm(pain ~ age + sex + STAI_trait + pain_cat + 
                      mindfulness + cortisol_serum,
                           data = data_pain_corr_2)

summary(mod_pain_reg)


mod_pain_ran <- lmer(pain ~ age + sex + STAI_trait + pain_cat +
                       mindfulness + cortisol_serum + (1 | hospital),
                          data = data_pain_corr_2)
 

summary(mod_pain_ran)


#confidence intervals and standardized mixed model coefficients

confint(mod_pain_ran, level = .90)


stdCoef.merMod(mod_pain_ran)


#testing cAIC to check for model fit, and ANOVA

cAIC(mod_pain_ran)$caic

cAIC(mod_pain_mix)$caic

anova(mod_pain_ran, mod_pain_mix)



#marginal and conditional R^2

r2beta(mod_pain_ran, method = "nsj", data = data_pain_corr_2)

r.squaredGLMM(mod_pain_ran)


#Pain score predictions on data file four

pred_test_ran <- predict(mod_pain_ran, data_pain_4, allow.new.levels = T)

pred_test_ran


#sum of squared residuals

RSS_value = sum((data_pain_4["pain"] - pred_test_ran)^2)

RSS_value


#total sum of squares

mod_mean_value <- lmer(pain ~ 1 + (1 | hospital), data = data_pain_4)

TSS_value = sum((data_pain_4["pain"] - predict(mod_mean_value))^2)

TSS_value


#variance explained by the model

var_exp <- 1 - (RSS_value/TSS_value)

var_exp


mod_pain_mix <- lmer(pain ~ cortisol_serum + (cortisol_serum | hospital),
                                data = data_pain_corr_2)

summary(mod_pain_mix)



#Visualizing the mixed effects model (intercept and slope)

data_pain_corr_2 = data_pain_corr_2 %>%
  mutate(pred_int = predict(mod_pain_ran), pred_slope = predict(mod_pain_mix))


data_pain_corr_2 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
      geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
              aes(y = pred_int, x = cortisol_serum)) + facet_wrap(~ hospital, ncol = 2)


data_pain_corr_2 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
      geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
              aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~ hospital, ncol = 2)







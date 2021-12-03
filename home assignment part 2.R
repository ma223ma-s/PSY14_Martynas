#load in packages

library(psych)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)
library(tidyverse)
library(lm.beta)


coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}




#load in the dataset

data_pain = read.csv("https://tinyurl.com/yxm5rd89")

#adjust variables for ease of analysis

data_pain = data_pain %>% 
  mutate(household_income1000 = household_income / 1000, sex = factor(sex))



#correct coding errors in the data set (none excluded)

data_pain_corr <- data_pain %>% 
  mutate(pain = replace(pain, pain == "55", 5),
         STAI_trait = replace(STAI_trait, STAI_trait == "4.2", 42))


summary(data_pain_corr)


mod_pain_full2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + 
                      mindfulness + cortisol_serum + weight + IQ +
                       household_income1000,
                          data = data_pain_corr)


summary(mod_pain_full2)



#Checking Cook's distance, residuals & leverage

mod_pain_full2 %>% 
  plot(which = 5)

#some values above 0.25 threshold, however will be kept to make the model more representative

mod_pain_full2 %>% 
  plot(which = 4)


#testing for normality with a QQ plot

mod_pain_full2 %>% 
  plot(which = 2)



#plotting a histogram to check for normal distribution of residuals

residuals_mod_pain_full2 = enframe(residuals(mod_pain_full2))
residuals_mod_pain_full2 %>%
  ggplot() + aes(x = value) + geom_histogram(binwidth = 0.3)



#measure of skew and kurtosis (normality is not violated)

describe(residuals(mod_pain_full2))



#testing for linearity (no p values are significant)

mod_pain_full2 %>% 
  residualPlots()



#testing for homoscedasticity (assumption for homoscedasticity not violated)

mod_pain_full2 %>% 
  plot(which = 3)

#NCV test not significant

mod_pain_full2 %>% 
  ncvTest()


#testing for multicollinearity (all VIF values are < 3)

mod_pain_full2 %>%
  vif()



#running a backward regression on the initial model

mod_back_pain = step(mod_pain_full2, direction = "backward")



#saving the models into objects for later comparison

backward_model <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum,
                      data = data_pain_corr)

summary(backward_model)


theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + 
                              mindfulness + cortisol_serum,
                                  data = data_pain_corr)

summary(theory_based_model)




#AIC measures

AIC(backward_model)

AIC(theory_based_model)

AIC(mod_pain_full2)


#confidence intervals

confint(backward_model, level = .90)

#standardized coefficients

lm.beta(backward_model)

coef_table(backward_model)


#ANOVA

anova(backward_model, theory_based_model)

anova(backward_model, mod_pain_full2)



#load in the newly collected data set

data_pain_2 = read.csv("https://tinyurl.com/87v6emky")


#mutate the variables

data_pain_2 = data_pain_2 %>% 
  mutate(household_income1000 = household_income / 1000, sex = factor(sex))



#predicted values

pred_test_back <- predict(backward_model, data_pain_2)

pred_test_back

pred_test_theory <- predict(theory_based_model, data_pain_2)

pred_test_theory

data_pain_2$pain



#comparing predicted pain with actual scores (sum of squared residuals)

diff_pain_back = sum((data_pain_2["pain"] - pred_test_back)^2)

diff_pain_back

diff_pain_theory = sum((data_pain_2["pain"] - pred_test_theory)^2)

diff_pain_theory






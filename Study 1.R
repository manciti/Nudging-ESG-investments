###############################
####### Analysis Code ########
###############################

library(tidyverse)
library(ggplot2)
library(stargazer)
library(plyr)
library(psych)
library(corrplot)
library(corrtable)
library(marginaleffects)

# Setting the working directory
# setwd("C:/Users/mpasq/OneDrive - Università Politecnica delle Marche/Desktop/Dottorato/TESI/Capitolo 2/Analisi R/Directory")

# import data frame from .csv file
data <- read.csv("investData.csv")

# Dropping the variable "X", a sequential number automatically generated while creating the .csv file
data = select(data, -1)

# Checking data structure
str(data)

# All ordinal variables have lost their order in the transition to .csv, so we re-encode them as ordinal variables.
# Also, categorical variables have lost the status as categorical variable, so we re-encode them

###########################################################################
########## PART 1 - DATA HANDLING #########################################
###########################################################################

## Age
table(data$age)
data <- data%>%
  mutate(age = case_match(age,
                          "Between 18 and 24" ~ 21,
                          "Between 25 and 34" ~ 30,
                          "Between 35 and 44" ~ 40,
                          "Between 45 and 54" ~ 50,
                          "More than 55" ~ 55))

## Gender
data <- data %>%
  mutate(gender_female = case_match(gender_fem, 
                                    "Female" ~ 1,
                                    "Male" ~ 0,
                                    "Other" ~ 0))
data <- data %>%
  relocate(gender_female, .after = gender_fem)

data <- select(data, -gender_fem)

## Country
table(data$country)
data <- data %>%
  mutate(country = factor(country))

# Income
data <- data %>%
  mutate (income = income_bracket)

data <- data%>%
  mutate(income = case_match(income,
                             "<1000 EUR" ~ 1,
                             "1000-1500 EUR" ~ 1.25,
                             "1500-2000 EUR" ~ 1.75,
                             "2000-2500 EUR" ~ 2.25,
                             "2500-3000 EUR" ~ 2.75,
                             "3000-5000 EUR" ~ 4,
                             ">5000 EUR" ~ 5))
data <- data%>%
  relocate (income, .after=income_bracket)
data <- select(data, -income_bracket)

## savings
data <- data %>%
  mutate(savings = factor(savings, ordered = TRUE,
                          levels = c("<10% of income",
                                     "10-30% of income",
                                     " >30% of income")))

# converting "savings" into a numerical variable 
data <- data%>%
  mutate(savings = as.character(savings))
data <- data%>%
  mutate(savings = case_match(savings, 
                              '<10% of income' ~ 10,
                              '10-30% of income' ~ 20,
                              '>30% of income' ~ 30))
# it can't convert '>30% of income' into 30, so i'll replace na values with 30 manually
data$savings[is.na(data$savings)] <- 30

# Debt
data <- data %>%
  mutate(debt = family_hasDebt)

data <- data %>% 
  mutate(debt = case_match(debt,
                           "1 - Totally Disagree" ~ 1, 
                           "2 - Disagree" ~ 2,
                           "3 - Neither Agree or Disagree" ~ 3,
                           "4 - Agree" ~ 4,
                           "5 - Totally Agree" ~ 5))

data <- data %>% 
  relocate(debt, .after=family_hasDebt) 
data = select(data, -family_hasDebt)

## education
data <- data %>% mutate(education = case_match(education,
                                               "Less than Primary" ~ 0, 
                                               "Primary" ~ 5,
                                               "Lower Secondary" ~ 8,
                                               "Upper Secondary" ~ 12,
                                               "Post Secondary" ~ 13,
                                               "Short tertiary" ~ 14,
                                               "Bachelor" ~ 15,
                                               "Masters" ~ 17,
                                               "Doctoral" ~ 20))

## Investor Yes
data <- data %>%
  mutate(investoryes = case_match(advisor_reliance, 
                                  0 ~ 0,
                                  1 ~ 1,
                                  2 ~ 1,
                                  3 ~ 1))
data <- data %>%
  relocate(investoryes, .after = advisor_reliance)

# Advisor_reliance
data <- data %>%
  mutate(advisor_reliance = case_match(advisor_reliance, 
                                       0 ~ 0,
                                       1 ~ 0,
                                       2 ~ 1,
                                       3 ~ 2))

## owns_sustInvest
data <- data %>%
  mutate(owns_sustInvest = case_match(owns_sustInvest,
                                      "Yes" ~ 1,
                                      "No" ~ 0))

## past_sustInvest
data <- data %>%
  mutate(past_sustInvest = case_match(past_sustInvest,
                                      "Yes" ~ 1,
                                      "No" ~ 0))

## sust_preference
data <- data%>%
  mutate(sust_preference = case_match(sust_preference,
                                      "No" ~ 0,
                                      "Do not know" ~ 1,
                                      "Yes" ~ 1))

## advisor_useful
data <- data %>%
  mutate(adv_usefulness = advisor_useful)

data <- data %>% 
  mutate(adv_usefulness = case_match(adv_usefulness,
                                     "1 - Totally Disagree" ~ 1, 
                                     "2 - Disagree" ~ 2,
                                     "3 - Neither Agree or Disagree" ~ 3,
                                     "4 - Agree" ~ 4,
                                     "5- Totally Agree" ~ 5))

data <- data %>% relocate(adv_usefulness, .after=advisor_useful)
data = select(data, -advisor_useful)

## advisor_trustworthy
data <- data %>%
  mutate(adv_trustworthiness = advisor_trustworthy)

data <- data %>% 
  mutate(adv_trustworthiness = case_match(adv_trustworthiness,
                                          "1 - Totally Disagree" ~ 1, 
                                          "2 - Disagree" ~ 2,
                                          "3 - Neither Agree or Disagree" ~ 3,
                                          "4 - Agree" ~ 4,
                                          "5- Totally Agree" ~ 5))

data <- data %>% relocate(adv_trustworthiness, .after=advisor_trustworthy)
data = select(data, -advisor_trustworthy)

## trust_SustFunds
data <- data %>% mutate(trusting_Sustfunds = trust_SustFunds)


data <- data %>% mutate(trusting_Sustfunds = case_match(trusting_Sustfunds,
                                                        "1- Totally Disagree" ~ 1, 
                                                        "2 - Disagree" ~ 2,
                                                        "3 - Neither Agree or Disagree" ~ 3,
                                                        "4 - Agree" ~ 4,
                                                        "5 - Totally Agree" ~ 5))
data <- data%>%
  relocate(trusting_Sustfunds, .after = trust_SustFunds)
data = select(data, -trust_SustFunds)

## perceived_effect
data <- data %>% mutate(sust_investPositive = perceived_effect)

data <- data %>% mutate(sust_investPositive = case_match(sust_investPositive,
                                                         "1- Totally Disagree" ~ 1, 
                                                         "2 - Disagree" ~ 2,
                                                         "3 - Neither Agree or Disagree" ~ 3,
                                                         "4 - Agree" ~ 4,
                                                         "5 - Totally Agree" ~ 5))
data <- data%>%
  relocate(sust_investPositive, .after = perceived_effect)
data = select(data, -perceived_effect)

view(data)

################################################################################################
########################## PART 3 - T-TEST #####################################################
##############################################################################################

datacontrol = subset(data, treatment == 0)
datatreatment = subset(data, treatment == 1)

t.test(datatreatment$age, datacontrol$age, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$gender_female, datacontrol$gender_female, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$income, datacontrol$income, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$savings, datacontrol$savings, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$debt, datacontrol$debt, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$household, datacontrol$household, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$Household_n.Incomes, datacontrol$Household_n.Incomes, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$education, datacontrol$education, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$fin_literacy, datacontrol$fin_literacy, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$sust_literacy, datacontrol$sust_literacy, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$experience, datacontrol$experience, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$investoryes, datacontrol$investoryes, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$advisor_reliance, datacontrol$advisor_reliance, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$owns_sustInvest, datacontrol$owns_sustInvest, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$past_sustInvest, datacontrol$past_sustInvest, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$risk_preference, datacontrol$risk_preference, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$time_preference, datacontrol$time_preference, var.equal = TRUE, conf.level=0.90)
t.test(datatreatment$sust_preference, datacontrol$sust_preference, var.equal = TRUE, conf.level=0.90)


####################################################################
#################### PART 4 - CORRELATIONS #########################
####################################################################

#################### Correlation Matrix ##########################
##################################################################
# creating a correlation matrix using only numerical variables (+ 'savings')
data2 = select(data, -1, -inv_equityFundX, -inv_equityFundY, -inv_sustFund, -inv_bondFund, -country,
               -owns_sustInvest, -past_sustInvest, -treatment, -adv_usefulness, -adv_trustworthiness, 
               -trusting_Sustfunds, -sust_investPositive)

data2 <- data2%>%
  relocate(risk_preference, .after = sust_literacy)

data2 <- data2%>%
  relocate(time_preference, .after = risk_preference)

data2 <- data2%>%
  relocate(sust_preference, .after = experience)

view(data2)
  
# Correlation Matrix (total)
correlation_matrix(data2, digits=2, use = 'lower', show_significance = TRUE, replacement = " ")
save_correlation_matrix(df = data2, filename = 'corrmatrix.csv', digits = 2, use = 'lower',  show_significance = TRUE)

# Correlogram of correlation matrix
correlation.matrix <- cor(data2)
correlation.matrix[upper.tri(correlation.matrix)] <- NA
stargazer(correlation.matrix, title='correlation matrix', type='text', out = 'correlation.html')
correlogram <- corrplot(correlation.matrix, tl.cex = 1, tl.col = 'black', tl.srt = 45, type = 'lower')

# Correlation matrix only on investors
data3 = subset(data2, investoryes == 1)
data3 = select(data3, -investoryes)
correlation_matrix(data3, digits=2, use = 'lower', show_significance = TRUE, replacement = " ")
save_correlation_matrix(df = data3, filename = 'corrmatrix2.csv', digits = 2, use = 'lower',  show_significance = TRUE)

######## Subsitution Effect - measuring correlations ######
###########################################################

datainvest <- select(data, 'inv_equityFundX', 'inv_bondFund', 'inv_equityFundY', 'inv_sustFund')
correlation_matrix(datainvest, digits=2, use = 'lower', show_significance = TRUE, replacement = " ")
save_correlation_matrix(df = datainvest, filename = 'corrmatrixinvest.csv', digits = 2, use = 'lower',  show_significance = TRUE)

datatreatment = subset(data, treatment == 1)
datainvesttreat <- select(datatreatment, 'inv_equityFundX', 'inv_bondFund', 'inv_equityFundY', 'inv_sustFund')
correlation_matrix(datainvesttreat, digits=2, use = 'lower', show_significance = TRUE, replacement = " ")
save_correlation_matrix(df = datainvesttreat, filename = 'corrmatrixinvest2a.csv', digits = 2, use = 'lower',  show_significance = TRUE)


datacontrol = subset(data, treatment == 0)
datainvestcontr <- select(datacontrol, 'inv_equityFundX', 'inv_bondFund', 'inv_equityFundY', 'inv_sustFund')
correlation_matrix(datainvestcontr, digits=2, use = 'lower', show_significance = TRUE, replacement = " ")
save_correlation_matrix(df = datainvestcontr, filename = 'corrmatrixinvest2b.csv', digits = 2, use = 'lower',  show_significance = TRUE)

stargazer(datainvest, title ='Average Investment Portfolio Total Sample', type = 'text', digits = 2, out = 'Portfolio.html')
stargazer(datainvesttreat, title ='Average Investment Portfolio Treatment Group', type = 'text', digits = 2, out = 'Portfolio.html')
stargazer(datainvestcontr, title ='Average Investment Portfolio Control Group', type = 'text', digits = 2, out = 'Portfolio.html')

######## Chi-squared test for measuring correlations
###################################################
chisq.test(data$investoryes, data$experience)
chisq.test(data$investoryes, data$advisor_reliance)
chisq.test(data$advisor_reliance, data$experience)


#########################################################################
########### PART 5 - INVESTOR PROFILES #################################

################ comparison of full models ###############
#########################################################

RegA <- lm(inv_equityFundX ~ age + gender_female + country + income + savings + debt
           + education + fin_literacy + sust_literacy + risk_preference + time_preference + experience
           + sust_preference + investoryes + advisor_reliance + treatment, data)

RegB <- lm(inv_bondFund ~ age + gender_female + country + income + savings + debt
           + education + fin_literacy + sust_literacy + risk_preference + time_preference + experience
           + sust_preference + investoryes + advisor_reliance + treatment, data)

RegC <- lm(inv_equityFundY ~ age + gender_female + country + income + savings + debt
           + education + fin_literacy + sust_literacy + risk_preference + time_preference + experience
           + sust_preference + investoryes + advisor_reliance + treatment, data)

RegD <- lm(inv_sustFund ~ age + gender_female + country + income + savings + debt
           + education + fin_literacy + sust_literacy + risk_preference + time_preference + experience
           + sust_preference + investoryes + advisor_reliance + treatment, data)

stargazer(RegA, RegB, RegC, RegD, title = "Investors profile", type = "text", out = 'profiles.html')

################## comparison of single models ############
###########################################################

# Sociodemographic variables
Reg1 <- lm(inv_equityFundX ~ age + gender_female + country + income + savings + debt
           + education, data)
Reg2 <- lm(inv_bondFund ~ age + gender_female + country + income + savings + debt
           + education, data)
Reg3 <- lm(inv_equityFundY ~ age + gender_female + country + income + savings + debt
           + education, data)
Reg4 <- lm(inv_sustFund ~ age + gender_female + country + income + savings + debt
           + education, data)
stargazer(Reg1, Reg2, Reg3, Reg4, title = "Sociodemographic variables", type = "text", out = 'Sociodemographic variables.html')

# Literacy variables
Reg1 <- lm(inv_equityFundX ~ age + gender_female + country + income + savings + debt
           + education + fin_literacy + sust_literacy, data)
Reg2 <- lm(inv_bondFund ~ age + gender_female + country + income + savings + debt
           + education + fin_literacy + sust_literacy, data)
Reg3 <- lm(inv_equityFundY ~ age + gender_female + country + income + savings + debt
           + education + fin_literacy + sust_literacy, data)
Reg4 <- lm(inv_sustFund ~ age + gender_female + country + income + savings + debt
           + education + fin_literacy + sust_literacy, data)
stargazer(Reg1, Reg2, Reg3, Reg4, title = "Literacy variables", type = "text", out = 'Literacy variables.html')

# Investment profile
Reg1 <- lm(inv_equityFundX ~ age + gender_female + country + income + savings + debt
           + education + risk_preference + time_preference + experience + sust_preference + investoryes + advisor_reliance, data)
Reg2 <- lm(inv_bondFund ~ age + gender_female + country + income + savings + debt
           + education + risk_preference + time_preference + experience + sust_preference + investoryes + advisor_reliance, data)
Reg3 <- lm(inv_equityFundY ~ age + gender_female + country + income + savings + debt
           + education + risk_preference + time_preference + experience + sust_preference + investoryes + advisor_reliance, data)
Reg4 <- lm(inv_sustFund ~ age + gender_female + country + income + savings + debt
           + education + risk_preference + time_preference + experience + sust_preference + investoryes + advisor_reliance, data)
stargazer(Reg1, Reg2, Reg3, Reg4, title = "Investment profile variables", type = "text", out = 'Investment profile variables.html')


###################################################################
################ PART 6 - ATE & CATE ESTIMATIONS ##################
###################################################################

################################# estimating ATE 
ATE <- lm(data = data, inv_sustFund ~ treatment)
summary(ATE)
stargazer(ATE, type = "text")
# The treatment is significant at the .05 level

######### estimating heterogeneity effects of treatment (CATE)

## CATE Fin_literacy
CATEFin_literacy <- lm(data = data, inv_sustFund ~ treatment*fin_literacy)
summary(CATEFin_literacy)

## CATE Sust_literacy
CATESust_literacy <- lm(data = data, inv_sustFund ~ treatment*sust_literacy)
summary(CATESust_literacy)

# CATE Sustainable Preferences
CATEPref <- lm(data = data, inv_sustFund ~ treatment*sust_preference)
summary(CATEPref)
stargazer(CATEPref, type = 'text', out = 'CATEPref.html')

################# ATE & CATE Plots #############
# ATE Plot
plot_predictions(ATE, by = "treatment") + ylab("% invested in sustainable fund") +
  xlab("Control group (0), Treated group (1)")

# CATE Plots
# Financial Literacy
plot_predictions(CATEFin_literacy, by = c("fin_literacy", "treatment")) + ylab("% invested in sustainable fund") +
  xlab("Financial Literacy")

# Sustainable Literacy
plot_predictions(CATESust_literacy, by = c("sust_literacy", "treatment")) + ylab("% invested in sustainable fund") +
  xlab("Sustainable Literacy")

# Sustainable Preferences
plot_predictions(CATEPref, by = c("sust_preference", "treatment")) + ylab("% invested in sustainable fund") +
  xlab("Declared sustainable preferences")


########################################################
###### PART 7 - MEDIATION EFFECTS  - PROCESS ANALYSIS###
########################################################
# process(data=data, y="inv_sustFund",x="fin_literacy", m=c("PCE","trust"), w="treatment", model=6, wmatrix=c(0,0,0,1,0,0))

# process(data=data, y="inv_sustFund",x="sust_literacy", m=c("PCE","trust"), w="treatment", model=6, wmatrix=c(0,0,0,1,0,0))

##########################################################
################## PART 8 - Perception of the advisor ####
##########################################################

# Additional analysis on the possible interaction between treatment and perception of the advisor

Reg1 <- lm(inv_sustFund ~ treatment + adv_usefulness + adv_usefulness*treatment, data)
Reg2 <- lm(inv_sustFund ~ treatment + adv_trustworthiness + adv_trustworthiness*treatment, data)
Reg3 <- lm(inv_sustFund ~ age + gender_female + country + income + savings + debt
           + household + Household_n.Incomes + education + treatment + adv_usefulness + adv_usefulness*treatment, data)
Reg4 <- lm(inv_sustFund ~ age + gender_female + country + income + savings + debt
           + household + Household_n.Incomes + education + treatment + adv_trustworthiness + adv_trustworthiness*treatment, data)
stargazer(Reg1, Reg2, Reg3, Reg4, title = "Regression Output", type = "text", out = 'Interaction2.html')



CATEadv_useful <- lm(inv_sustFund ~ treatment*adv_usefulness, data)

plot_predictions(CATEadv_useful, by = c("adv_usefulness", "treatment")) + ylab("% Invested in Sustainable Fund") +
  xlab("Advisor Usefulness")

########################## Quantile Treatment Effect ########################################################
#############################################################################################################


#### Loading the relevant library
library(quantreg)

taus <- seq(0.1, 0.9, by = 0.1)

QTE_full <- rq(inv_sustFund ~ treatment + 
                 age + gender_female + country + income + savings + debt + 
                 education + household + Household_n.Incomes, 
               tau = taus, data = data)



# We visualize the quantile treatment effect across the full quantile range
plot(summary(QTE_full), parm = "treatment", 
     main = "Treatment Effect Across Quantiles",
     ylab = "Treatment Coefficient",
     xlab = "Quantile")

# Ensure axis labels are rendered (some plot methods ignore xlab/ylab passed via ...)
title(xlab = "Quantile", ylab = "Treatment Coefficient")

# Save the same plot as a high-resolution PNG in the project folder
png(filename = "QTE_results.png", width = 2000, height = 1400, res = 300)
plot(summary(QTE_full), parm = "treatment", 
  main = "Treatment Effect Across Quantiles",
  ylab = "Treatment Coefficient",
  xlab = "Quantile")
title(xlab = "Quantile", ylab = "Treatment Coefficient")
dev.off()


########################## Conditional Quantile Treatment Effect (Financial Literacy) #######################
#############################################################################################################

# Create binary financial literacy variable
data <- data %>%
  mutate(fin_literacy_binary = case_when(
    fin_literacy %in% c(0, 1) ~ 0,  # Low literacy
    fin_literacy %in% c(2, 3) ~ 1,  # High literacy
    TRUE ~ NA_real_
  ))

# Quantiles and model with interaction
taus <- seq(0.1, 0.9, by = 0.1)

CQTE_fin_literacy <- rq(inv_sustFund ~ treatment*fin_literacy_binary +
                  age + gender_female + country + income + savings + debt +
                  education + household + Household_n.Incomes,
                tau = taus, data = data)

# Summaries with covariance matrices for confidence intervals
sfm <- summary(CQTE_fin_literacy, se = "boot", covariance = TRUE, R = 500)

# Build effects for each literacy level
alpha <- 0.10
z     <- qnorm(1 - alpha/2)
levels_fl <- c(0, 1)  # Binary: Low (0) and High (1)

rows <- list()
for (j in seq_along(taus)) {
  coefs <- sfm[[j]]$coefficients
  V     <- sfm[[j]]$cov
  
  # Set dimnames if missing
  if (is.null(dimnames(V))) {
    dimnames(V) <- list(rownames(coefs), rownames(coefs))
  }
  
  b_treat <- coefs["treatment", 1]
  b_int   <- coefs["treatment:fin_literacy_binary", 1]

  for (ell in levels_fl) {
    eff <- b_treat + ell * b_int
    se2 <- V["treatment","treatment"] +
           (ell^2) * V["treatment:fin_literacy_binary","treatment:fin_literacy_binary"] +
           2 * ell * V["treatment","treatment:fin_literacy_binary"]
    se  <- sqrt(se2)
    
    lit_label <- ifelse(ell == 0, "Low", "High")
    rows[[length(rows) + 1]] <-
      data.frame(tau = taus[j], fin_literacy = factor(lit_label, levels = c("Low", "High")),
                 estimate = eff, lo = eff - z*se, hi = eff + z*se)
  }
}
df <- do.call(rbind, rows)

# Plot
p <- ggplot(df, aes(x = tau, y = estimate, color = fin_literacy, group = fin_literacy)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = fin_literacy), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = taus) +
  labs(title = "Conditional Quantile Treatment Effect by Financial Literacy",
       x = "Quantile", y = "Treatment effect",
       color = "Financial literacy", fill = "Financial literacy") +
  theme_minimal(base_size = 12)

print(p)

# Save as PNG
ggsave("CQTE_by_finlit.png", p, width = 8, height = 5.6, dpi = 300)

########################## Conditional Quantile Treatment Effect (Sustainable financial knowledge) ##########
#############################################################################################################


# Create binary sustainable literacy variable
data <- data %>%
  mutate(sust_literacy_binary = case_when(
    sust_literacy %in% c(0, 1, 2) ~ 0,  # Low literacy
    sust_literacy %in% c(3, 4, 5, 6) ~ 1,  # High literacy
    TRUE ~ NA_real_
  ))

# Quantiles and model with interaction
taus <- seq(0.1, 0.9, by = 0.1)

CQTE_sust_lit <- rq(inv_sustFund ~ treatment*sust_literacy_binary +
                  age + gender_female + country + income + savings + debt +
                  education + household + Household_n.Incomes,
                tau = taus, data = data)

# Summaries with covariance matrices for confidence intervals
sfm <- summary(CQTE_sust_lit, se = "boot", covariance = TRUE, R = 500)

# Build effects for each literacy level
alpha <- 0.10
z     <- qnorm(1 - alpha/2)
levels_sl <- c(0, 1)  # Binary: Low (0) and High (1)

rows <- list()
for (j in seq_along(taus)) {
  coefs <- sfm[[j]]$coefficients
  V     <- sfm[[j]]$cov
  
  # Set dimnames if missing
  if (is.null(dimnames(V))) {
    dimnames(V) <- list(rownames(coefs), rownames(coefs))
  }
  
  b_treat <- coefs["treatment", 1]
  b_int   <- coefs["treatment:sust_literacy_binary", 1]

  for (ell in levels_sl) {
    eff <- b_treat + ell * b_int
    se2 <- V["treatment","treatment"] +
           (ell^2) * V["treatment:sust_literacy_binary","treatment:sust_literacy_binary"] +
           2 * ell * V["treatment","treatment:sust_literacy_binary"]
    se  <- sqrt(se2)
    
    lit_label <- ifelse(ell == 0, "Low", "High")
    rows[[length(rows) + 1]] <-
      data.frame(tau = taus[j], sust_literacy = factor(lit_label, levels = c("Low", "High")),
                 estimate = eff, lo = eff - z*se, hi = eff + z*se)
  }
}
df <- do.call(rbind, rows)

# Plot
p <- ggplot(df, aes(x = tau, y = estimate, color = sust_literacy, group = sust_literacy)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = sust_literacy), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = taus) +
  labs(title = "Conditional Quantile Treatment Effect\nby Sustainable Financial Knowledge",
       x = "Quantile", y = "Treatment effect",
       color = "Sustainable Financial\nKnowledge", fill = "Sustainable Financial\nKnowledge") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, lineheight = 1.1))

print(p)

# Save as PNG
ggsave("CQTE_by_sustlit.png", p, width = 8, height = 5.6, dpi = 300)


########################## Conditional Quantile Treatment Effect (Sustainable preferences) ###############
#############################################################################################################

# Define quantiles
taus <- seq(0.1, 0.9, by = 0.1)

# Run quantile regression with sustainability preference interaction
CQTE_sust_pref <- rq(inv_sustFund ~ treatment*sust_preference +
                  age + gender_female + country + income + savings + debt +
                  education + household + Household_n.Incomes,
                tau = taus, data = data)

# Get summary with bootstrap standard errors and covariance matrix
sfm <- summary(CQTE_sust_pref, se = "boot", covariance = TRUE, R = 500)

# Build effects for each sustainability preference level
alpha <- 0.10
z     <- qnorm(1 - alpha/2)
levels_sp <- c(0, 1)  # Binary: Low (0) and High (1)

rows <- list()
for (j in seq_along(taus)) {
  coefs <- sfm[[j]]$coefficients
  V     <- sfm[[j]]$cov
  
  # Set dimnames if missing
  if (is.null(dimnames(V))) {
    dimnames(V) <- list(rownames(coefs), rownames(coefs))
  }
  
  b_treat <- coefs["treatment", 1]
  b_int   <- coefs["treatment:sust_preference", 1]

  for (sp in levels_sp) {
    eff <- b_treat + sp * b_int
    se2 <- V["treatment","treatment"] +
           (sp^2) * V["treatment:sust_preference","treatment:sust_preference"] +
           2 * sp * V["treatment","treatment:sust_preference"]
    se  <- sqrt(se2)
    
    pref_label <- ifelse(sp == 0, "Low", "High")
    rows[[length(rows) + 1]] <-
      data.frame(tau = taus[j], sust_preference = factor(pref_label, levels = c("Low", "High")),
                 estimate = eff, lo = eff - z*se, hi = eff + z*se)
  }
}
df <- do.call(rbind, rows)

# Plot
p <- ggplot(df, aes(x = tau, y = estimate, color = sust_preference, group = sust_preference)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = sust_preference), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = taus) +
  labs(title = "Conditional Quantile Treatment Effect\nby Sustainability Preference",
       x = "Quantile", y = "Treatment effect",
       color = "Sustainability\nPreference", fill = "Sustainability\nPreference") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, lineheight = 1.1))

print(p)

# Save as PNG
ggsave("CQTE_by_sustpref.png", p, width = 8, height = 5.6, dpi = 300)

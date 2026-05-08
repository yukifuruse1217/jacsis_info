library(ggplot2)
library(MASS)

# if (!requireNamespace("DescTools", quietly = TRUE)) {
#   install.packages("DescTools")
# }
library(DescTools)
library(forcats)

library(readxl)
library(dplyr)
library(tidyr)

# install.packages("VGAM")
library(VGAM)

# load data ### This is just an example. Not real data!!!
load("jacsis_git_example.RData")








# Ordinal Logistic Regression

jac_table$outcome <- jac_table$count_conspi


table(jac_table$outcome)


model <- polr(outcome ~ sex, data = jac_table)
model <- polr(outcome ~ agegroup, data = jac_table)
model <- polr(outcome ~ chiiki, data = jac_table)
model <- polr(outcome ~ livesingle, data = jac_table)
model <- polr(outcome ~ u16child, data = jac_table)
model <- polr(outcome ~ parent, data = jac_table)
model <- polr(outcome ~ grandkid, data = jac_table)
model <- polr(outcome ~ grandparent, data = jac_table)
model <- polr(outcome ~ married, data = jac_table)
model <- polr(outcome ~ education, data = jac_table)
model <- polr(outcome ~ dad_edu, data = jac_table)
model <- polr(outcome ~ mom_edu, data = jac_table)
model <- polr(outcome ~ como_body, data = jac_table)
model <- polr(outcome ~ como_mental, data = jac_table)
model <- polr(outcome ~ self_covid_no, data = jac_table)
model <- polr(outcome ~ family_covid_no, data = jac_table)
model <- polr(outcome ~ info_family, data = jac_table)
model <- polr(outcome ~ info_friends, data = jac_table)
model <- polr(outcome ~ info_doctor, data = jac_table)
model <- polr(outcome ~ info_expert, data = jac_table)
model <- polr(outcome ~ info_gov, data = jac_table)
model <- polr(outcome ~ info_internet, data = jac_table)
model <- polr(outcome ~ info_paper, data = jac_table)
model <- polr(outcome ~ info_tv, data = jac_table)
model <- polr(outcome ~ income, data = jac_table)

model <- polr(outcome ~ agegroup + sex + education +
                como_body + como_mental +
                self_covid_no +
                info_family + info_friends + info_doctor + info_expert + info_gov + info_internet + info_paper + info_tv +
                income, data = jac_table)

summary(model)



### P-value calculation
# Extract coefficients
coefficients <- coef(summary(model))

# The standard errors are located in the second column of the coefficients summary
standard_errors <- coefficients[,2]

# Calculate z values
z_values <- coefficients[,1] / standard_errors

# Calculate p-values
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

result_table <- data.frame(coefficients, p_values)
result_table$OR <- exp(result_table$Value)

# show only p_values < 0.05
#sig_result_table <- result_table[result_table$p_values < 0.05,]

# Obtain confidence intervals for the model parameters
result_ci <- exp(confint(model, level = 0.95))

result_ci <- data.frame(result_ci)

if (ncol(result_ci) == 1) {
  # swap column and row
  result_ci <- t(result_ci)
}

# Add confidence intervals to the result table

result_table$ci_low <- c(result_ci[,1], rep(NA, nrow(result_table) - nrow(result_ci)))
result_table$ci_high <- c(result_ci[,2], rep(NA, nrow(result_table) - nrow(result_ci)))

result_table






















# Normal logistic regression (2 groups)
jac_table$outcome <- jac_table$willing


table(jac_table$outcome)

jac_table$outcome <- relevel(jac_table$outcome, ref = "1")




model <- glm(outcome ~ sex, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ agegroup, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ chiiki, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ livesingle, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ u16child, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ parent, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ grandkid, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ grandparent, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ married, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ education, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ dad_edu, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ mom_edu, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ como_body, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ como_mental, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ self_covid_no, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ family_covid_no, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ info_family, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ info_friends, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ info_doctor, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ info_expert, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ info_gov, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ info_internet, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ info_paper, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ info_tv, data = jac_table, family = binomial(link = "logit"))
model <- glm(outcome ~ income, data = jac_table, family = binomial(link = "logit"))

jac_table$count_conspi <- factor(jac_table$count_conspi, ordered = FALSE)
jac_table$count_conspi <- relevel(jac_table$count_conspi, ref = "0")
model <- glm(outcome ~ count_conspi, data = jac_table, family = binomial(link = "logit"))




model <- glm(outcome ~ agegroup + sex +
               education + como_body + como_mental + self_covid_no +
               info_family + info_friends + info_doctor + info_expert + info_gov + info_internet + info_paper + info_tv +
               income + count_conspi, data = jac_table, family = binomial(link = "logit"))




summary(model)







# Extract coefficients
coefficients <- coef(summary(model))
result_table <- data.frame(coefficients)

result_table$OR <- exp(result_table$Estimate)


# Calculate p-values
standard_errors <- coefficients[,2]
z_values <- coefficients[,1] / standard_errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)

result_table$p_values <- p_values


# Caluculate confidence intervals
result_ci <- exp(confint(model, level = 0.95))

result_table$ci_low <- result_ci[,1]
result_table$ci_high <- result_ci[,2]

result_table

















# Normal logistic regression (3 groups)
jac_table$outcome <- jac_table$covid_vac_rev2


table(jac_table$outcome)

jac_table$outcome <- relevel(jac_table$outcome, ref = "accept")







# Poly logistic regression
fit_vg <- vglm(outcome ~ sex, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ agegroup, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ education, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ income, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ como_body, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ como_mental, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ self_covid_no, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ info_family, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ info_friends, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ info_doctor, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ info_expert, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ info_gov, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ info_internet, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ info_paper, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P

fit_vg <- vglm(outcome ~ info_tv, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P



jac_table$count_conspi <- factor(jac_table$count_conspi, ordered = FALSE)
jac_table$count_conspi <- relevel(jac_table$count_conspi, ref = "0")

fit_vg <- vglm(outcome ~ count_conspi, family = multinomial(refLevel = "accept"), data = jac_table)
summary(fit_vg)
exp(coef(fit_vg)) # OR
confint(fit_vg) |> exp() # 95%CI
summary(fit_vg)@coef3[,4] # P











fit_vg <- vglm(outcome ~ agegroup + sex +
                 education + como_body + como_mental + self_covid_no +
                 info_family + info_friends + info_doctor + info_expert + info_gov + info_internet + info_paper + info_tv +
                 income + count_conspi, family = multinomial(refLevel = "accept"), data = jac_table)

summary(fit_vg)
temp <- exp(coef(fit_vg)) # OR
temp

temp <- confint(fit_vg) |> exp() # 95%CI
temp

temp <- summary(fit_vg)@coef3[,4] # P
temp













# Subgroup analysis
table(jac_table$count_conspi)

jac_table$count_conspi <- as.integer(jac_table$count_conspi) - 1

table(jac_table$count_conspi)

# subgroups 
jac_table <- jac_table[jac_table$count_conspi == 0,]
# OR
jac_table <- jac_table[jac_table$count_conspi >= 2,]



jac_table$outcome <- jac_table$willing


table(jac_table$outcome)

jac_table$outcome <- relevel(jac_table$outcome, ref = "1")




model <- glm(outcome ~ agegroup + sex + education + como_body + como_mental +
               self_covid_no +
               info_family + info_friends + info_doctor + info_expert + info_gov + info_internet + info_paper + info_tv + income,
             data = jac_table, family = binomial(link = "logit"))



summary(model)







# Extract coefficients
coefficients <- coef(summary(model))
result_table <- data.frame(coefficients)

result_table$OR <- exp(result_table$Estimate)


# Calculate p-values
standard_errors <- coefficients[,2]
z_values <- coefficients[,1] / standard_errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)

result_table$p_values <- p_values


# Caluculate confidence intervals
result_ci <- exp(confint(model, level = 0.95))

result_table$ci_low <- result_ci[,1]
result_table$ci_high <- result_ci[,2]

result_table




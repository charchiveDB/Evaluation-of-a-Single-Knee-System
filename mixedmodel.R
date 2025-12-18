library(rstatix)
library(tidyverse)
library(janitor)
library(plotly)
library(scico)
library(lme4)
library(Epi)
library(simr)
library(emmeans)
library(confinterpret)

stats <- read_csv('/Users/charlottehuang/Desktop/all poly data kappa score/all poly mixed .csv') %>% 
  clean_names()

View(stats)

stats <- read_csv('/Users/charlottehuang/Desktop/all poly data kappa score/metal backed mized.csv') %>% 
  clean_names()

allpoly_mixed = lmer(pain ~ days + (1 | combined_id), data = init)
summary(allpoly_mixed)





View(a)
write.csv(a, "mixed.csv")
  
metal_mixed = lmer(pain ~ days + (1 | combined_id), data = init)
summary(metal_mixed)




View(a)

View(stats)

set.seed(1234)
pain_lm = lm(pain ~ days, data=stats)

init = stats %>%
  modelr::add_predictions(pain_lm, var = 'all') %>%
  mutate(select = factor(combined_id %in% sample(1:774, 20)),
         sz = c(.5, 1)[select]) %>%
  group_by(combined_id, select) 



init$predicted_pain <- predict(pain_lm, newdata = init) 
#predicted_pain <- pmax(predicted_pain, 0)
  
nls_model <- nls(pain ~ a + b / days, data = init, start = list(a = 1, b = 1))
init$predicted_nonlinear <- predict(nls_model, newdata = init)

View(predicted_nonlinear)
filtered_data <- init %>%
  ungroup() %>% 
  mutate(row_id = row_number()) %>%
  filter(row_id <= 37) %>%
  select(-row_id) %>% 
  group_by(combined_id)
  
plot(init$days, init$pain)

class(stats)
init %>%
  plot_ly %>%
  add_lines(
    data = init,
    x =  ~ days,
    y =  ~ pain,
    size = I(.5),
    opacity = .25,
    color =  ~ select,
    size = ~ sz,
    colors = scico::scico(2, begin = .25),
    name = "Patients",
  ) %>%
  add_lines(
    x = ~ days,
    y = ~ predicted_pain,  # Plot the regression line
    line = list(color = 'blue', width = .5, dash = 'solid'),  # Customizing line style
    data = init,  # Use the full data to draw the line
    name = "Linear Regression"  # Name for the regression line in the legend
  ) %>% 
  add_lines(
    data = filtered_data,
    x =  ~ days,
    y =  ~ pain,
    size = I(.75),
    opacity = 10,
    color =  ~ select,
    name = "Sample Patients",
   # group = combined_id,
    size = ~ sz,
    colors = "red"
) %>% 
add_lines(
  x = ~ init$days,
  y = ~ predicted_nonlinear,  # Nonlinear regression line (predicted values from nls model)
  line = list(color = 'green', width = 1),  # Customizing line style
  name = "Nonlinear Regression (1/x model)"  # Name for the nonlinear regression line
) %>%
  add_lines(
    x = ~ init$days,
    y = ~ init$predicted_polynomial,  # Polynomial regression line (predicted values from polynomial model)
    line = list(color = 'purple', width = 1),
    name = "Polynomial Regression (degree 2)"
  )

summary(nls_model)
View(init)




# Check if any 'days' values are zero or negative
sum(init$days <= 0)

# Remove rows where 'days' is zero or negative
init <- init[init$days > 0, ]
init <- init[init$pain > 0, ]

plot(init$days, init$pain, main = "Pain vs Days", xlab = "Days", ylab = "Pain")

polynomial_model <- nls(pain ~ a * days^2 + b * days + c, 
                        data = init, 
                        start = list(a = 1, b = 1, c = 1))


init$predicted_polynomial <- predict(polynomial_model, newdata = init)


length(init$days)      # Check size of 'days'
length(init$pain)      # Check size of 'pain'
length(init$predicted_pain)  # Check size of 'predicted_pain'
length(init$predicted_nonlinear)   # Check size of 'predicted_nl'


new_stats <- read_csv('/Users/charlottehuang/Desktop/all poly data kappa score/04-09-2025 CI.csv') %>% 
  clean_names()


View(new_stats)
lin_pain <- lmer(pain ~ days + ap_vs_mb + (1 | combined_id), data = new_stats)
summary(lin_pain)
View(lin_pain)
ggplot()
ci.lin(lin_pain)

# the null hypothesis is not rejected because confidence contains 0 for coefficient of treatment 



ci_test <- matrix(c(-0.05, 0.05),
                  nrow = 1, dimnames = list("estimate",
                                            c("2.5 %", "97.5 %")))
interpret_noninferiority(ci_test, 0, 0.1, c("Treatment as usual",
                                            "New treatment"))
interpret_superiority(ci, null_value = 0, groups = c("Control intervention",
                                                     "Test intervention"), beneficial_outcome = TRUE)


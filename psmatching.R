library(tidyverse)
install.packages("sandwich")
library(MatchIt)
library(lmtest)
library(sandwich)

score <- read_csv('/Users/charlottehuang/Desktop/all poly data kappa score/all poly.csv') %>% 
  clean_names()
View(score)
score <- score %>% 
  mutate(id = seq(1:647)) %>% 
  select(id, everything())
 
score <- score %>%   
  mutate(smoker = case_when(score$current_smoker == "former smoker" ~ 1,
            score$current_smoker == "never smoker"  ~ 0,
            score$current_smoker == "current smoker"  ~ 2))
 
score$m_f <- as.numeric(score$m_f == "Male")
score$all_poly_vs_metal_backed <- as.numeric(score$all_poly_vs_metal_backed == "All-poly")
score$age <- as.numeric(score$age)
score$bmi <- as.numeric(score$bmi)

match_obj <- matchit(score$all_poly_vs_metal_backed ~ score$age + score$current_smoker + score$bmi + score$m_f,
                     data = score, method = "nearest", distance ="bart",
                     ratio = 1,
                     replace = FALSE,
                     exact = ~ m_f)
View(match_obj$model)
summary(match_obj)
plot.matchit(match_obj)
data <- match_data(match_obj)

View(data)

write.csv(data, "psdata.csv")

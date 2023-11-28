library(tidyverse)
library(ggeffects)
library(performance)
library(here)
library(broom)
theme_set(theme_light())

### data set-up ####
d <- haven::read_dta(here("Demos","ess_bigger_imp.dta"))

ggplot(d,
       aes(x = stflife)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Life Satisfaction",
       y = "# of Respondents",
       title = "Life Satisfaction in 29 European Countries",
       subtitle = "2014 European Social Survey")

gb <- d |>
  filter(cntry == "GB") |> 
  select(-starts_with("c_"),
         -cntry,
         -country,
         -gnipc1000) |> 
  haven::zap_labels()

glimpse(gb)

ggplot(gb,
       aes(x = stflife)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Life Satisfaction",
       y = "# of Respondents",
       title = "Life Satisfaction in Great Britain",
       subtitle = "2014 European Social Survey")

### linear regression null model ####
m0 <- lm(stflife ~ 1, data = gb) # gets us average value of life satisfaction--no predictors
tidy(m0) # average is 7.27, standard error of the mean is 0.0475

# to get 95% CI, do 7.27 + (2*.0475) and 7.27 - (2*0.0475)

# lm(y~1) is same as doing glm(y~1, family = gaussian)

m0 <- glm(stflife ~ 1, data = gb,
          family="gaussian")
tidy(m0)

# to get standard deviation (sigma) of the null model:
sigma(m0)

### simple linear regression with 2 groups (AKA T-tests!) ####
my_fake_ttest <- glm(stflife ~ female,
                     family = gaussian,
                     data = gb)
tidy(my_fake_ttest)
# t statistic = 0.671, p = 0.502: no significant difference between sexes


### simple linear regression regression with >2 groups (AKA ANOVA!) ####
my_fake_anova <- lm(stflife ~ factor(region),
                     data = gb)
tidy(my_fake_anova)
# null is that all betas = 0 (no significant differences among regions)

anova(my_fake_anova)
# F-stat is joint test of whether all those betas are equal to zero
# this F-stat = 0.857, p = 0.5826. No difference among regions


### simple linear regression regression with numeric predictor ####
m1 <- glm(stflife ~ inc,
          family = gaussian,
          data = gb)

ggplot(gb, aes(x=inc, y=stflife))+
  geom_smooth(method="lm")+
  geom_jitter(alpha=.1)

tidy(m1, conf.int = TRUE)

# rescale income variable so that it goes between 0 and 1
gb <- gb |> 
  mutate(inc01 = (inc-1)/9)

m1_rescaled <- glm(stflife ~ inc,
                   family = gaussian,
                   data = gb)
tidy(m1_rescaled, conf.int = TRUE)



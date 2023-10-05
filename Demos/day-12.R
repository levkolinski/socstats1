library(tidyverse)
library(janitor)
library(infer)
theme_set(theme_light())

# data prep
library(gssr)
gss18<-gss_get_yr(2018) |> 
  haven::zap_labels()

# difference in church attendance by sex
d <- gss18 |> 
  mutate(female = if_else(sex == 2, 1L, 0L),
         weekly = if_else(attend >= 7, 1L, 0L)) |> 
  select(female, weekly) |> 
  drop_na() |> 
  mutate(sex = if_else(female == 1, "Female", "Male"))

# cross-tab using tabyl() function in 'janitor' package
d |> 
  tabyl(female, weekly) |> 
  adorn_percentages("row") |>  # gives row percentages
  adorn_pct_formatting(digits = 2) |> # percentages with 2 decimal points
  adorn_ns() # gives counts in parens next to percents

# graphing male vs female church attendance percentages
d |> 
  group_by(sex) |> 
  summarize(percent = mean(weekly)*100) |> 
  ggplot(aes(x=sex,
             y=percent,
             fill = sex))+
  geom_col() +
  coord_flip() # makes bar plot horizontal

# differences in proportions
# confidence intervals
set.seed(12345)
boot_dist <- d |> 
  specify(weekly ~ sex) |>  # weekly is response variable, sex is input variable
  generate(reps = 1000, 
           type = "bootstrap") |>  #1000 bootstrap replications of the data (resamples of the dataset)
  calculate(stat = "diff in means",
            order = c("Female", "Male")) # tells us mean differences is female minus male

ci <- boot_dist |> 
  get_confidence_interval(level = 0.95) # looks for the stat column
ci # about 1% and 7%

boot_dist |> 
  visualize() + # not pipe, but + because we're adding a geom layer
  shade_ci(ci)

## hypothesis test
# new concept: permutation--breaks the independence between the columns
obs_diff <- mean(d$weekly[d$sex=="Female"])-mean(d$weekly[d$sex=="Male"])

null_dist <- d |> 
  specify(weekly ~ sex) |> 
  hypothesize("independence") |> 
  generate(reps=1000,type="permute") |> 
  calculate(stat="diff in means",
            order=c("Female","Male"))

# get p-value
null_dist |> 
 get_p_value(obs_diff,direction = "both")

# visualize null distribution with p-value
null_dist |> 
  visualize() +
  shade_p_value(obs_stat = obs_diff,direction = "both")


### CHI-SQUARE ###
# need both variables to be factors
d <- d |> 
  mutate(church = if_else(weekly == 1, "Weekly", "Less Often"))

# expected values under independence --marginal values (row and column totals) don't
d |> 
  specify(church ~ sex,
          success = "Weekly")|> 
  hypothesize(null = "independence") |> 
  generate(reps = 1,
           type = "permute") |> 
  tabyl(sex, church) |> 
  adorn_totals(c("row", "col"))

# calculate chi-square (sum of (observed - expected)^2/expected for all cells)
obs_chi_square <- d |> 
  specify(church ~ sex,
          success = "Weekly")|> 
  hypothesize(null = "independence") |> 
  calculate(stat = "Chisq")
obs_chi_square       

# hypothesis test
null_dist <- d |> 
  specify(church ~ sex,
          success="Weekly") |> 
  hypothesize(null = "independence") |> 
  generate(reps = 1000,
           type = "permute") |> 
  calculate(stat = "Chisq")

# get p value
null_dist |> 
  get_p_value(obs_chi_square,
              direction = "greater")

null_dist |> 
  visualise()+
  shade_p_value(obs_chi_square,
                direction = "greater")
         
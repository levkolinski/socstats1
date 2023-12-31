# library(remotes)
# remotes::install_github("kjhealy/gssr")

library(tidyverse)
library(gssr)
theme_set(theme_minimal())

# What proportion of American adults idenitfies as conservative?
gss18<-gss_get_yr(2018)

ggplot(gss18,
       aes(x = polviews)) +
  geom_bar()

prop_polviews <- gss18 |> 
  select(polviews) |> 
  drop_na() |> 
  group_by(polviews) |> 
  summarize(count=n()) |> 
  mutate(prop = count/sum(count))
prop_polviews

ggplot(prop_polviews, aes(x=polviews,y=prop))+geom_col()

gss_polviews <- gss18 |> 
  select(polviews) |> 
  drop_na() |> 
  mutate(conservative = if_else(polviews>4,1L,0L))

est_prop <- mean(gss_polviews$conservative)
est_prop

# how confident should we be in this estimate (est_prop of conservatives=0.3275)?
# let's do a simulation

# set up simulation "dresser"
num_sims <- 1e4 #how many simulations
svy_size <- nrow(gss_polviews) # number of people in each poll

sims <- tibble(sim_num = 1:num_sims) |> 
  uncount(svy_size) #dupicates rows according to political views

# do the simulations
sims <- sims |> 
  mutate(conservative = rbinom(num_sims*svy_size, 1, est_prop)) |> 
  group_by(sim_num) |> 
  summarize(prop = mean(conservative))

# visualize the sim data (sampling distribution)
ggplot(sims, aes(x=prop))+
  geom_histogram(boundary = round(est_prop,2),
                 bindwidth=0.005,
                 color="white")+
  scale_x_continuous(breaks = seq(.3,.4,.01))

# takehome point: even if we know exaclty what the population parameter is,
# we still expereience variability in the realized samples

# let's characterize the simulation results
mean(sims$prop)
sd(sims$prop)

# since we don't know the true p, what can we say about it?
sample_from_gss <- sample_n(gss_polviews,
                            size=nrow(gss_polviews),
                            replace=TRUE)



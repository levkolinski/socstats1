# Frequentest approach to probability: long-run probability
# ex. If we do this enough times, how many times will something happen?


# Bayesian approach to probability: subjective probability
# ex. Biden has a 55% chance of winning the election--
# doesn't mean that if we did the eleciton 100 times, Biden would win 55 times=
# Bayesian is more skepitcal of long-run probability, good for events that are not repeatable

# We can use long-run thinking to end up at subjective probability

# loading packages----
library(tidyverse)
library(ggplot2)

# making a coin flip table inefficiently ----
coins <- tibble(
  coin1 = c(1, 0, 1, 0),
  coin2 = c(1, 0, 0, 1)
)

# adding a num_heads column
coins <- coins |> 
  mutate(num_heads = coin1 + coin2)
coins

# sampling using rbinom()----
#rbinom(n, size, probability)
rbinom(10, 1, .5)
#^ this is to model flipping 1 coin 10 times, with 50% probability of being heads

# create a mock population of Durham
durham<-tibble(
  female = rbinom(285000, 1, .52)
)

# take sample of durham
durham_sample <- durham |> 
  slice_sample(replace=FALSE, n = 100) #take sample of 100 people from durham without replacement

# if replace=TRUE, we put the item back in the pot after we sample to be possibly fished out again
# if replace=FALSE, we don't put the item back in the pot after sampling, so they can't be sampled again
# generally in stats, we sample WITH replacement (replace=TRUE)

durham_sample |> 
  group_by(female) |> 
  summarize(count=n())

durham |>  #using a full sample of durham = a census, will give you the same answer every time
  group_by(female) |> 
  summarize(count=n())


# simulations ----
# sampling from an infinite population that's 52% female

one_durham_sample <- tibble(
  female = rbinom(100, 1, .52)
)
set.seed(1234) #rigging sampling so it matches with class
sims<-tibble(
  sim_num = 1:300, # is 300 simulations
  females = rbinom(300, 100, .52) # take 300 samples from 100 people from Durham, how many came up 1
)

ggplot(data = sims,
       mapping = aes(x = females)) +
  geom_bar()





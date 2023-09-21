library(tidyverse)
theme_set(theme_light())
library(ggplot2)
# law of large numbers----
lln_sim <- tibble(
  n = 2^(3:14),
  est_prop = rbinom(12, n, .33)/n # true value is .33
)


ggplot(lln_sim,
       aes(x=n,
           y=est_prop))+
  geom_line(alpha = 0.8) +
  geom_hline(yintercept=0.33,
             linetype="dashed",
             alpha=0.2)

# as the sample size gets larger, 
#the est_prop approaches closer ot the true value (.33)


# now let's see how confident we should be in our estimate of proprotion
# conservative responsidents of GSS (.33)

# set up simulation "dresser"
set.seed(123)
est_prop <- .33
num_sims<-10000
svy_size <- 2247

sims <- tibble(sim_num=1:num_sims) |> 
  uncount(svy_size)

# do the sims
sims <- sims |> 
  mutate(conservative = rbinom(num_sims*svy_size, 1, est_prop)) |> 
  group_by(sim_num) |> 
  summarize(prop = mean(conservative))

# visualize results 
ggplot(sims,
       aes(x=prop))+
  geom_histogram(color="white",
                 boundary=round(est_prop,2),
                 bindwidth=.005)+
  scale_x_continuous(breaks=seq(.3,.4,.01))


# different visualization--denisty plot
ggplot(sims,
       aes(x=prop))+
  geom_density(color=NA,
               fill="blue",
               alpha=.2)+
  scale_x_continuous(breaks=seq(.3,.4,.01))

# let's characterize the simulation results
mean(sims$prop)
sd(sims$prop)

# confidence interval--boundaries of an integral that bounds (usually) a % of the estimates

#95%
lower_bound95<-quantile(sims$prop,.025)
upper_bound95<-quantile(sims$prop,.975)

ci95<-c(lower_bound95,upper_bound95)
ci95

# so 95% of values fall within 0.311 and .349

# 99%
lower_bound99<-quantile(sims$prop,.005)
upper_bound99<-quantile(sims$prop,.995)

ci99<-c(lower_bound99,upper_bound99)
ci99
# so 99% of values fall within 0.305 and .356


# central limit theorem--with greater sample, sample becomes normal----

# football field simulation:

# 50% chance of either moving forward or back 1 yard-line
est_prop <- .5
num_sims<-50 # 50 rounds
svy_size <- 100 #100 people

# set up dresser
sims <- tibble(sim_num=1:num_sims) |> 
  uncount(svy_size)

# do the simulation of football field
sims <- sims |> 
  mutate(movement = sample(c(-1L,1L),  
                       size = num_sims*svy_size,  
                       prob = c(.5, .5),    
                       replace = TRUE)) 

# summarize football field results
results <- sims |> 
  group_by(sim_num) |> 
  summarize(location=sum(movement)+50)

# plot results
ggplot(results,
       aes(x = location)) +
  geom_bar(color="white")+
  xlim(0,100)

# standard error is the standard deviation of the sampling distribution
# SD=sqrt(prop(1-prop))
# SD is a measure of entropy--how uncertain is the system. If proportion is 50-50,
# system is very uncertain, and SD is high (.5)

# Standard erorr is SD as related to the sample size
# SE = SD/sqrt(n)--as sample size increases, SE decreases
# to halve SE, you have to increase sample size by factor of 4







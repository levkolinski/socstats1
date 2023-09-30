library(tidyverse)
theme_set(theme_light())

npeople<-1000
nflips<-50

foot_sims<-
  expand_grid(person_id=1:npeople,
              flip_id = 1:nflips) |> 
  mutate(flip=sample(c(-1,1),
                     replace=TRUE,
                     size=npeople*nflips,
                     prob=c(.5,.5))) |> 
  group_by(person_id) |> 
  summarize(location = sum(flip) + 50)

ggplot(data=foot_sims, aes(x=location))+geom_bar()

# going to infinity using dbinom() function

# what we expect to happen
bin_probs <- tibble(
  heads=0:50,
  probability = dbinom(heads, 50, .5)
)

ggplot(bin_probs,aes(x=heads,y=probability))+geom_line()


hyp_33<-dbinom(737,2247,.33)
hyp_32<-dbinom(737,2247,.32)

hyp_33
hyp_32

hyp_33 / hyp_32 #factor MORE LIKELY to be the correct value--it is 1.36X more likely that 33% of americans are 
# conservative than that 32% of americans are conservative


hyp_328 <- dbinom(737, 2247, .328)
hyp_327 <- dbinom(737, 2247, .327)

hyp_328/hyp_327 # it is 1.005043X more likely that 32.8% of americans are conservative
# than 32.7% of americans are conservative

# probability is the probability of some data given a model (Prob = Pr(Data | Model))
# likelihood is the probability of a model given data (likelihood = Pr(Model |Data))


# plotting likelihood of seeing 737 conservatives out of 2247
ggplot() +
  xlim(0,1)+
  geom_function(fun=\(prob) dbinom(x = 737, 
                                size = 2247,
                                prob=prob))


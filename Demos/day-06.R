library(tidyverse)
library(ggplot2)

theme_set(theme_minimal())

# set up simulation
num_sims <- 500 # number of polls
poll_size <- 300 # number of people in each poll

# create skeleton to "accept" generated votes
sims <- tibble(sim_num = 1:num_sims) |> 
  uncount(poll_size) #uncount() duplicates rows
View(sims)
#inspect simulation "skeleton"
sims

# draw the samples
sims <- sims |> 
  mutate(vote = sample(c("Dem","Rep"),  #will sample from either Dem or Rep
         size = num_sims*poll_size,   #will generate 150,000 votes
         prob = c(0.52, 0.48),    # 0.52 prob dem, 0.48 prob rep
         replace = TRUE)) # need to put Dem and Rep back in the bag after you fish it out
         
# summarize the results
results <- sims |> 
  mutate(dem_vote = if_else(vote=="Dem", 1L, 0L)) |>  #assigning democrat the 1 integer
  group_by(sim_num) |> 
  summarize(dem_prop=mean(dem_vote))

# plot the results (univariate because only looking at one variable)
ggplot(results,
       aes(x = dem_prop)) +
  geom_histogram(color="white",
                 boundary = 0.5,
                 binwidth=0.1)


# how often does the poll predict the winner
results<-results |> 
  mutate(dem_win = if_else(dem_prop>0.5,1L,0L))
results |> 
  summarise(prop_correct=mean(dem_win))


# box plots
ggplot(results,
       aes(x = dem_prop))+
  geom_boxplot()

# strip plot
ggplot(results,
       aes(x = dem_prop,y = ""))+
  geom_jitter(alpha = 0.2) #alpha makes the points transparent
  
# doing jitter plot on top of box plot
ggplot(results,
       aes(x = dem_prop, y=""))+
  geom_boxplot(outlier.color=NA)+
  geom_jitter(alpha = 0.2)
  



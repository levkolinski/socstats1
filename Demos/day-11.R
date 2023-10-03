### resampling, CIs, and hypothesis testing

library(tidyverse,warn.conflicts = FALSE)
library(ggplot2)

# IDEA: rresampling from your data is a way to simulate the sampling distribution

# pretend the "original" dataa (n=75)
d <- tibble(
  republican=c(rep(0,45),   # Dem voter (rep() means repeat throughout)
               rep(1,30))   # Republican voter
)

# point estimate ("maximum likelihood")
mean(d$republican)   # 40% of original sample is republican

# resampling function (same as bootstrapping)
resample <- function(x) sample(x,              # sample from this input
                               length(x),       # the same size as the original
                               replace=TRUE)   # with replacement
# example using the resample() function
resample(c(1,2,3,4,5,6,7,8,9,0))

# resamples frame
resamples <- tibble(sample = 1:10000) |> #take 100000 resamples from dataset
  rowwise() |> 
  mutate(data = list(resample(d$republican)), #the list function allows us to place the 75-number sample into a single cell in the table
         est_prop=mean(data))

# plot the CI
ggplot(resamples,
       aes(x=est_prop))+
  geom_histogram(boundary = 0.5,
                 binwidth = 0.025,
                 color="white")+
  scale_x_continuous(breaks = seq(0,1,.05),
                     limits=c(0,1))+
  geom_vline(xintercept=quantile(resamples$est_prop, c(0.025,.975)),
             color="firebrick", 
             linetype="dashed")

# hypothesis test
# null hypothesis (H0) is that population is evenly split between reps and dems
# alternative hypothesis (Ha) is that proportion of reps is <.5
# alpha level (e.g., .05): the complement of the CI width, probability of a false positive
# p-value: what is the probability of getting a result "at least as extreme"

nulls <- tibble(sample = 1:10000) |> 
  rowwise() |> 
  mutate(nulldata = list(rbinom(75,1,.5)),
         null_prop=mean(nulldata))


#visualize the null data
ggplot(nulls,
       aes(x=null_prop))+
  geom_histogram(boundary=0.5,
                 binwidth=0.025,
                 color="white")+
  scale_x_continuous(breaks = seq(0,1,.05),
                     limits=c(0,1))+
  geom_vline(xintercept=mean(d$republican), # one-tail p-value
             color="firebrick", 
             linetype="dashed")

# how many times do we get a result as low as the real data if the null is true?
pvalue <- nrow(filter(nulls,null_prop<=mean(d$republican)))/nrow(nulls)
print(pvalue)
# this is a one-tailed p-value
# so, even if pop is 50:50, about 5% of the time, we are still going to get a value of ~40% or below

# to get a two-sided p-value:
pvalue*2
# So 2-sided p-value is .1048, we cannot reject null hypothesis

### Extending to two samples ###

#preparing data--do men and women differ in conservative leanings?
library(gssr)
gss18<-gss_get_yr(2018) |> 
  haven::zap_labels()

d <- gss18 |> 
  mutate(female=ifelse(sex==2,1L,0L),
         conservative = if_else(polviews > 4, 1L, 0L)) |> 
  select(female, conservative) |> 
  drop_na()

# group proportions to get descriptive statistics
d |> 
  group_by(female) |> 
  summarize(proportion_conservative = mean(conservative),
            sample_size=n())

# get CI of the difference

# define the function for resampling
resample_diff <- function() {
  # get male proportion conservative
  pm<-mean(sample(d$conservative[d$female==0],
                  size = length(d$conservative[d$female==0]),
                  replace=TRUE))
  # get female proportion conservative
  pf <-mean(sample(d$conservative[d$female==1],
                   size = length(d$conservative[d$female==1]),
                   replace=TRUE))
  # return the difference--how much more conservative are men than women?
  return(pm-pf)
}

# resample 10,000 times
resamples <- tibble(sample=1:10000) |> 
  rowwise() |> 
  mutate(diff=resample_diff())

# plot the sampling distribution of the differences between males and females
ggplot(resamples,
       aes(x=diff))+
  geom_histogram(color="white",
                 boundary=0.5,
                 binwidth=0.01)+
  geom_vline(xintercept=quantile(resamples$diff, c(0.025,.975)),
             color="firebrick",
             linetype="dashed")
# we can see that the 95% CI overlaps with 0....so we can't reject the null hypothesis that there 
# is no difference between men and women....confirmed when we geneterate the 2.5% and 97.5% quantiles
quantile(resamples$diff, c(0.025,.975))

# official hypothesis testing
observed_proportion <- mean(d$conservative)
n_males<-nrow(filter(d, female==0))
n_females<-nrow(filter(d, female==1))

# the null distribution (i.e. where the difference is really zero)
null_diff<-function() {
  mean(rbinom(n_males,1,observed_proportion)) - 
    mean(rbinom(n_females,1, observed_proportion))
}

nulls <- tibble(sample = 1:10000) |> 
  rowwise() |> 
  mutate(diff = null_diff())

# getting the observed difference for reference
obs_diff <-
  mean(d$conservative[d$female==0])-mean(d$conservative[d$female==1])

# plotting null distribution
ggplot(nulls,
       aes(x=diff))+
  geom_histogram(color="white",
                 boundary=0.5,
                 binwidth=0.005)+
  geom_vline(xintercept=obs_diff,
             color="firebrick",
             linetype="dashed")

# p-value
pvalue<-
  nrow(filter(nulls, diff>=obs_diff)) /
  nrow(nulls)

# 2-tailed p-value:
pvalue*2

# cannot reject null



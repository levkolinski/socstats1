library(ggplot2)
library(tidyverse)

plot <- ggplot() +
  xlim(c(0,1)) +
  geom_function(fun = \(prob) dbinom(x =7,
                                     size = 22,
                                     prob = prob),
                n=1001)
plot

# simulation confidence interval
sims <- tibble(success = rbinom(1e6, size=22, prob=7/22),
               est_prob = success/22)
sims |> 
  summarize(lb = quantile(est_prob, .025),
            ub = quantile(est_prob, .975))

# success
qbinom(p=c(0.025,0.975),
       size = 22,
       prob=7/22)

#so you can see the distribution
ggplot(sims,
       aes(x=success))+geom_bar()

# save CI as a vector
ci <- qbinom(p = c(0.025, 0.975),
             size=22,
             prob=7/22)/22

# plot distribution with CI included
plot + geom_vline(xintercept=ci,linetype="dashed")

# calculate with CLT
# what happens when we replace the 7 with larger or smaller numbers?
p_hat <-7/22
sd <- sqrt(p_hat*(1-p_hat))
se <- sd/sqrt(22)

ub <- p_hat + 1.96*se
lb <- p_hat - 1.96*se

ci_clt<-c(lb, ub)
ci_clt

# likelihood based grid approximation
# reminder: what is an integral? Why does it matter here?
likelihoods <- tibble(
  prob = seq(from = 0.001,
             to = 0.999,
             by = 0.001)
) |> 
  mutate(likelihood = dbinom(7,22,prob),
         cumulative_likelihood = cumsum(11)/sum(11))

# get the CI
ll_ci<-likelihoods |> 
  filter(cumulative_likelihood >= 0.025 & 
           cumulative_likelihood <= .975) |> 
  summarize(lb = min(prob),
            ub = max(prob)) |> 
  as.numeric()

ll_ci



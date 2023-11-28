library(bayesrules)
library(broom)
library(tidyverse)
library(ggeffects)
data(bikes, package="bayesrules")

glimpse(bikes)

# null model--just get the mean ridership, no predictors
m0 <- lm(rides ~ 1,  
         data = bikes)
tidy(m0)

m1 <- lm(rides ~ temp_feel,
         data = bikes)
tidy(m1, conf.int = TRUE) 
#conf.int gives us a 95% CI, for each degree F that temp increases, we expect ridership to go up somewhere
# between 71.8 and 91.9


# intercept is -2179--that isn't possible! there is never a day in the dataset with 0 degree temp_feel,
# and it is not possible to have negative riders

# visualizing data
ggplot(bikes,
        aes(x = temp_feel,
            y = rides)) +
  geom_smooth(method = "lm")+
  geom_point(alpha = 0.5)


m2 <- lm(rides ~ temp_feel + I(temp_feel^2),  # wrapping in a capital I() 
         data = bikes)

tidy(m2)

ggpredict(m2, terms = "temp_feel") |> 
  plot()



# Among the models we're considering, which is the most appropriate (Model Selection)

# For a given model, what are the best estimates of its parameters (point estimation)

# Given that the best paramester estimates from teh data do not equal those from the population,
# what range of parameter values is reasonable? (interval estimation

# Does a given model adequately represent the phenomenon under study? (goodness of fit)


# What is probability? (Pr(data | parameter))
# assuming that each incoming student has a probability equal to 0.5 of being female, how probable 
# is a cohort of 6 out of 21 members being female

# What is likelihood? (Pr(parameter | data))
# Knowing that 5 out of 21 members of this year's cohort were female, how likely is it that 
# the true probability that a new student #will be a female is equal to 0.5

# Imagine two types of departments: one taht is more likely to admit US citizens, the other is not
# They are NOT labeled. Figure out which department is which

# Department A admits 33 of 245 US applicants and 4 of 97 non-US applicants. Which type of department
# do you think this is?

# Model: logit(admission)=alpha*beta*x
# if the value of Beta is actually 0, the department is not biased. Let's compare models!
# L(beta = 0 | data) = 1.253E-51
# take the log likelihood to be easier to work with: ln(1.253E-51) = -117.206

# L(beta = 1 | ata)


alpha<-exp(-3.4972)/(1+exp(-3.4972))
beta1<-exp(1.8222)/(exp(-3.4972)+exp(1.8222))
beta2<-exp(1.5019)/(1+exp(1.5019)+exp(-3.4972)+exp(1.8222))

library(tidyverse, warn.conflicts=FALSE)
library(janitor)

d <- tribble(
  ~gre, ~letters, ~admitted, ~n,
  0, 0, 0
  0, 0, 1
  0, 1, 0
  0, 0,
  1, 0,
  1, 0,
  1, 1,
  1, 1
)

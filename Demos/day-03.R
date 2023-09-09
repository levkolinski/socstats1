library(tidyverse)
data(mtcars)

mean(mtcars$mpg)

mtcars$mpg |> 
  mean()

mtcars |> 
  filter(am==1) |> 
  pull(mpg) |> 
  mean()

library(palmerpenguins)
library(openintro)
data(bac)
View(bac)
library(ggplot2)

lm<-lm(bac~beers, data=bac)
ggplot(aes(x=beers, y=bac), data=bac)+geom_point()+ stat_smooth(method = "lm",
                                                                formula = y ~ x,
                                                                geom = "smooth")

library(haven)
gss2022<-read_dta("Data/GSS2022.dta") |> 
  zap_labels()




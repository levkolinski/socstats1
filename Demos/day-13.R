# data prep
library(gssr)
library(tidyverse)
library(janitor)
gss18<- gss_get_yr(2018) |> 
  haven::zap_labels()

d1<- gss18 |> 
  select(degree, natenvir) |> 
  drop_na()

# marginal probabilities (univariate probabilities--e.g., Pr(natevnir = 1))
tabyl(d1, degree) #education level
tabyl(d1, natenvir) # 1 = not spending enough on environment, 3 = spending too much


## joint probability--probabilities of rows and columns combined, whole grid sums to 100
# e.g., Pr(natenevir = 1 & degree = 0)
tabyl(d1, degree, natenvir) |> 
  adorn_percentages(denominator = "all") |> 
  adorn_pct_formatting(digits = 1)

# rowwise conditional probabilities--each row sums to 100, only looking at one row at a time
# e.g., Pr(natenvir=1 | degree = 0)    | means if
tabyl(d1, degree, natenvir) |> 
  adorn_percentages(denominator = "row") |> 
  adorn_pct_formatting(digits=0)

# columnwise conditional probabilities --each column sums to 100
# condition given that we are spending too little on the environment, what is the prob of having a grad degree?
tabyl(d1, degree, natenvir) |> 
  adorn_percentages(denominator = "col") |> 
  adorn_pct_formatting(digits=0)

# chi2 on education vs. thoughts on spending on environment
x2<-tabyl(d1, degree, natenvir) |> 
  chisq.test()
x2
# we see that p<0.05--these education and perceptions of environental spending are not independent

# to see observed values
x2$observed
#...expected
x2$expected # if independent


## Four ways of quantifying dependence
# data prep
d2 <- gss18 |> 
  select(sex, xmarsex) |>  #xmarsex = is extramarital sex okay? 
  drop_na() |> 
  mutate(female = if_else(sex==2, 1L, 0L),
         sex = if_else(female == 1, "Female", "Male"),
         xmarsex = if_else(xmarsex == 2, 1L, 0L),
         xmarsex_chr = if_else(xmarsex ==1, "Yes", "No")) |> 
  relocate(sex, female, xmarsex_chr, xmarsex)
d2

# tables
tabyl(d2, sex)
tabyl(d2, xmarsex_chr)

#cross tab
tabyl(d2, sex, xmarsex_chr)

# joint probability
tabyl(d2, sex, xmarsex_chr) |> 
  adorn_percentages(denominator = "all") |> 
  adorn_pct_formatting(digits = 0)

# conditional probabilities 
tabyl(d2, sex, xmarsex_chr) |> 
  adorn_percentages(denominator = "row") |>  #(if a given sex, what are their thoughts)
  adorn_pct_formatting(digits = 0) 

tabyl(d2, sex, xmarsex_chr) |> 
  adorn_percentages(denominator = "col") |>  #(if a given thought, what is their sex)
  adorn_pct_formatting(digits = 0) 



# Practice
#Probability ratio:
male_prob<-461/687 # male okay
fem_prob<-489/621 # female okay

difference_prob<-male_prob-fem_prob
difference_prob

risk_ratio<-male_prob/fem_prob
risk_ratio

male_odds<-461/226
fem_odds<-489/132

OR<-male_odds/fem_odds
OR

log_odds_ratio<-log(OR)
log_odds_ratio

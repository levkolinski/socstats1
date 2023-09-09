# loading packages
library(tidyverse)
library(nycflights13)

# loading data
data(flights)
glimpse(flights)
# filter() pulls out specific rows----
flights_EWR<- flights |> 
  filter(origin=="EWR")

# this is the same as:
flights[flights$origin=="EWR",]

# select() allows us to pull specific columns----
flights_selected<-flights |> 
  select(origin,dest,carrier,dep_time:arr_time) #can do minus sign to remove object

# mutate() allows us to make new variables (columns) in a dataframe----
# ex. making new column "late" saying "yes or no" flight was delayed
flights <- flights |> 
  mutate(late=if_else(dep_delay>0,1L,0L))

# summarize() summarizes the data frame into just one value or vector----
flights |> 
  summarize(count=n()) # "count" is just the name of the column...could be called anything

# group_by()----
flights |> 
  group_by(origin) |>  # breaks up dataset by variable level for purposes of calculations in pipe
  summarize(count=n())

# arrange() allows us to sort columns in order--wrap object in desc() if you want order descending----


# examples----
# which airlines leaving from the NYC area are late most frequently?
flights |> 
  drop_na() |> 
  group_by(carrier) |> 
  summarize(prop_late=mean(late)) |> 
  arrange(desc(prop_late))


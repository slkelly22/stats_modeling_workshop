# Stats Modeling - Week 1
# Fall 2025

library(car)
library(tidyverse)

students <- read_csv("data/cassady_finch_bolin.csv")

## Where is the GPA 302 person? 
## Not in the model b/c their CTA.tot is missing
students |> 
  select(Gender:GPA, BStotal:CTA.tot) |> 
  filter(GPA > 4) 

# yup, you can see them in the BStotal plot
ggplot(students, aes(x = BStotal, y = GPA)) + geom_point() + 
  geom_smooth(method = "lm")

ggplot(students, aes(x = BStotal, y = GPA)) + geom_point() + 
  geom_smooth(method = "lm") + ylim(0,4)


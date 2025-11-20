library(tidyverse)
library(usdata)
library(corrplot)

county_numeric <- county |> 
  select_if(is.numeric)

factors <- county |> 
  select_if(is.factor) # state, metro, median_edu, smoking_ban

corrplot(cor(county_numeric, use = "complete.obs"), method = "pie", 
         type = "upper")

modelA <- lm(unemployment_rate ~ homeownership + median_edu + homeownership:median_edu, data = county)
summary(modelA) # that's a significant interaction

library(interactions)

interact_plot(modelA, pred = homeownership, modx = median_edu, jnplot = TRUE)

modelB <- lm(unemployment_rate ~ metro + median_edu + metro:median_edu, data = county)
summary(modelB) # why singularity on one level? 
cat_plot(modelB, pred = median_edu, modx = metro, jnplot = TRUE)

table(county$metro, county$median_edu) # why singularity? B/c only 2 observations
county |> 
  filter(median_edu == "below_hs") # only two observations so should drop

dim(county)

nohs_county <- county |> 
  filter(median_edu != "below_hs")
dim(nohs_county)

nohs_county$median_edu <- levels(nohs_county)

fct_count(county$median_edu)

test <- fct_collapse(county$median_edu, high_school = c("hs_diploma", "below_hs"))
fct_count(test)

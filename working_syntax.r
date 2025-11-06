str(students)
unique(students$Male)
levels(students$Male)

 
students$gender_fac <- factor(students$Gender, levels = c(0, 1), labels = c("Female", "Male")) # Nope: you can't turn a 1/2 numeric variable into a 0/1 factor. If you try that it will only turn the 1s and then NA everything else.
levels(students$gender_fac)
count()


# let me try something
students$gender_sk <- factor(students$Gender, levels = c(1, 2), labels = c("Female", "Male"))
levels(students$gender_sk)

# modeling the variable as is
model_gpa_anxiety_gender <- lm(GPA ~ CTA.tot + BStotal + Gender, data = students)
summary(model_gpa_anxiety_gender)

# modeling the variable as a factor (with 1/2, not 0/1)
model_gpa_anxiety_gender2 <- lm(GPA ~ CTA.tot + BStotal + gender_sk, data = students)
summary(model_gpa_anxiety_gender2)

# now, what does it look like if we 0/1 the variable? 
students <- students |> 
  mutate(factor_test =
           case_when(gender_sk == "Female" ~ 0, 
                     gender_sk == "Male" ~ 1))

# modeling the variable as a factor (now with 0/1....any difference)
model_gpa_anxiety_gender3 <- lm(GPA ~ CTA.tot + BStotal + factor_test, data = students)
summary(model_gpa_anxiety_gender3)
# TAKEAWAY: THE COEFFICIENTS ARE ALL THE SAME FOR THE BINARY GENDER REGARDLESS IF IT'S A NUMBER, A 1/2 FACTOR, OR A 0/1 FACTOR  

# I need something with three levels to make it worth our while
# Let me try turning age into a categorical factor
unique(students$Age)

sum(is.na(students$Age)) # 2 missing

# creating a new variable with three levels
students <- students %>% 
  mutate(adult_group = case_when(
    Age <= 20 ~ "young",
    Age > 20 & Age <= 25 ~ "mid",
    Age > 25 ~ "older"))

str(students$adult_group)
count(students,adult_group)

# And what happens when we push this to a regression model? 
model_gpa_anxiety_age <- lm(GPA ~ CTA.tot + BStotal + adult_group, data = students) 
summary(model_gpa_anxiety_age) # it works in the model but it makes the "mid" group the reference category b/c it's doing it alphabetically

# So...you want to create a factor so you can set the levels
students$adult_group_factor <- factor(students$adult_group, levels = c("young", "mid", "older")) # it won't look different in the df but the coefficients will be different when you run the model
model_gpa_anxiety_agefactor <- lm(GPA ~ CTA.tot + BStotal + adult_group_factor, data = students) 
summary(model_gpa_anxiety_agefactor)

# let's look at this visually
students |> 
  filter(GPA <= 4) |> 
  drop_na() |> 
  ggplot(aes(x = adult_group_factor, y = GPA)) + geom_boxplot() # this is the median, but no real difference in GPA 

data()

HairEyeColor
sleep
crimtab
data(package = "forcats")

forcats::gsscat
gss_cat

library(forcats)
gss_cat
View(gss_cat)
str(gss_cat)

fct_count(gss_cat$marital)
levels(gss_cat$marital)

View(gss_cat)

model <- lm(tvhours ~ age + race, data = gss_cat)
summary(model)
levels(gss_cat$race)


# COurse: PS 811
# Skill Task 4
# Date: 2020/03/21
# Author: Yi-jung Wu
# Email: wu363@wisc.edu

library("here")       # file paths
library("tidyverse")  # workhorse package
library("broom")
library("estimatr")

here()
cafe <- read_csv(here("data","CAFE.csv"))

#1.recode variables
cafe1<-cafe %>%
  mutate(rep_caucus = case_when(Party_Code == 100 ~ 0, 
                                Party_Code == 200 ~ 1, 
                                Party_Code == 328 ~ 0,),
         yea_vote = case_when(Vote == "Yea" ~ 1,
                              Vote == "Nay" ~ 0))
#Q1
cafe1 %>%
  group_by(rep_caucus) %>%
  summarize(avg_vote = mean(yea_vote, na.rm = TRUE))
#Ans: The vote for Republican Caucus is higher than Democrat Caucus with 0.878-0.373=0.505

#Q2
ols <- lm(yea_vote ~ rep_caucus, data = cafe1)
ols
tidy(ols)
#Ans: Republican Caucus's vote is higher than Democrat Caucus with 0.505, which is the same as in Q2.

#Q3
prediction_data<-tibble(rep_caucus = c(0,1))
augment(ols, newdata = prediction_data)
#Ans:The values are the same as Q1. They are predicted values from the regression model, but in Q1, 
#they are the mean of empirical values.

#Q4
critical_t <- qt(p = .975, df = glance(ols)$df.residual)
predictions <- 
  augment(ols, newdata = prediction_data) %>%
  mutate(
    conf.low = .fitted - (critical_t * .se.fit),
    conf.high = .fitted + (critical_t * .se.fit)
  ) %>%
  print()


#Q5
ols_robust <- lm_robust(yea_vote ~ rep_caucus,data = cafe1)

big_tidy <- ols_robust %>%
  tidy(conf.int = TRUE) %>%
  print()
  
# error: augment(ols_robust)

ggplot(big_tidy) +
  aes(x = rep_caucus, y = yea_vote) 

 

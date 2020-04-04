# COurse: PS 811
# Skill Task 2
# Date: 2020/2/14

library("here")       # file paths
library("tidyverse")  # workhorse package

#Q1
here()
cafe <- read_csv(here("data","CAFE.csv"))

#Q2
names(cafe)
nrow(cafe)
ncol(cafe)
summary(cafe)

#Q3
count(cafe, Contribution)

#Q4
cafe_q4<-cafe %>%
  mutate(Party = case_when(Party_Code == 100 ~ "Democrat", 
                           Party_Code == 200 ~ "Republican", 
                           Party_Code == 328 ~ "Independent",),
        
         Vote_v = case_when(Vote == "Yea" ~ 1,
                            Vote == "Nay" ~ 0))

#Q5
cafe_q4 %>%
  group_by(Party) %>%
  summarize(avg_vote = mean(Vote_v, na.rm = TRUE))

#1 Democrat       0.38 
#2 Independent    0    
#3 Republican     0.878
     
#Q6
cafe_q6<- cafe_q4 %>% 
            select(State, Vote_v) %>%
              group_by(State) %>%
                summarize(sumvote = sum (Vote_v))%>%
                  mutate(co_voted = case_when(sumvote == 0 ~ "Yes",
                                              sumvote == 1 ~ "No",
                                              sumvote == 2 ~ "Yes"))



# COurse: PS 811
# Skill Task 3
# Date: 2020/2/27
# Author: Yi-jung Wu
# Email: wu363@wisc.edu

library("here")       # file paths
library("tidyverse")  # workhorse package
here()
cafe <- read_csv(here("data","CAFE.csv"))

#1.recode variables
cafe1<-cafe %>%
  mutate(Party = case_when(Party_Code == 100 ~ "Democrat Caucus", 
                           Party_Code == 200 ~ "Republican Caucus", 
                           Party_Code == 328 ~ "Democrat Caucus",),
         Yea_Vote = case_when(Vote == "Yea" ~ 1,
                              Vote == "Nay" ~ 0))
#2.plot
p<-
  ggplot(cafe1) + 
  aes(x = Contribution, y = Yea_Vote)+
  geom_point(aes(color = Party))+
  geom_smooth(aes(color = Party), method = "glm", se = FALSE, method.args = list(family = binomial(link = "logit")))+
  scale_color_manual(values = c("Democrat Caucus" = "dodgerblue", "Republican Caucus" = "tomato"))+
  scale_x_log10(labels = scales::dollar_format())+
  labs(
    x = "Campaign Contributions from \n Auto Manufacturer Employees(Log Scale)", 
    y = "Probability of 'Yea' Vote",
    color = NULL
  )+
theme(panel.background = element_rect(fill = "white", color = "white", size = 2, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',  color = "grey"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = "white"),
      legend.position="bottom"
)

p

#3.save plot
ggsave(here("output", "graphic.pdf"), width = 6, height = 4)    
  
              
              
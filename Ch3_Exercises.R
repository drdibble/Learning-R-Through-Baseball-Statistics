library(tidyverse)
hof <- read_csv("hofbatting.csv")


hof <- hof %>%  
  mutate(MidCareer = (From + To)/ 2,  
         Era = cut(MidCareer,  
                   breaks = c(1800, 1900, 1919, 1941,  1960, 1976, 1993, 2050),  
                   labels = c("19th Century", "Dead Ball",  "Lively Ball", "Integration",  "Expansion", "Free Agency",  "Long Ball")))


# Exercise 1
hofpitching <- read_csv("hofpitching.csv")
hofpitching <- hofpitching %>%  
  mutate(BF.group = cut(BF,  c(0, 10000, 15000, 20000, 30000),  
                        labels = c("Less than 10000", "(10000, 15000)",  "(15000, 20000)", "more than 20000"))) 

# A 
frq_table <- hofpitching %>% 
  group_by(BF.group) %>%
  summarize(number=n())

# B 
ggplot(frq_table, aes(BF.group, number)) + geom_col()
# 14 pitchers faced more than 20,000 pitchers in their career

# C
ggplot(frq_table, aes(BF.group, number)) + geom_point() + coord_flip()
# This is not as effective as the bar chart in my opinion. I would only use this type of plot if I had 
# many categories. 

# Exercise 2 
# A 
ggplot(hofpitching, aes(WAR)) + geom_histogram()

# B
hofpitching %>% filter(WAR > 125) %>% pull(X2)
# Walter Johnson and Cy Young stand above the rest in terms of WAR.

# Exercise 3 
hofpitching <- hofpitching %>%  mutate(WAR.Season = WAR/ Yrs) 

# A 
ggplot(hofpitching, aes(BF.group, WAR.Season)) + geom_point() + coord_flip()

# B 
ggplot(hofpitching, aes(BF.group, WAR.Season)) + geom_boxplot() + coord_flip()

# C 
# The WAR per season tends to increase as the number of batters faced increases.

# Exercise 4 
hofpitching <- hofpitching %>%  
  mutate(MidYear = (From + To)/ 2)  

hofpitching.recent <- hofpitching %>%  
  filter(MidYear >= 1960)

hofpitching.recent <- hofpitching.recent %>% arrange(desc(WAR.Season))

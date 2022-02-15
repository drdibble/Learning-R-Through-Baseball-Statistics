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

# A
hofpitching.recent <- hofpitching.recent %>% arrange(desc(WAR.Season))

# B 
ggplot(hofpitching.recent, 
       aes(x=WAR.Season, y = 1, label = X2)) +
  geom_text(angle = 45)

# C 
# Tom Seaver and Bob Gibson stand out in terms of WAR per season. 

# Exercise 5 
# A 
ggplot(hofpitching, aes(MidYear, WAR.Season)) + geom_point() + geom_smooth()

# B 
# The WAR per season seems to be decreasing slightly over the years. 

# C
library(ggrepel)
ggplot(hofpitching, aes(MidYear, WAR.Season)) + 
  geom_point() + 
  geom_smooth() + 
  geom_text_repel(data = filter(hofpitching, MidYear < 1900 & WAR.Season < 2),
                  aes(MidYear, WAR.Season, label = X2))

# Exercise 6
# A 
library(Lahman)

# B 
cobb_id <- Master %>% filter(nameLast == "Cobb", 
                  nameFirst == "Ty") %>% 
  select(playerID) %>% pull()

williams_id <- Master %>% filter(nameLast == "Williams", 
                  nameFirst == "Ted") %>% 
  select(playerID) %>% pull()

rose_id <- Master %>% filter(nameLast == "Rose", 
                  nameFirst == "Pete",
                  birthYear == 1941) %>% 
  select(playerID) %>% pull()


three <- Batting %>% filter(playerID %in% 
                     c(cobb_id, williams_id, rose_id))

# C 
head(Master)
get_birthyear <- function(pid) {
  Master %>%
    filter(playerID == pid) %>% 
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear)) %>%
    select(playerID, birthyear)
}

three_by <- bind_rows(get_birthyear(cobb_id), 
                      get_birthyear(williams_id),
                      get_birthyear(rose_id))

three <- three %>% 
  inner_join(three_by, by = "playerID") %>% 
  mutate(Age = yearID - birthyear) %>% 
  select(playerID, Age, H) %>%
  group_by(playerID) %>%
  mutate(CH = cumsum(H))

# D 
ggplot(filter(three, playerID == rose_id), aes(Age, CH)) +
         geom_line()

# E
ggplot(three, 
       aes(Age, CH, group = playerID, color = playerID)) +
  geom_line()

# F 
# Cobb began his career at the young age of 18 and had the highest cumulative hits
# at each age until he was caught by Pete Rose. Pete Rose caught him largely due to
# his longevity. Ted Williams did not approach the cumulative hits of Rose and Cobb.

# Exercise 7 
# A 
mac_id <- Master %>% filter(nameLast == "McGwire", 
                  nameFirst == "Mark") %>% 
  select(retroID) %>% pull()

sosa_id <- Master %>% filter(nameLast == "Sosa", 
                  nameFirst == "Sammy") %>% 
  select(retroID) %>% pull()

fields <- read_csv("fields.csv")
data1998 <- read_csv("all1998.csv",
                     col_names = pull(fields, Header))

mac.data <- filter(data1998, BAT_ID == mac_id)
sosa.data <- filter(data1998, BAT_ID == sosa_id)

# B 
mac.data <- filter(mac.data, BAT_EVENT_FL == TRUE)
sosa.data <- filter(sosa.data, BAT_EVENT_FL == TRUE)

# C 
mac.data <- mutate(mac.data, PA = 1:nrow(mac.data))
sosa.data <- mutate(sosa.data, PA = 1:nrow(sosa.data))

# D
mac.HR.PA <- mac.data %>%
  filter(EVENT_CD == 23) %>%
  pull(PA)

sosa.HR.PA <- sosa.data %>%
  filter(EVENT_CD == 23) %>%
  pull(PA)

# E 
mac.spacings <- diff(c(0, mac.HR.PA))
sosa.spacings <- diff(c(0, sosa.HR.PA))

# F 
HR_spacing <- rbind(data.frame(Player = "McGwire", spacing = mac.spacings),
                    data.frame(Player = "Sosa", spacing = sosa.spacings))

# G 
ggplot(HR_spacing, aes(spacing)) + geom_histogram() + facet_wrap(~ Player, ncol = 1) # facet wrap!

HR_spacing %>% group_by(Player) %>% summarize(M = median(spacing))



  


library(tidyverse)
library(Lahman)
ws <- filter(SeriesPost, yearID >= 1903,  round == "WS", wins + losses < 8)
ggplot(ws, aes(x = wins + losses)) +  
  geom_bar(fill = "blue") +  
  labs(x = "Number of games", y = "Frequency")

?Batting

hr_leader <- function(data) {
  data %>%  
    group_by(playerID) %>%
    summarize(HR = sum(HR)) %>%
    arrange(desc(HR)) %>%
    head(1)
}

Batting %>% 
  mutate(decade = 10 * floor(yearID / 10)) %>%
  split(pull(., decade)) %>%
  map_df(hr_leader, .id="decade")
    
Batting %>%  
  group_by(playerID) %>%
  summarize(tAB = sum(AB, na.rm = TRUE),  tHR = sum(HR, na.rm = TRUE),  tSO = sum(SO, na.rm = TRUE)) -> long_careers 

Batting_5000 <- filter(long_careers, tAB >= 5000)

head(Batting_5000)

ggplot(Batting_5000, aes(x = tHR/ tAB, y = tSO/ tAB)) +
  geom_point() + 
  geom_smooth()


# Exercise 1
# A
SB <- c(1406, 938, 897, 741, 738, 689, 506, 504, 474)
CS <- c(335, 307, 212, 195, 109, 162, 136, 131, 114)
G <- c(3081, 2616, 3034, 2826, 2476, 2649, 2599, 2683, 2379)

# B 
SB.Attempt <- SB + CS

# C
Success.Rate <- SB / SB.Attempt

# D 
SB.Game <- SB / G

# E 
ggplot(data.frame(SB.Game, Success.Rate), aes(x=SB.Game, y=Success.Rate)) + geom_point()
# Lou Brock had a low success rate while Max Carey had a high success rate.
# Rickey Henderson had the greatest number of stolen bases per game.

# I was eyeballing it but this is a better way of doing it. 
Player = c('Rickey Henderson', 'Lou Brock', 'Ty Cobb',
           'Eddie Collins', 'Max Carey', 'Joe Morgan',
           'Luis Aparicio', 'Paul Molitor',
           'Roberto Alomar')
df <- tibble(Player, Success.Rate, SB.Game)
df %>% arrange(desc(Success.Rate)) %>% head(1)
df %>% arrange(Success.Rate) %>% head(1)

df %>% arrange(desc(SB.Game)) %>% select(Player) %>% 
  slice(1) %>% pull()

# Exercise 2 
# A
outcomes <- c('Single', 'Out', 'Out', 'Single', 'Out', 
              'Double', 'Out', 'Walk', 'Out', 'Single')

# B
table(outcomes)

# C
f.outcomes <- factor(outcomes,  levels = c("Out", "Walk", "Single", "Double"))
table(f.outcomes)
# Now it is ordered by the specified levels ordering

# D 
outcomes == "Walk"
sum(outcomes == "Walk")
# The first line returns a vector of all the times that the outcome was a walk.
# The second line sums all the times that the outcome was a walk.

# Exercise 3 
# A
W <- c(373, 354, 364, 417, 355, 373, 361, 363, 511)
L <- c(208, 184, 310, 279, 227, 188, 208, 245, 316)
Name <- c('Alexander', 'Clemens', 'Galvin', 'Johnson',
          'Maddux', 'Mathewson', 'Nichols', 'Spahn',
          'Young')

# B
Win.PCT <- W / (W+L)

# C
Wins.350 <- data.frame(Name, W, L, Win.PCT)

# D
Wins.350 %>% arrange(desc(Win.PCT))
# Galvin had the lowest winning percentage and Mathewson had the highest winning percentage.

# Exercise 4 
# A 
SO <- c(2198, 4672, 1806, 3509, 3371, 2502, 
        1868, 2583, 2803)
BB <- c(951, 1580, 745, 1363, 999, 844, 
        1268, 1434, 1217)
Name <- c('Alexander', 'Clemens', 'Galvin', 'Johnson',
          'Maddux', 'Mathewson', 'Nichols', 'Spahn',
          'Young')

# B 
SO.BB.Ratio <- SO/BB

# C 
SO.BB <- data.frame(Name, SO, BB, SO.BB.Ratio)

# D 
SO.BB %>% filter(SO.BB.Ratio > 2.8)

# E 
SO.BB %>% arrange(desc(BB))
# Clemens has the most walks but he did have a fairly high strikeout to walk ratio

# Exercise 5
# A 
library(Lahman)

# B 
career.pitching <- Pitching %>%
  group_by(playerID) %>%
  summarize(SO = sum(SO, na.rm = TRUE),  BB = sum(BB, na.rm = TRUE),  IPouts = sum(IPouts, na.rm = TRUE),  midYear = median(yearID, na.rm = TRUE))

career.pitching %>% inner_join(Pitching)

# C 
career.10000 <- career.pitching %>% filter(IPouts >= 10000)

# D
ggplot(career.10000, aes(x=midYear, y=SO/BB)) + geom_point()
# It appears that the strikeout to walk ratio has generally gone up outside of a few early outliers

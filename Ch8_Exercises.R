#### Exercise 1: Career Trajectory of Willie Mays

get_stats <- function(player.id) {
  Batting %>% 
    filter(playerID == player.id) %>%
    inner_join(Master, by = "playerID") %>%
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Age = yearID - birthyear,
           SF = ifelse(is.na(SF) == TRUE, 0, SF),
           SLG = (H - X2B - X3B - HR +
                    2 * X2B + 3 * X3B + 4 * HR) / AB,
           OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
           OPS = SLG + OBP) %>%
    select(Age, SLG, OBP, OPS)
}

library(tidyverse)
library(Lahman)
Master %>% filter(nameFirst == "Willie",
                  nameLast == "Mays") %>% 
  select(playerID) %>% pull() -> mays_id
mays_stats <- get_stats(mays_id)

ggplot(mays_stats, aes(Age, OPS)) +
  geom_point() + ggtitle('Willie Mays Career Trajectory')

fit <- lm(OPS ~ I(Age - 30) + I((Age  - 30) ^ 2), 
          data = mays_stats)
fit$coef

A <- fit$coef[1]
B <- fit$coef[2]
C <- fit$coef[3]
(Peak.Age <- 30 - B / (2 * C))
(MAX <- A - B ^ 2 / 4 / C)


vars <- c("G", "AB", "R", "H", "X2B", "X3B", 
          "HR", "RBI", "BB", "SO", "SB")
Batting %>% 
  group_by(playerID) %>% 
  summarize_at(vars, sum, na.rm = TRUE) -> C.totals
C.totals %>%
  mutate(AVG = H / AB,
         SLG = (H - X2B - X3B - HR + 2 * X2B +
                  3 * X3B + 4 * HR) / AB) ->
  C.totals
Fielding %>% 
  group_by(playerID, POS) %>%
  summarize(Games = sum(G)) %>% 
  arrange(playerID, desc(Games)) %>% 
  filter(POS == first(POS)) -> Positions
C.totals %>%
  inner_join(Positions, by = "playerID") %>%
  mutate(Value.POS = case_when(
    POS == "C" ~ 240,
    POS == "SS" ~ 168,
    POS == "2B" ~ 132,
    POS == "3B" ~ 84,
    POS == "OF" ~ 48,
    POS == "1B" ~ 12, 
    TRUE ~ 0)) -> C.totals
similar <- function(p, number = 10) {
  C.totals %>% filter(playerID == p) -> P
  C.totals %>% 
    mutate(sim_score = 1000 -
             floor(abs(G - P$G) / 20) -
             floor(abs(AB - P$AB) / 75) -
             floor(abs(R - P$R) / 10) -
             floor(abs(H - P$H) / 15) -
             floor(abs(X2B - P$X2B) / 5) -
             floor(abs(X3B - P$X3B) / 4) -
             floor(abs(HR - P$HR) / 2) -
             floor(abs(RBI - P$RBI) / 10) -
             floor(abs(BB - P$BB) / 25) -
             floor(abs(SO - P$SO) / 150) -
             floor(abs(SB - P$SB) / 20) - 
             floor(abs(AVG - P$AVG) / 0.001) - 
             floor(abs(SLG - P$SLG) / 0.002) -
             abs(Value.POS - P$Value.POS)) %>%
    arrange(desc(sim_score)) %>% 
    head(number)
}
s_player <- similar(mays_id, number = 6) %>% 
  select(playerID) %>% pull()
s_player

get_stats2 <- function(playerid){
  d <- get_stats(playerid)
  d %>% mutate(Player = playerid)
}
map_df(s_player, get_stats2) -> alldata
ggplot(alldata, aes(Age, OPS)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              size = 1.5) +
  facet_wrap(~ Player, ncol = 3)

Batting %>% 
  group_by(playerID) %>% 
  summarize(H = sum(H)) %>% 
  filter(H >= 3200) %>% 
  select(playerID) %>% pull() -> p3200
p3200

get_stats_new <- function(player.id){
  Batting %>% 
    filter(playerID == player.id) %>%
    inner_join(Master, by = "playerID") %>%
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Age = yearID - birthyear,
           AVG = H / AB) %>%
    select(Age, AVG) %>% 
    mutate(Player = player.id)
}
map_df(p3200, get_stats_new) -> alldata
ggplot(alldata, aes(Age, AVG)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              size = 1.5) +
  facet_wrap(~ Player, ncol = 4)


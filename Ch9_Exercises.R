library(tidyverse)

# Exercise 1
# A
P <- matrix(c(.3, .7, 0, 0,  
              0, .3, .7, 0,  
              0, 0, .3, .7,  
              0, 0, 0, 1), 4, 4, byrow = TRUE) 
P2 <- P %*% P
# The probability is 0.42 of moving from 0 outs to 1 out after two plate appearances

# B 
N <- solve(diag(c(1, 1, 1)) - P[-4, -4])
#1.43

# Exercise 2
# A
simulate_half_inning <- function(P){  
  s <- 1
  path <- NULL 
  while(s < 4){  
    s.new <- sample(1:4, 1, prob = P[s,])
    path <- c(path, s.new)  
    s <- s.new  
  }  
  length(path)
}

lengths <- replicate(1000, simulate_half_inning(P))
table(lengths)
323/sum(lengths)

# B 
# 0.076 chance of exactly four outs occurring 

# C 
mean(lengths)

# Exercise 3 
# I can't seem to download the 1968 data. 


# Exercise 4 
make_schedule <- function(teams, k) {  
  n.teams <- length(teams)  
  Home <- rep(rep(teams, each = n.teams), k)  
  Visitor <- rep(rep(teams, n.teams), k)  
  schedule <- tibble(Home = Home, Visitor = Visitor) %>%  filter(Home != Visitor)  
}



teams <- c("PHI", "BRO", "NYG", "BSN", "STL", "CIN", "CHC", "PIT")  
league <- c(rep(1, 8))  
schedule <- make_schedule(teams, 11)

s.talent <- 0.25
talents <- rnorm(8, 0, s.talent)  
TAL <- tibble(Team = teams, League = league,  Talent = talents)  
SCH <- schedule %>%  
  inner_join(TAL, by = c("Home" = "Team")) %>%
  rename(Talent.Home = Talent) %>%  
  inner_join(TAL, by = c("Visitor" = "Team", "League")) %>%  
  rename(Talent.Visitor = Talent)

SCH <- SCH %>%  mutate(prob.Home = exp(Talent.Home)/  (exp(Talent.Home) + exp(Talent.Visitor)))

SCH <- SCH %>%  mutate(outcome = rbinom(nrow(.), 1, prob.Home),  winner = ifelse(outcome, Home, Visitor))

RESULTS <- SCH %>%  group_by(winner) %>%  summarize(Wins = n()) %>%  inner_join(TAL, by = c("winner" = "Team"))

# Exercise 5
df <- data.frame("winner" = "", "talent" = "-")

simulated_yr <- function(teams, league, schedule, talent, df){
  talents <- rnorm(8, 0, talent)  
  TAL <- tibble(Team = teams, League = league,  Talent = talents)  
  SCH <- schedule %>%  
    inner_join(TAL, by = c("Home" = "Team")) %>%
    rename(Talent.Home = Talent) %>%  
    inner_join(TAL, by = c("Visitor" = "Team", "League")) %>%  
    rename(Talent.Visitor = Talent)
  
  SCH <- SCH %>%  mutate(prob.Home = exp(Talent.Home)/  (exp(Talent.Home) + exp(Talent.Visitor)))
  
  SCH <- SCH %>%  mutate(outcome = rbinom(nrow(.), 1, prob.Home),  winner = ifelse(outcome, Home, Visitor))
  
  RESULTS <- SCH %>%  group_by(winner) %>%  summarize(Wins = n()) %>%  inner_join(TAL, by = c("winner" = "Team"))
  RESULTS <- RESULTS[order(-RESULTS$Wins),]
  winner <- RESULTS[1,1]
  RESULTS <- RESULTS[order(-RESULTS$Talent),]
  talent <- RESULTS[1,1]
  
  df[nrow(df) + 1,] <- data.frame(c(winner, talent))
}

simulate_1k <- function(teams, league, schedule, talent, df){
  for (x in 1:1000){
    row <- simulated_yr(teams, league, schedule, talent, df)
    df[nrow(df) + 1,] <- row
  }
}

df_1k <- simulate_1k(teams, league, schedule, s.talent, df)

df_new <- simulated_yr(teams, league, schedule, s.talent, df)

one.simulation.68 <- function(s.talent = 0.20){
  require(dplyr)
  make.schedule <- function(teams, k){
    n.teams <- length(teams)
    Home <- rep(rep(teams, each=n.teams), k)
    Visitor <- rep(rep(teams, n.teams), k)
    schedule <- tibble(Home = Home,
                       Visitor = Visitor)
    dplyr::filter(schedule, Home != Visitor)
  }
  
  NL <- c("ATL", "CHN", "CIN", "HOU", "LAN",
          "NYN", "PHI", "PIT", "SFN", "SLN")
  AL <- c("BAL", "BOS", "CAL", "CHA", "CLE",
          "DET", "MIN", "NYA", "OAK", "WS2")
  teams <- c(NL, AL)
  league <- c(rep(1, 10), rep(2, 10))
  
  schedule <- bind_rows(make.schedule(NL, 9),
                        make.schedule(AL, 9))
  
  # simulate talents
  talents <- rnorm(20, 0, s.talent)
  TAL <- tibble(Team = teams, League = league,
                Talent = talents)
  
  # merge talents and win probs with schedule data frame
  SCH <- schedule %>%
    inner_join(TAL, by = c("Home" = "Team")) %>%
    rename(Talent.Home = Talent) %>%
    inner_join(TAL, by = c("Visitor" = "Team", "League")) %>%
    rename(Talent.Visitor = Talent)
  
  # play season of games
  SCH %>% 
    mutate(prob.Home = exp(Talent.Home) /
             (exp(Talent.Home) + exp(Talent.Visitor))) -> SCH
  
  SCH %>%
    mutate(outcome = rbinom(nrow(.), 1, prob.Home),
           winner = ifelse(outcome, Home, Visitor)) -> SCH
  
  # compute number of games won for all teams
  SCH %>% 
    group_by(winner) %>%
    summarize(Wins = n()) %>%
    inner_join(TAL, by = c("winner" = "Team"))   -> RESULTS
  
  win_league <- function(RR) {
    out <- RR %>%
      mutate(Winner.Lg = 0, 
             prob = exp(Talent),
             outcome = sample(nrow(.), prob = prob)) %>%
      arrange(League, desc(Wins), outcome) %>%
      select(-outcome)
    out[1 + c(0, nrow(RR) / 2), "Winner.Lg"] <- 1
    out
  }
  
  # record if eligible for wild card (Wild), in playoffs (Playoff)
  # in conference playoff (CS), World Series (WS), or winner (B)
  
  RESULTS <- win_league(RESULTS)
  
  ws_winner <- RESULTS %>%
    filter(Winner.Lg == 1) %>%
    mutate(outcome = rmultinom(1, 7, prob),
           Winner.WS = ifelse(outcome > 3, 1, 0)) %>%
    filter(outcome > 3) %>%
    select(winner, Winner.WS)
  
  # data frame has teams, division, talent, wins, and diff playoff results
  RESULTS %>%
    left_join(ws_winner, by = c("winner")) %>%
    replace_na(list(Winner.WS = 0)) %>% 
    rename(Team = winner) %>% 
    select(- prob)
}

Many.Results <- map_df(rep(0.25, 1000),  one.simulation.68)






library(Lahman)
library(tidyverse)

# Exercise 1
# A 
Eras <- Teams %>% 
  filter(yearID >= 1961, yearID <= 2000) %>% 
  mutate(Era = ifelse(yearID <= 1970, "1961-1970", 
                      ifelse(yearID <= 1980, "1971-1980",
                             ifelse(yearID <= 1990, "1981-1990",
                                    "1991-2000"))),
         W_pct = W / (W+L))

lin_model <- function(years){
  lm(W_pct ~ I(R-RA),
     data = filter(Eras, Era == years))
}

the_eras <- c("1961-1970", "1971-1980", "1981-1990", "1991-2000")
four_models <- lapply(the_eras, lin_model)
names(four_models) <- the_eras
sapply(four_models, coef)

# B
rd_10 <- function(fit){
  predict(fit, data.frame(R=10, RA=0))
}

162 * sapply(four_models, rd_10)
# In all four eras, you can expect to win 82 games with a season run differential of 10 runs. 
# This supports the belief that one extra win is worth about 10 runs.

# Exercise 2 
# A 
pre_19 <- Teams %>% 
  filter(yearID < 1900) %>% 
  mutate(W_pct = W / (W+L))
pre_19_model <- lm(W_pct ~ I(R - RA), data = pre_19)
pre_19_model

# B 
library(broom)
out <- augment(pre_19_model)
out <- out %>% mutate(rating = ifelse(W_pct >= 0.7, "great",
                                      ifelse(W_pct <= 0.3, "bad", "other")))
ggplot(out, aes(I.R...RA., .std.resid, color=rating)) + 
  geom_point() +
  geom_hline(yintercept=0)

# Exercise 3 
# A 
decade <- Teams %>% 
  filter(yearID >= 2000, yearID <= 2009) %>% 
  mutate(W_pct = W / (W+L))
decade_model <- lm(W_pct ~ I(R - RA), data = decade)

# B 
out <- augment(decade_model, data = select(decade, yearID, teamID, R, RA))
out <- out %>% 
  inner_join(
    select(Managers, playerID, yearID, teamID), 
    by = c("yearID", "teamID")) # You can join based on a vector to specify multiple matching conditions!
out <- out %>% 
  group_by(playerID) %>%
             summarize(N = n(), Mean_Residual = mean(.std.resid)) %>% 
             arrange(desc(Mean_Residual))
head(out)
tail(out)
# Is this showing that the coaches who won much more than expected (i.e. high residuals) only coached one year?
# It appears the coaches who outperformed the run differential model's expectation coached only one year, 
# while the coaches who underperformed the model's expectation coached 3, 1, 2, and 2 years during 2000-2009.

# Exercise 4 
library("nbastatR")
?nbastatR             
nba <- bref_teams_stats()

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(jpeg)
library(png)
library(ggimage)

# setwd('Desktop/archive')
raptor <- read.csv('train.csv')

full <- raptor %>% filter(season_x.1 == 2022) %>% select(c('player_name_x.1', 
                                                                'mp_x.1', 'raptor_offense_x.1', 
                                                                'raptor_defense_x.1', 'raptor_total_x.1')) %>%
                                                  distinct() %>% 
                                                  arrange(-raptor_total_x.1)


this_year <- raptor %>% filter(season_x.1 == 2022) %>% select(c('player_name_x.1', 
                                                                'mp_x.1', 'raptor_offense_x.1', 
                                                                'raptor_defense_x.1', 'raptor_total_x.1')) %>%
                                                  distinct() %>% 
                                                  arrange(-raptor_total_x.1) %>%
                                                  filter(mp_x.1 >= 1000)

head(this_year, 15)

ggplot(this_year, aes(raptor_total_x.1)) + geom_histogram() + xlab('RAPTOR') + 
  ggtitle('2022 RAPTOR Distribution')

top_15 <- head(this_year, 15)
top_30 <- head(this_year, 30)

# write_csv(top_30, 'top_30.csv')

player_img <- read_csv('top_30.csv')

top_30 <- top_30 %>% inner_join(player_img, by = "player_name_x.1")

ggplot(top_15, aes(x=raptor_total_x.1, y=reorder(player_name_x.1, raptor_total_x.1))) + 
  geom_bar(stat='identity') + 
  xlab('RAPTOR') + 
  ylab('Player') +
  ggtitle('Top 15 Players') + 
  theme_classic()

ggplot(top_30, aes(x=raptor_offense_x.1, y=raptor_defense_x.1, image=image)) + 
  geom_image(size=0.1) +
  geom_text(x=8, y=-1, label="Scorers", family="Times New Roman") + 
  geom_text(x=0, y=5, label="Stoppers", family="Times New Roman") + 
  xlab('Offensive RAPTOR') +
  ylab('Defensive RAPTOR') + 
  theme_classic()

ggplot(top_30, aes(x=mp_x.1, y=raptor_total_x.1, image=image)) + 
  geom_image(size=0.1) +
  xlab('Minutes Played') +
  ylab('RAPTOR') + 
  theme_classic()




salary <- read.csv('Salary.csv')

combo <- this_year %>% 
  inner_join(salary, by=c("player_name_x.1" = "Player")) %>%
  distinct(player_name_x.1, .keep_all = TRUE) %>% 
  filter('X2021.22' > 0) %>%
  drop_na('X2021.22') %>%
  mutate(Raptor_Dollar = raptor_total_x.1 / X2021.22) %>% 
  arrange(-Raptor_Dollar)

full_combo <- full %>% 
  inner_join(salary, by=c("player_name_x.1" = "Player")) %>%
  distinct(player_name_x.1, .keep_all = TRUE) %>% 
  filter('X2021.22' > 0) %>%
  drop_na('X2021.22') %>%
  mutate(Raptor_Dollar = raptor_total_x.1 / X2021.22) %>% 
  arrange(-Raptor_Dollar)

#compare average age of best and worst players 
player_ages <- read.csv('player_ages.csv')
combo_age <- combo %>% 
  inner_join(player_ages, by=c('player_name_x.1' = "FULL.NAME")) %>%
  distinct(player_name_x.1, .keep_all = TRUE)

complete <- full_combo %>% 
  inner_join(player_ages, by=c('player_name_x.1' = "FULL.NAME")) %>%
  distinct(player_name_x.1, .keep_all = TRUE) %>% 
  mutate(raptor_min = raptor_total_x.1 * mp_x.1) %>%
  mutate(o_raptor_min = raptor_offense_x.1 * mp_x.1) %>%
  mutate(d_raptor_min = raptor_defense_x.1 * mp_x.1) %>%
  mutate(age_min = AGE * mp_x.1)

best_for_money <- head(combo_age, 15) %>% 
  select(c('player_name_x.1', 'raptor_total_x.1', 'X2021.22', 'Raptor_Dollar'))
#worst_for_money <- tail(combo, 10)

#Team

team_wise_total_best <- combo_age %>% group_by(Tm) %>% 
  filter(raptor_total_x.1 == max(raptor_total_x.1)) %>%
  arrange(-raptor_total_x.1)
team_wise_total_worst <- combo_age %>% group_by(Tm) %>% filter(raptor_total_x.1 == min(raptor_total_x.1))

team_wise_value <- combo_age %>% group_by(Tm) %>% 
  filter(Raptor_Dollar == max(Raptor_Dollar)) %>%
  arrange(-Raptor_Dollar)

raptor_standings <- complete %>% 
  group_by(Tm) %>% 
  summarize(min_weighted_raptor = sum(raptor_min)) %>%
  arrange(-min_weighted_raptor)

logos <- read_csv('logos.csv')

raptor_standings <- complete %>% 
  group_by(Tm) %>% 
  summarize(minute_weighted_raptor = sum(raptor_min) / sum(mp_x.1), 
            o_minute_weighted_raptor = sum(o_raptor_min) / sum(mp_x.1),
            d_minute_weighted_raptor = sum(d_raptor_min) / sum(mp_x.1),
            minute_weighted_age = sum(age_min) / sum(mp_x.1)) %>%
  arrange(-minute_weighted_raptor)

raptor_standings <- raptor_standings %>% inner_join(logos, by="Tm")

ggplot(raptor_standings, aes(x=o_minute_weighted_raptor, y=d_minute_weighted_raptor, image=Image_Path)) + 
  geom_image(size=0.1) +
  xlab('Average Offensive RAPTOR') +
  ylab('Average Defensive RAPTOR') + 
  geom_text(label="Elite", x=0.75, y=1.2) + 
  geom_text(label="Poor", x=-1.1, y=-0.5) + 
  ggtitle('Average Team RAPTOR Performance Based on Minutes Played') +
  theme_classic()

ggplot(raptor_standings, aes(x=min_weighted_age, y=min_weighted_raptor, image=Image_Path)) + 
  geom_image(size=0.1) +
  xlab('Average Age') +
  ylab('Average Raptor') + 
  ggtitle('Average Raptor and Age Based on Minutes Played') +
  theme_classic()

#Age
mean(team_wise_total_best$AGE)
mean(team_wise_total_worst$AGE)
mean(team_wise_value$AGE)

ggplot(combo_age, aes(AGE, raptor_total_x.1)) + 
  geom_point() + 
  geom_smooth(method=lm) + 
  xlab('Age') + 
  ylab('Raptor')

#Position
position <- combo_age %>% group_by(POS) %>% summarize(avg = mean(raptor_total_x.1))
ggplot(position, aes(x=POS, y=avg)) + 
  geom_bar(stat='identity') + 
  geom_hline(yintercept = mean(combo_age$raptor_total_x.1), col = "blue") + 
  xlab('Position') + 
  ylab('Average Raptor')


# write_csv(combo, 'combo.csv')


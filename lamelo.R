library(tidyverse)
library(ggrepel)
setwd('Desktop')
lamelo <- read_csv('lamelo.csv')
lamelo <- lamelo %>% filter(GS != "Inactive" & GS != "Did Not Play")

lamelo$Impact <- as.numeric(as.character(lamelo$Impact))  # Convert one variable to numeric
lamelo$DRB <- as.numeric(as.character(lamelo$DRB))  # Convert one variable to numeric
sapply(lamelo$DRB, class)

lamelo <- lamelo %>% mutate(Result = ifelse(grepl('W', X8), 'W', 'L'))

ggplot(lamelo, aes(DRB, Impact)) + 
  geom_point(aes(color=Result)) + 
  geom_smooth() +
  xlab('Defensive Rebounds') + 
  ylab('+/-') +
  ggtitle('Lamelo Ball Defensive Rebounds vs. Plus-Minus') +
  geom_text_repel(data = filter(lamelo, is.na(G)),  aes(DRB, Impact, label = 'Last Night'))

ggplot(lamelo, aes(DRB, Impact)) + 
  geom_point(aes(color=Result)) + 
  geom_smooth() +
  xlab('Defensive Rebounds') + 
  ylab('+/-') +
  ggtitle('Lamelo Ball Defensive Rebounds vs. Plus-Minus') +
  geom_text_repel(data = filter(lamelo, G==8),  aes(DRB, Impact, label = 'Nov. 1 vs Cavs'))


losses <- lamelo %>% 
  filter(Result == 'L')

losses <- losses[order(-losses$Impact),]

head(losses)

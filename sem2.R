#install.packages("haven")

library(haven)
library(tidyverse)

ess <- read_dta("data/ESS9NO.dta")

ess_csv <- read.csv("https://raw.githubusercontent.com/egen97/STV1020v2023/main/data/ESS9NO.csv")

summary(ess)

ess_subset <- select(ess, vote, news = nwspol, interest = polintr, year_born = yrbrn)
ess_subset2 <- filter(ess_subset, year_born >= 1990)

ess_subset <- ess %>%  
  select(vote, news = nwspol, interest = polintr, year_born = yrbrn) 

ess_subset <- ess_subset %>% 
  mutate(age = 2018 - year_born)

ess_subset <- ess_subset %>% 
  mutate(vote_factor = as.factor(vote),
         news = as.numeric(news))

class(ess_subset$vote_factor)
levels(ess_subset$vote_factor)
mean(ess_subset$vote_factor)  

head(ess_subset)
max(as.numeric(ess_subset$news), na.rm = TRUE)
max(ess_subset$news, na.rm = TRUE)

tail(ess_subset$vote_factor)


prop.table(
  table(ess_subset$vote_factor)
)


table(ess_subset$vote_factor, ess_subset$interest)

quantile(ess_subset$age, na.rm = TRUE)

nei <- ess_subset %>% 
  filter(year_born <= 1990) %>% 
  mutate(age = as.numeric(age))

quantile(nei$age)



ggplot(ess_subset, aes(vote_factor)) +
  geom_bar()

!is.na(NA)

ess_subset %>% 
  filter(!is.na(vote_factor)) %>% 
  ggplot(aes(vote_factor)) +
  geom_bar()
  

ess_subset %>% 
  filter(!is.na(interest)) %>% 
  ggplot(aes(interest, fill = vote_factor)) +
  geom_bar(position = "dodge")


ggplot(ess_subset, aes(news)) +
  geom_boxplot()


 

ess_subset %>% 
  ggplot(aes(age)) +
  geom_density(fill = "steelblue", alpha = .6) +
  ggthemes::theme_excel_new()

table(is.na(ess_subset$age))


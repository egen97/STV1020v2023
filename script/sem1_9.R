#install.packages("haven")

library(tidyverse)
library(haven)

#ESS <- read_dta("data/ESS9NO.dta") #LA


ESS_csv <- read.csv("https://raw.githubusercontent.com/egen97/STV1020v2023/main/data/ESS9NO.csv")

camelCase
snake_case
ESS_csv
ess_csv
library(tidyverse)
ess_subset <- ESS %>% 
  select(news = nwspol, interest = polintr,
         year_born = yrbrn, vote)
  
  

ess_subset <- ess_subset %>% 
  mutate(age = 2018 - year_born)
  

summary(ess_subset)

class(ess_subset$year_born)
class(ess_subset$vote)

mean(ess_subset$vote_fct, na.rm = TRUE)

ess_subset <- ess_subset %>% 
  mutate(vote_fct = as.factor(vote))


class(ess_subset$vote_fct)

mean(ess_subset$vote_fct)
table(ess_subset$vote_fct)
prop.table(
  table(ess_subset$vote_fct)
)

table(ess_subset$vote_fct, ess_subset$interest)
tail(ess_subset)
head(ess_subset)
table(ess_subset$vote,ess_subset$vote_fct )


ggplot(ess_subset, aes(age)) +
  geom_density(position = "dodge")

ggplot(ess_subset, aes(age)) +
  geom_bar()

ggplot(ess_subset, aes(vote, fill = as.factor(interest))) +
  geom_bar(position = "dodge")


1964 >= 1990
ess_subset %>% 
  filter(year_born >= 1990) %>% 
  ggplot(aes(vote, fill = as.factor(interest))) +
  geom_bar(position = "dodge")

install.packages("ggthemes")

ggplot(data = ess_subset, aes(x = age)) +
  geom_bar()

ess_subset %>% 
  ggplot(aes(age)) +
  geom_density(fill = "steelblue", alpha = .3) +
  ggthemes::theme_excel_new() +
  labs(title = "Alderfordeling i ESS runde 9")

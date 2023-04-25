
ANES1996small <- read.csv("https://raw.githubusercontent.com/martigso/STV1020/main/data/ANES1996small.csv")

library(tidyverse)
library(stargazer)

names(ANES1996small)



ANES1996small <- ANES1996small %>% 
  rename(hillary_thermo = v960281,
         income = v960701,
         womenmvmt_thermo = v961039,
         gender = v960066,
         age = v960605)



ANES1996small2 <- ANES1996small %>% 
  select(hillary_thermo,
         income,
         womenmvmt_thermo,
         gender,
         age)


stargazer(ANES1996small2, type = "text")



table(
  complete.cases(ANES1996small2)
)


table(is.na(ANES1996small2$hillary_thermo))



ANES1996small23 <- ANES1996small2 %>% 
  select(age) %>% 
  arrange(age)


ANES1996small2 <- ANES1996small2 %>%
  mutate(female = ifelse(gender == 1, 0, 1))


table(ANES1996small2$gender, ANES1996small2$female )


ggplot(ANES1996small2, aes(hillary_thermo)) +
  geom_histogram()

ggplot(ANES1996small2, aes(hillary_thermo)) +
  geom_density() +
  theme_classic() +
  labs(title = "Hillary Thermometer", x = "")


ggplot(ANES1996small2, aes(income)) +
  geom_density() +
  theme_classic() +
  labs(title = "Inntekt", x = "")


ggplot(ANES1996small2, aes(income, hillary_thermo)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm") +
  theme_classic()

ANES1996small2
ggplot(ANES1996small2) +
  geom_density(aes(womenmvmt_thermo), fill = "red") +
  geom_density(aes(hillary_thermo), fill = "blue", alpha = .1)

summary(ANES1996small2$income)
summary(model1)


stargazer(model1, type = "text")


model0 <- lm(hillary_thermo ~ income, 
             data = ANES1996small2, na.action = "na.exclude")


summary(model0)
stargazer(model0, type = "text")


model1 <- lm(hillary_thermo ~ income + female,
             data = ANES1996small2,
             na.action = "na.exclude")

stargazer(model0, model1, 
          type = "text",
          dep.var.labels = "Hillary Termometer",
          covariate.labels = c("Income",
                               "Female"))





ANES1996small2 <- ANES1996small2 %>% 
  mutate(mod1fitted = fitted(model1),
         mod1resid = resid(model1))


model2additiv <- lm(hillary_thermo ~ female + womenmvmt_thermo, 
                    data = ANES1996small2, na.action = "na.exclude")


model2samspill <- lm(hillary_thermo ~ female * womenmvmt_thermo, 
                    data = ANES1996small2, na.action = "na.exclude")

#POSE
#CTRL+SHIFT+F
summary(ANES1996small2$hillary_thermo)



stargazer(model2additiv, model2samspill, type = "text",
          title = c("Tabell fra Kellstedt og Whitten s. 257"),
          covariate.labels = c("Female",
                               "Women's Movement Thermometer",
                               "Women's Movement Thermometer x Female",
                               "Intercept"),
          dep.var.labels = "Hillary Clinton Thermometer score")


install.packages("interactions")
library("interactions")

interact_plot(model2samspill,
              pred = "womenmvmt_thermo",
              modx = "female")


ggplot(data = ANES1996small2 ,
       aes(x = mod1fitted, y = mod1resid)) +
  geom_point() + 
  geom_smooth() + 
  theme_bw() +
  xlim(0, 100)



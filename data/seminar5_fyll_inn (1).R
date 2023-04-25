### SEMINAR 5 ####
# I dag skal vi se på fem ting:
#   
#   1. Laste inn data (repetisjon)
# 2. Omkoding av variabler (repetisjon)
# 3. Plotting (repetisjon)
# 4. Kjøre en regresjonsmodell med en uavhengig variabel (nytt)
# 5. Tolkning og fremstilling av regresjonsresultater (nytt)
# 
# Datasettet vi skal bruke er det samme som det som omtales i kapittel ni i The
# Fundamentals of Political Science Research. I likhet med kapittel ni så skal vi 
# kjøre en regresjon der vi ser på effekten av økonomisk vekst (`growth`)
# på andel stemmer partiet til den sittende kandidaten får (`inc_vote`). Det første vi skal gjøre er å åpne prosjektfilen vår/sette working directory, laste inn pakker og laste inn datasettet: 
  



#laste pakker og data

# Laster inn pakker
library(tidyverse)
library(stargazer)

# Laster inn datasettet
# Bytt ut det som står mellom "" til å passe din filbane:

load("data/FairFPSR3.RData")


str(FairFPSR3)

summary(FairFPSR3)


table(complete.cases(FairFPSR3))




## Undersøker data
#Når vi skal kjøre en regresjonsanalyse så er noe av det første vi gjør å undersøke datasettet: 
  


  

  

# Sjekker hvor mange observasjoner som har missing på variabelen inflation
table(is.na(FairFPSR3$inflation))



 

#Legg merke til at funksjonene `complete.cases()` og `is.na()` er logiske tester. Disse evaluerer hver observasjon og sjekker om vi har informasjon om alle variabler (`complete.cases()`) og om vi mangler informasjon på variabelen `inflation`. For å illustrere dette så kan vi prøve å legge til to nye variabler i datasettet basert på disse logiske testene: 
  

FairFPSR3 <- FairFPSR3 %>% 
  mutate(complete = complete.cases(.),
         inf_na = is.na(inflation))



summary(FairFPSR3$growth)
## Omkoding av variabler
# Oppretter den nye variabelen og endrer referansekategori
FairFPSR3 <- FairFPSR3 %>% 
  mutate(growth_dich = ifelse(growth > 0, "Growth", "No growth"),
         growth_dich = factor(growth_dich, levels = c("No growth", "Growth")))

class(FairFPSR3$growth_dich)
  
levels(FairFPSR3$growth_dich)

table(FairFPSR3$growth, FairFPSR3$growth_dich) #KRYSSTABELL

p <- ggplot(data = FairFPSR3) +
  geom_bar(aes(x=growth, fill = growth_dich),
           binwidth = 1) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(x = "Growth rate",
       y = "No. of observations")


ggsave("gpicture.png", p) #LAGE GRAF



#Ut fra plottet kan vi se at alle observasjonene med verdien `No growth` hadde negativ vekstrate, mens alle observasjoner med verdien `Growth` hadde positiv vekstrate. 



## Litt plotting før regresjon
## Før du kjører en regresjon så kan det være lurt å plotte den avhengige og den uavhengige variabelen din. I vårt tilfelle er dette variabelene `inc_vote` og `growth`. For å få til dette bruker vi `ggplot`.





ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita")

ggsave("../../output/sem5_fig2.png")


ggplot(FairFPSR3, aes(growth, inc_vote)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Økonomisk vekst",
    y = "Stemmer på sittende president"
  ) +
  geom_smooth(method = "lm")






cor.test(FairFPSR3$inc_vote, FairFPSR3$growth)

## Regresjon med numerisk uavhengig variabel

#For å kjøre en lineær regresjon i R så bruker vi funksjonen `lm()`.  `lm()` har følgende syntaks: 
  

# lm(avhengig_variabel ~ uavhengig_variabel + uavhengig_variabel2 + uavhengig_variabel3, data = mitt_datasett)




model <- lm(inc_vote ~ growth, 
            data = FairFPSR3)

model
summary(model)




stargazer(model, 
          type = "html",
          covariate.labels = c("Økonomisk vekst"),
          dep.var.labels = "Stemmer sittende president",
          out = "regtabel.html"
          )



ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) + #SCATTERPLOTT
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita")



ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita")

ggsave("../../output/sem5_fig3.png")





### Legge til regresjonslinje med `geom_smooth`


  

ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita") 



# I kapittel ni viser Kellstedt og  Whitten at regresjonslinjen krysser
# utvalgets gjennomsnittsverdier på uavhengig og avhengig variabel. Det kan vi også
# vise ved å legge til to linjer i koden vår:
  

ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita") +
  geom_smooth(aes(x = growth, y = inc_vote),
              method = "lm", color = "goldenrod3") +
  geom_hline(yintercept=mean(FairFPSR3$inc_vote), linetype = "dashed") +
  geom_vline(xintercept = mean(FairFPSR3$growth), linetype = "dashed")



### Legge til regresjonslinje med `fitted()` og `geom_line()`

FairFPSR3 <- FairFPSR3 %>% 
  mutate(fitted = fitted(model), 
         residuals = resid(model))



ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita") +
  geom_line(aes(x = growth, y = fitted))


ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita") +
  geom_line(aes(x = growth, y = fitted)) +
  geom_hline(yintercept=mean(FairFPSR3$inc_vote), linetype = "dashed") +
  geom_vline(xintercept=mean(FairFPSR3$growth), linetype = "dashed")



confint(model)


## Regresjon med dikotom uavhengig variabel

# Lagrer modellen
model_dich <- lm(inc_vote ~ growth_dich, 
                 data = FairFPSR3,
                 na.action = "na.exclude")


stargazer(model, 
          type = "text")

ggplot(FairFPSR3, aes(growth)) +
  geom_density()





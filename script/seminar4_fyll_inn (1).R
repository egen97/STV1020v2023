
################################################################
####################### SEMINAR 4 ##############################
################################################################

## Vi skal lære:
# 1. Repetere å laste inn data
# 2. Repetere å få oversikt over dataene
# 3. Statistiske mål
## 3a. Sentraltendens
## 3b. Spredning
# 4. Lage tabeller
# 5. Bivariat hypotesetesting
## 5a. Tabellanalyse (to kategoriske variabler)
## 5b. T-test (en kategorisk og en kontinuerlig variabel)
## 5c. Korrelasjonskoeffisient (to kontinuerlige variabler)


#-# betyr fyll inn selv


## 1. LASTE INN DATA

##### Installerer nye pakker #####
# install.packages("stargazer")
# install.packages("gmodels")
# #-#("scales")

##### Laster inn pakker #####
library(tidyverse)
library(stargazer)
library(gmodels)
#-#(scales)


##### Laste inn .csv-filer #####
ess <- #-#

##### Lagre .csv-filer #####
#-#(ess, file = "internett_lagret.csv")


## 2. FÅ OVERSIKT OVER DATAENE 

# View(ess)
#-#(ess)
#-#(ess)
#-#(ess)

#-#(ess) 


## 3. MISSINGVERDIER 

ess %>% # Går inn i datasettet ess
  slice_head(n = 6) %>% # Og henter ut de seks første radene
  is.na() # Og sjekker med en logisk test om disse er NA

# Vi kan telle opp antall missingverdier med funksjonen sum()
sum(2, 2, 6) # eksempel på hvordan sum() fungerer

sum(TRUE, FALSE, TRUE, TRUE, FALSE) # Bruker vi sum() på TRUE og FALSE, regner den TRUE som 1 og FALSE som 0


# Teller antall missing blant de seks første observasjonene

ess %>% 
  #-# %>%
  #-# %>%
  #-#


# Vi kan også sjekke missingverdier for bare en variabel:
ess %>% 
  slice_head(n = 6) %>% # Hente ut de seks første observasjonene
  #-# %>% # Hente ut variabelen tillit
  is.na() # Finne ut om disse observasjonene har missing

ess %>% 
  slice_head(n = 6) %>% 
  #-# %>%
  is.na() %>% 
  #-# # Legger på sum() for å telle opp antall missing


# For å finne antall missing i hele datasettet:
ess %>% 
  is.na() %>% 
  sum()


# Funksjonen complete.cases() går motsatt vei fra is.na().
# Den spør hvilke observasjoner som -ikke- har missingverdier.
ess %>% 
  slice_head(n = 6) %>% 
  complete.cases()

# Vi kan bruke complete.cases() sammen med sum() for å sjekke hvor mange hele observasjoner (ikke missing) vi har i datasettet
ess %>% 
  #-# %>%
  #-#


# Denne informasjonen dukker også opp når vi bruker den velkjente summary() funksjonen

summary(ess) # For alle variabler i datasettet

#-#(ess$internettbruk) # For en variabel i datasettet


##### Fjerne NA #####

ess_nona <- ess %>% 
  drop_na() # Fjerne alle observajoner med minst en missing  

ess_nona_internet <- ess %>% 
  #-# # Fjerne alle observasjoner med missing på en variabel (eller fler) 

# Du kan legge til flere variabeler med komma, eks. drop_na(internettbruk, tillit)


rm(ess_nona, ess_nona_internet) # Vi skal ikke bruke data1 og data2 mer så jeg fjerner dem fra environment



## 3. STATISTISKE MÅL

ess %>%
  ggplot(aes(x = internettbruk)) + 
  geom_histogram()


## 3a. SENTRALTENDENS

# Gjennomsnitt
#-#

# Median
#-#

# Modus
ess %>% 
  count(internettbruk) %>% 
  filter(n == max(n))


## 3b. SPREDNING

# Standardavvik

#-# # Gjennomsnittlig avstand fra gjennomsnittet


# Varians

#-#


# Forholdet mellom varians og standardavvik

stdavvik <- sd(ess$internettbruk, na.rm = TRUE) # Legger standardavviket inn i et eget objekt
varians <- var(ess$internettbruk, na.rm = TRUE) # Legger variansen inn i et eget objekt


stdavvik^2 # Varians er standardavvik opphøyd i annen
varians == stdavvik^2

sqrt(varians) # Standardsavviket er kvadratroten av variansen
stdavvik == sqrt(varians)
# Standardavviket er gjerne enklere å tolke

rm(stdavvik, varians)



## 4. Lage tabeller

#-#


stargazer(ess, type = "html", out = "internett.html",
          covariate.labels = c("Internettbruk", "Kjønn", 
                               "Alder", "Utdanning", 
                               "Tillit til parlamentet"))

# Bonus: 
ess %>%
  group_by(kjonn) %>%
  summarise(internett_gjennomsnitt = mean(internettbruk, na.rm = TRUE),
            tillit_gjennomsnitt = mean(tillit, na.rm = TRUE)) %>%
  knitr::kable() %>%
  kableExtra::kable_minimal()



## 5. Bivariat hypotesetesting

load("ANES2016small.RData") # Laste inn nytt datasett (fra K&)

## Hvilken test som passer avhenger av målenivået på avhengig og uavhengig variabel:
# Kategorisk avhengig og uavhengig variabel: tabellanalyse
# Kontinuelig avhengig og kategorisk uavhengig variabel: t-test
# Kontinuerlig avhengig og uavhengig variabel: korrelasjonskoeffisient

## Er det en sammenheng mellom kjønn og stemmegivning til ulike presidenter 
# i det amerikanske presidentvalget mellom Trump og Clinton?


## 5a. TABELLANALYSE

# Når vi har to kategoriske variabler

# Steg 1 er å rydde litt i dataene
# Vi omkoder variabelene "V2Trump" og "female" fra 0 og 1 til ord som sier noe om hva verdien betyr
# Vi kan gjøre dette med både case_match() og ifelse()

# Alternativ med case_match()
ANES2016small <- ANES2016small %>% 
  mutate(vote = case_match(V2Trump,
                           1 ~ "Trump",
                           0 ~ "Clinton"), 
         gender = case_match(female, 
                             0 ~ "Male", 
                             1 ~ "Female"))

# Alternativ med ifelse()

#-#


# Sjekker at omkodingene ble riktige:
table(ANES2016small$female, ANES2016small$gender, useNA = "always")
table(ANES2016small$V2Trump, ANES2016small$vote, useNA = "always")

#-#(ANES2016small$vote)
#-#(ANES2016small$gender)


## Krysstabell ##

krysstabell <- #-#

krysstabell


## Proporsjontabell ##

prop.table(krysstabell, 
           margin = 2) # Spesifiserer at vi vil ha andeler per kolonne 
# (margin = 1 gir andeler per rad, bare margin er andel per celle)


## Kji-kvadrattest ##

# Kji-kvadratet er 19,371 og p-verdien er 0,00001076

format(1.076e-05, scientific = FALSE) # For å se tallet uten vitenskapelig notasjon


# En mer utfyllende tabell med kji-kvadrattest
CrossTable(ANES2016small$vote, ANES2016small$gender, chisq = TRUE)



# Visualisering av sammenhengen mellom kjønn og stemmegivning

# Absolutte tall:
ggplot(ANES2016small, aes(x = vote,
                          fill = gender)) + 
  geom_bar(position = "dodge") +
  labs(x = "",
       y = "Antall") +
  theme_bw()



# Andeler:
ggplot(ANES2016small, aes(x = vote,
                          group = gender)) + 
  geom_bar(aes(y = after_stat(prop)),
           position = "dodge") +
  labs(x = "",
       y = "",
       title = "Stemmegivning og kjønn") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  facet_wrap(~ gender)



## 5b. T-TEST 

# Når vi har en kategorisk og en kontinuerlig variabel

# I en t-test sammenlikner vi gjennomsnittet mellom to grupper

# Vi ser på gjennomsnittlig utdanning mellom menn og kvinner:
ess %>%
  group_by(kjonn) %>%
  summarise(utdanning_gjennomsnitt = mean(utdanning, na.rm = TRUE))

# En visualisering av forskjellen i utdanning for menn og kvinner med boksplot:

#-#


t.test(utdanning ~ as.factor(kjonn), 
       data = ess, 
       var.equal = TRUE)

# t-verdien er 2,72
# frihetsgrader er 2658
# p-verdien er 0,0064
# den alternative hypotesen (i motsetning til nullhypotesen)
# konfidensintervallet for forskjellen i gjennomsnittlig utdanning mellom menn og kvinner (0,13 til 0,79)
# gjennomsnittlig år utdanning for menn (gruppe 1): 11,75
# gjennomsnittlig år utdanning for kvinner (gruppe 2): 11,29


# Differanse mellom snittene
round(11.74582 - 11.28754, digits = 2) 

# Differansen mellom snittene faller alltid midt inne i konfidensintervallet
# Midtpunktet mellom nedre og øvre konfidensintervall 
round(sum(0.1288003, 0.7877442) / 2, digits = 2)



## Forskjellige typer t-test ##

# Enhalet test
# Tester om menn (verdi 1) har signifikant mindre utdanning enn kvinner:

#-#


# Enhalet test
# Tester om menn (verdi 1) har signifikant mer utdanning enn kvinner:
t.test(utdanning ~ as.factor(kjonn), 
       data = ess, 
       alternative = "greater",
       var.equal = TRUE)


# Tohalet test
# Tester om menn (verdi 1) har signifikant forskjellig utdanning fra kvinner:
t.test(utdanning ~ as.factor(kjonn), 
       data = ess, 
       alternative = "two.sided", # Dette er default, altså det vi kjører når vi ikke spesifiserer noe på "alternative"
       var.equal = TRUE)



## 5c. KORRELASJONSKOEFFISIENT 

# Når vi har to kontinuerlige variabler

# Sammenhengen mellom økonomisk vekst og stemmeandel til sittende presidents parti

# Laste inn FairFPSR3 
load("FairFPSR3.RData")


#-# 


# Kovarians - hvordan samvarierer variablene?

#-#


# Kovariansmatrise for alle variablene i datasettet
cov(FairFPSR3,
    use = "pairwise.complete.obs")

# Kovarians forteller oss noe om sammenhengens retning, men ikke styrke eller signifikans.


## Om vi vil finne korrelasjon og signifikans bruker vi Pearson's r 
## Pearsons r varierer mellom -1 og 1.
## 0 betyr ingen korrelasjon. -1 betyr perfekt negativ korrelasjon. 1 betyr perfekt positiv korrelasjon.

#-#


# Pearson's r med signifikans

cor.test(FairFPSR3$inc_vote, 
         FairFPSR3$growth, 
         use = "pairwise.complete.obs")


# Korrelasjonsmatrise
cor(FairFPSR3, use = "pairwise.complete.obs")



# Visualiserer sammenhengen i et spredningsplot #

ggplot(FairFPSR3, aes(x = growth, y = inc_vote)) +
  geom_point(shape = 1) +
  theme_bw() +
  labs(x = "Percentage Change in Real DGP Per Capita",
       y = "Incumbent Party Vote Percentage")


# Legger på en linje som viser sammenhengen
ggplot(FairFPSR3, aes(x = growth, y = inc_vote)) +
  geom_point(shape = 1) +
  theme_bw() +
  labs(x = "Percentage Change in Real DGP Per Capita",
       y = "Incumbent Party Vote Percentage") +
  geom_smooth(method = "lm", 
              color = "black")



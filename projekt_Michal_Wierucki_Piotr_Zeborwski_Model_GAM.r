#*****************************************************************************#
# Projekt zaliczeniowy Michal Wierucki Piotr Zebrowski                        #
# Generalized Additive Models
#*****************************************************************************#

# Zrodlo danych: https://www.kaggle.com/debajyotipodder/co2-emission-by-vehicles 

# Opis danych:

# Model
# 4WD/4X4 = Four-wheel drive
# AWD = All-wheel drive
# FFV = Flexible-fuel vehicle
# SWB = Short wheelbase
# LWB = Long wheelbase
# EWB = Extended wheelbase

# Transmission
# A = Automatic
# AM = Automated manual
# AS = Automatic with select shift
# AV = Continuously variable
# M = Manual
# 3 - 10 = Number of gears

# Fuel type
# X = Regular gasoline
# Z = Premium gasoline
# D = Diesel
# E = Ethanol (E85)
# N = Natural gas

## Zbiór danych 'CO2_Emissions_Canada.csv'

library(mgcv)

emissions <- read.csv("CO2_Emissions_Canada.csv")

colnames(emissions)

keeps <- c("CO2.Emissions.g.km.", "Engine.Size.L.", "Fuel.Consumption.Comb..L.100.km.")
DF<- data.frame(emissions)
DF[keeps]

summary(emissions)

str(emissions)

#### Model 1 z wygladzonymi zmiennymi ####
## Oszacowanie Modelu 1. Wpyw pojemnosci silnika i spalania w cyklu mieszanym w l/100 km na emisje CO2/g/km

model1 <-  gam(CO2.Emissions.g.km.  ~s(Engine.Size.L.) + s(Fuel.Consumption.Comb..L.100.km.),
               data = emissions)

## Najważniejsze statystyki podsumowujące model 1 z wygladzonymi zmiennymi
summary(model1)


## Interpretacja paramteru F mówi jaki jest wplyw danej zmiennej na zmienna objasniana:
# s(Engine.Size.L.) 234.1  
# s(Fuel.Consumption.Comb..L.100.km.) 1626.4   

# wielkosc spalania w cyklu mieszanym w l/100 km znacznie lepiej przewiduje wielkosc emisji CO2/g/km niz pojemnosc silnika


# R-sq.(adj) =  0.911 - model bardzo dobry, powyzsze zmienne wyjasniaja w 91% zmiennosc roznicy 
# w wielkosci emisji CO2/g/km

# GCV = 304.55 - srednia kwadratowa roznica - ocenia blad modelu, im mniejszy tym lepiej

gam.check(model1) # wykres swiadczy o dobrze dopasowanym modelu 


## Wykres z resztami cząstkowymi model 1
plot(model1, 
     page = 1,
     all.terms = T,
     scheme = 1,
     residuals = F)

# wykres 1 wraz ze wzrostem pojemnosci silnika rosna emisje co2/g/km

# wykres 2 wraz ze wzrostem spalania w cyklu mieszanym w l/100 rosna emisje co2/g/km

## Wartości miar dopasowania (AIC, GCV) model 1 
model1$gcv.ubre
model1$aic

#im mniejsze miary tym lepiej dopasowany model

## Oszacowany model liniowy (Model 1a) z tymi samymi zmiennymi, co w Modelu 1 

model1a <-  gam(CO2.Emissions.g.km.  ~Engine.Size.L. + Fuel.Consumption.Comb..L.100.km.,
               data = emissions)

## Najważniejsze statystyki podsumowujące model 1a
summary(model1a)

# GCV = 433.29

gam.check(model1)

## Wykres z resztami cząstkowymi dla model 1a
plot(model1a, 
     page = 1,
     all.terms = T,
     scheme = 1,
     residuals = F)

## Miary dopasowania (AIC, GCV) dla Model 1a
model1a$gcv.ubre
model1a$aic

# porownanie z modelem 1
model1$gcv.ubre
model1$aic

# model 1 z wygladzonymi zmiennymi jest lepiej dopasowany niz model, mniejsze wskazniki GCV i AIC

## Porównanie dokładności predykcji modelu liniowego (model 1) i modelu wygladzonego (model 1a) dzieląc zbiór danych
#  na próbę uczącą i testową

div<- sample(nrow(emissions), nrow(emissions)/4)

emissions.test <- emissions[div, ]
emissions.learn <- emissions[-div, ]


model1.learn <-  gam(CO2.Emissions.g.km. ~ s(Engine.Size.L.) + s(Fuel.Consumption.Comb..L.100.km.),
                     data = emissions.learn)

model1a.learn <-  gam(CO2.Emissions.g.km. ~ Engine.Size.L. + Fuel.Consumption.Comb..L.100.km.,
                      data = emissions.learn)

# porównanie srednich bledow 
mean((predict(model1.learn, newdata = emissions.test) - emissions.test$CO2.Emissions.g.km.)^2)
mean((predict(model1a.learn, newdata = emissions.test) - emissions.test$CO2.Emissions.g.km.)^2)

# porównanie srednich bledow - model z wygladzonymi zmiennymi ma mniejszy sredni blad 337.6416 
# niż model liniowy (457.0797) co swiadczy o lepszej dokladnosci predykcji modelu wygladzonego

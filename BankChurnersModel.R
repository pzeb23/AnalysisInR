# Qualitative variable modeling (classification problem)
# data source: https://www.kaggle.com/sakshigoyal7/credit-card-customers

# exploratory data alysis done additionaly in Python at: 
# https://github.com/pzeb23/Python_code/tree/master/Non-parametric_models

library(tree)
library(rpart)
library(rpart.plot)

### Załadowanie danych
setwd("~/Projects/AnalysisInR")
churners <- read.csv("BankChurners.csv")

### Podzielić dane losowo na zbiór uczący i zbiór testowy

set.seed(123)
testowe.nr <- sample(x = nrow(churners), size = nrow(churners)/4, replace = F)
#testowe.nr <- sample(x = nrow(spam), size = nrow(spam)/4, replace = F)

churners.uczacy <- churners[-testowe.nr, ]
churners.testowy <- churners[testowe.nr, ]

#spam.uczacy <- spam[-testowe.nr, ]
#spam.testowy <- spam[testowe.nr, ]

### Zbudować drzewo klasyfikacyjne (zmienna celu: Spam)

drzewo.churners0 <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy)

drzewo.churners0
summary(drzewo.churners0)
drzewo.churners0$variable.importance
rpart.rules(drzewo.churners0, style = "tallw")

# Narysować wykres drzewa

prp(drzewo.churners0)
rpart.plot(drzewo.churners0)
# Ustalić optymalne parametry modelu

drzewo.churners1 <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy,
                         control = rpart.control(cp = 0))

drzewo.churners1$cptable
printcp(drzewo.churners1)
plotcp(drzewo.churners1)

# reczne obliczanie, bierzemy wiersz z najmniejszym xerror 
# a nastepnie dodajemy xerror i xstd tego wiersza
0.3436197 + 0.01646342

# które drzewo ma xerror mniejsze od 0.3600831
# oraz ktore z nich jest najmniej złożone?

# 10 0.00500417     20   0.29191 0.35780 0.016780   <-- to nasz kandydat

# bierzemy jego CP = 0.00500417 i podstawiamy do modelu

drzewo.churners2 <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy,
                         control = rpart.control(cp = 0.00500417))

drzewo.churners2$cptable
printcp(drzewo.churners2)

# drugi raz plot cp, nadal 2 sa pod kreska
plotcp(drzewo.churners2)
# wiec mozna by jeszcze przyciac

# reczne obliczanie

# 11 0.0050042     22   0.28190 0.34445 0.016482   <-- wiersz z najmniejszym xerror

0.34445 + 0.016482

# = 0.360932  <-- który węzeł ma xerror mniejszy od tego i jest najmniej złożony?
#
# 10 0.0050042     20   0.29191 0.35363 0.016688 < -- to nasz kandydat
#
# bierzemy jego CP i podstawiamy do modelu:

drzewo.churners3 <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy,
                         control = rpart.control(cp = 0.005))

drzewo.churners3$cptable
printcp(drzewo.churners3)
plotcp(drzewo.churners3)


# Dalsze przycinanie nie przyniosło lepszych efektów
#
# 0.37531 + 0.017160

# drzewo.churners <- rpart(Attrition_Flag ~ ., 
#                          data = churners.uczacy,
#                          control = rpart.control(cp = 0.017))
# drzewo.churners$cptable
# printcp(drzewo.churners)
# plotcp(drzewo.churners)

# Odczytać ważność zmiennych

drzewo.churners3$variable.importance
cbind(drzewo.churners3$variable.importance)
dotchart(sort(drzewo.churners3$variable.importance, decreasing = F),
         pch = 16)

# Sprawdzić dokładność modelu na zbiorze testowym

predykcje.klasy <- predict(drzewo.churners3, newdata = churners.testowy,
                           type = "class")

mean(predykcje.klasy != churners.testowy$Attrition_Flag)
# 6%, jak sie przytnie to bedzie jeszcze lepiej

table(Rzeczywiste = churners.testowy$Attrition_Flag,
      Przewidywane = predykcje.klasy)

predykcje.prob <- predict(drzewo.churners3, newdata = churners.testowy,
                           type = "prob")
head(predykcje.prob)

hist(predykcje.prob[churners.testowy$Attrition_Flag == "Attrited Customer", 
                    "Attrited Customer"])


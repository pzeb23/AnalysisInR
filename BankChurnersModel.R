# m Qualitative variable modeling (classification problem)
library(tree)
library(rpart)
library(rpart.plot)

### Załadowanie danych

churners <- read.csv("BankChurners.csv")

#spam <- read.csv("/Users/macbook/Downloads/spam.RData")
### Podzielić dane losowo na zbiór uczący i zbiór testowy

set.seed(123)
testowe.nr <- sample(x = nrow(churners), size = nrow(churners)/4, replace = F)
#testowe.nr <- sample(x = nrow(spam), size = nrow(spam)/4, replace = F)

churners.uczacy <- churners[-testowe.nr, ]
churners.testowy <- churners[testowe.nr, ]

#spam.uczacy <- spam[-testowe.nr, ]
#spam.testowy <- spam[testowe.nr, ]

### Zbudować drzewo klasyfikacyjne (zmienna celu: Spam)

drzewo.churners <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy)

drzewo.churners
summary(drzewo.churners)
drzewo.churners$variable.importance
rpart.rules(drzewo.churners, style = "tallw")

# Narysować wykres drzewa

prp(drzewo.churners)
rpart.plot(drzewo.churners)
# Ustalić optymalne parametry modelu

drzewo.churners <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy,
                         control = rpart.control(cp = 0))

drzewo.churners$cptable
printcp(drzewo.churners)
plotcp(drzewo.churners)

# reczne obliczanie, bierzemy wiersz z najmniejszym xerror 
# a nastepnie dodajemy xerror i xstd tego wiersza
0.3436197 + 0.01646342

# które drzewo ma xerror mniejsze od 0.3600831
# oraz ktore z nich jest najmniej złożone?

# 10 0.00500417     20   0.29191 0.35780 0.016780   <-- to nasz kandydat

# bierzemy jego CP = 0.00500417 i podstawiamy do modelu

drzewo.churners <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy,
                         control = rpart.control(cp = 0.00500417))

drzewo.churners$cptable
printcp(drzewo.churners)

# drugi raz plot cp, nadal 2 sa pod kreska
plotcp(drzewo.churners)
# wiec mozna by jeszcze przyciac

# reczne obliczanie

# 11 0.0050042     22   0.28190 0.34445 0.016482   <-- wiersz z najmniejszym xerror

0.34445 + 0.016482

# = 0.360932  <-- który węzeł ma xerror mniejszy od tego i jest najmniej złożony?
#
# 10 0.0050042     20   0.29191 0.35363 0.016688 < -- to nasz kandydat
#
# bierzemy jego CP i podstawiamy do modelu:

drzewo.churners <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy,
                         control = rpart.control(cp = 0.005))

drzewo.churners$cptable
printcp(drzewo.churners)
plotcp(drzewo.churners)


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

drzewo.churners$variable.importance
cbind(drzewo.churners$variable.importance)
dotchart(sort(drzewo.churners$variable.importance, decreasing = F),
         pch = 16)

# Sprawdzić dokładność modelu na zbiorze testowym

predykcje.klasy <- predict(drzewo.churners, newdata = churners.testowy,
                           type = "class")

mean(predykcje.klasy != churners.testowy$Attrition_Flag)
# 6%, jak sie przytnie to bedzie jeszcze lepiej

table(Rzeczywiste = churners.testowy$Attrition_Flag,
      Przewidywane = predykcje.klasy)

predykcje.prob <- predict(drzewo.churners, newdata = churners.testowy,
                           type = "prob")
head(predykcje.prob)

hist(predykcje.prob[churners.testowy$Attrition_Flag == "Attrited Customer", 
                    "Attrited Customer"])

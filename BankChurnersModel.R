# Qualitative variable modeling (classification problem)
# data source: https://www.kaggle.com/sakshigoyal7/credit-card-customers

# exploratory data alysis done additionaly in Python at: 
# https://github.com/pzeb23/Python_code/tree/master/Non-parametric_models

library(tree)
library(rpart)
library(rpart.plot)

### Loading dataset
setwd("~/Projects/AnalysisInR")
churners <- read.csv("BankChurners.csv")

### Split the data for test and train subsets

set.seed(123)
testowe.nr <- sample(x = nrow(churners), size = nrow(churners)/4, replace = F)
#testowe.nr <- sample(x = nrow(spam), size = nrow(spam)/4, replace = F)

churners.uczacy <- churners[-testowe.nr, ]
churners.testowy <- churners[testowe.nr, ]

#spam.uczacy <- spam[-testowe.nr, ]
#spam.testowy <- spam[testowe.nr, ]

### Build classification tree (target variable: Spam)

drzewo.churners0 <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy)

drzewo.churners0
summary(drzewo.churners0)
drzewo.churners0$variable.importance
rpart.rules(drzewo.churners0, style = "tallw")

# Drawing tree chart

prp(drzewo.churners0)
rpart.plot(drzewo.churners0)
# Setting optimal model parameters

drzewo.churners1 <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy,
                         control = rpart.control(cp = 0))

drzewo.churners1$cptable
printcp(drzewo.churners1)
plotcp(drzewo.churners1)

# Manual calculation, take row with the lowest xerror
# and adding xerror and xstd of this row
0.3436197 + 0.01646342

# which tree has xerror less than 0.3600831
# and which of them is the least complex?

# 10 0.00500417     20   0.29191 0.35780 0.016780   <-- our cadidate

# take it's CP = 0.00500417 and put into our model

drzewo.churners2 <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy,
                         control = rpart.control(cp = 0.00500417))

drzewo.churners2$cptable
printcp(drzewo.churners2)

# again plot cp, there's still 2 below the line
plotcp(drzewo.churners2)
# so we can cut once more

# manual calculation

# 11 0.0050042     22   0.28190 0.34445 0.016482   <-- row with the lowest xerror

0.34445 + 0.016482

# = 0.360932  <-- which node has xerror lower than this and is the least complex?
#
# 10 0.0050042     20   0.29191 0.35363 0.016688 < -- our candidate
#
# take it's CP and put into our model:

drzewo.churners3 <- rpart(Attrition_Flag ~ ., 
                         data = churners.uczacy,
                         control = rpart.control(cp = 0.005))

drzewo.churners3$cptable
printcp(drzewo.churners3)
plotcp(drzewo.churners3)


# Further trimming did not pring better results
#
# 0.37531 + 0.017160

# drzewo.churners <- rpart(Attrition_Flag ~ ., 
#                          data = churners.uczacy,
#                          control = rpart.control(cp = 0.017))
# drzewo.churners$cptable
# printcp(drzewo.churners)
# plotcp(drzewo.churners)

# Read variable values:

drzewo.churners3$variable.importance
cbind(drzewo.churners3$variable.importance)
dotchart(sort(drzewo.churners3$variable.importance, decreasing = F),
         pch = 16)

# Check model accuracy on test dataset

predykcje.klasy <- predict(drzewo.churners3, newdata = churners.testowy,
                           type = "class")

mean(predykcje.klasy != churners.testowy$Attrition_Flag)
# 6%, after trimming it could be better

table(Rzeczywiste = churners.testowy$Attrition_Flag,
      Przewidywane = predykcje.klasy)

predykcje.prob <- predict(drzewo.churners3, newdata = churners.testowy,
                           type = "prob")
head(predykcje.prob)

hist(predykcje.prob[churners.testowy$Attrition_Flag == "Attrited Customer", 
                    "Attrited Customer"])


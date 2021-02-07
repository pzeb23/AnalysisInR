# m Qualitative variable modeling (classification problem)
library(tree)
library(rpart)
library(rpart.plot)

churners <- read.csv("BankChurners.csv")

table(churners$Attrition_Flag)

drzewo <- tree(Attrition_Flag ~., 
               data = churners,
               split = "deviance")

drzewo
summary(drzewo)

plot(drzewo) # same gałęzie
text(drzewo)

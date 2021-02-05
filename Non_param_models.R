# m Qualitative variable modeling (classification problem)
library(tree)
library(rpart)
library(rpart.plot)

myData1 <- read.csv("/Users/macbook/Projects/AnalysisInR/BankChurners.csv")

table(myData1$Attrition_Flag)

drzewo <- tree(Attrition_Flag ~., 
               data = myData1,
               split = "deviance")

drzewo
summary(drzewo)

plot(drzewo) # same gałęzie
text(drzewo)
library(arules)
data(Groceries)

#plot the data
itemFrequencyPlot(Groceries, support = 0.1)
itemFrequencyPlot(Groceries, topN = 20)
# apriori algo
groceryrules <- apriori(Groceries, parameter = list(support = 0.006, confidence = 0.30, minlen = 2 ))

sortedSupport <- groceryrules[order(groceryrules@quality$support, decreasing = TRUE),]
inspect(sortedSupport[1:5])

sortedConfidence <- groceryrules[order(groceryrules@quality$confidence, decreasing = TRUE),]
inspect(sortedConfidence[1:5])

sortedLift<- groceryrules[order(groceryrules@quality$lift, decreasing = TRUE),]
inspect(sortedLift[1:5])

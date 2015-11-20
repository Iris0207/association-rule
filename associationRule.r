library("arules")
groceries <- read.transactions("groceries.csv", format="basket", sep=",") 
summary(groceries)
class(groceries)
colnames(groceries)
basketSize<-size(groceries) 
itemFreq <- itemFrequency(groceries)
itemCount <- (itemFreq/sum(itemFreq))*sum(basketSize)

orderedItem <- sort(itemCount, decreasing =T)  #T means reserver order
orderedItemFreq <- sort(itemFrequency(groceries), decreasing=T) 
orderedItemFreq[1:5]
itemFrequencyPlot(groceries, support=0.1,horiz=T)  ##show the item whose support >0.1
itemFrequencyPlot(groceries, topN=10)

groceries_use <- groceries[basketSize >1]
dim(groceries_use)

inspect(groceries[1:5])  #scan the items
image(groceries[2]) 

library(arulesViz) # data visualization of association rules
library(RColorBrewer)  # color palettes for plots

##Apriori

groceryrules <- apriori(groceries, parameter = list(support =0.006, confidence = 0.25, minlen = 2))  

# minlen and maxlen mean the number of items of RHS+LHS
summary(groceryrules)
groceryrules[1]
##use inspect to see the rules
inspect(groceryrules[1:3])

ordered_groceryrules <- sort(groceryrules, by="lift",)  
inspect(ordered_groceryrules[1:5])  

yogurtrules <- subset(groceryrules, items %in% "yogurt")  
inspect(yogurtrules)

 berriesInLHS <- apriori(groceries, parameter = list( support = 0.001, confidence = 0.1 ), appearance = list(lhs = c("berries"), default="rhs")) 
inspect(berriesInLHS)
inspect(head(rhs(berriesInLHS), n=5))  

berrySub <- subset(berriesInLHS, subset = !(rhs %in% c("root vegetables", "whole milk")))  
 inspect(head(rhs(sort(berrySub, by="confidence")), n=5))  

##SAve the results
write(groceryrules, file="groceryrules.csv", sep=",", quote=TRUE, row.names=FALSE)
groceryrules_df <- as(groceryrules, "data.frame")  
str(groceryrules_df)


##Advanced parts
data(Groceries)
itemInfo(Groceries)
inspect(Groceries[1:3]) 
groceries <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])
inspect(groceries[1:3])  

itemFrequencyPlot(Groceries, support = 0.025, cex.names=0.8, xlim = c(0,0.3),  
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,  
                  xlab = paste("Proportion of Market Baskets Containing Item",  
                               "(Item Relative Frequency or Support)"))  
second.rules <- apriori(groceries,parameter = list(support = 0.025, confidence = 0.05))  

 
###
library("grid")
library("arulesViz")
library("RColorBrewer")
####
plot(second.rules,control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),shading = "lift")   
##shading seems no use
plot(second.rules, method="grouped",     
     control=list(col = rev(brewer.pal(9, "Greens")[4:9])))

top.rules <- head(sort(second.rules, decreasing = TRUE, by = "lift"), 10)
inspect(top.rules)
plot(sort(second.rules, decreasing = TRUE, by = "lift")[1:10], measure="confidence", method="graph",   
           control=list(type="items"),   
           shading = "lift")
top=sort(second.rules, decreasing = TRUE, by = "lift")[1:10]
inspect(top)

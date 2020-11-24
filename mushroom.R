library(arules)
library(arulesViz)

# Load Mushroom Dataset
url <- "https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Second-Edition/master/Chapter%2005/mushrooms.csv"
mushrooms <- read.csv(file = url, header = T, stringsAsFactors = T)
head(mushrooms)

trans <- as(mushrooms, "transactions")
inspect(trans[1:5])

# Frequent Itemset Generation
all.itemsets <- apriori(mushrooms, parameter=list(target="frequent"))
inspect(all.itemsets[1:15])
summary(all.itemsets)

# Generate sample association rules
rules1<- apriori(mushrooms, parameter=list(target="rules",  minlen=1, maxlen=5))
summary(rules1)
inspect(head(rules1, 15))

# Filter redundant rules. This can take ~5-10 minutes to completely.
subset.rules1 <- which(colSums(is.subset(rules1, rules1)) > 1) # get subset rules in vector
pruned.rules1 <- rules1[-subset.rules1]
length(pruned.rules1)

# Plot rules: confidence vs. support
plot(pruned.rules1)

# Generate association rules to determine edibility
edibility.rules <- apriori(mushrooms, parameter=list(target="rules", maxlen=5, confidence=1), appearance = list(rhs=c("type=edible", "type=poisonous")))
summary(edibility.rules)
inspect(head(edibility.rules, 20))

# Filter redundant rules. This will take ~5-10 minutes to completely.
subset.rules <- which(colSums(is.subset(apriori.rules, apriori.rules)) > 1) # get subset rules in vector
pruned.rules <- apriori.rules[-subset.rules]
inspect(head(pruned.rules[1:15], 15))

# Inspect rules and sort by lift values
inspect(sort(pruned.rules, by="lift"))

# Plot rules
plot(pruned.rules)

# Graph subset of rules
plot(pruned.rules[1:15], method="graph")

# Plot all rules (cuts off at 100)
plot(pruned.rules, method="graph")

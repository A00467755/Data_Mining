
library(plyr)

df_user = read.csv("D:/#Spring 2023/5580 - Text Mining/Assignment3/userDistinctMilestoneDec151 (1).csv")

# Transpose source
df_user= ddply(df_user,c("id"),function(dfl)paste(dfl$milestone, collapse=","))

# Remove id field
df_user$id = NULL

# Write to temp file
write.table(df_user,"D:/#Spring 2023/5580 - Text Mining/Assignment3/milestone2.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)


#install.packages("arules")
library("arules")

# Read temp file
tr = read.transactions("D:/#Spring 2023/5580 - Text Mining/Assignment3/milestone2.csv",format="basket",sep=",")

# Plot Frequent items
#summary(tr)
itemFrequencyPlot(tr, topN=10)

# List the rules
rules<- apriori(tr, parameter= list(supp=0.3, conf=0.5))

inspect(sort(rules,by='lift')[1:15])

# List item set 
itemsets=unique(generatingItemsets(rules))
inspect(sort(itemsets,by='support'))

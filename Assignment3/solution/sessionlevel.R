
library(plyr)
library(arules)
library(arulesViz)

df_session = read.csv("D:/#Spring 2023/5580 - Text Mining/Assignment3/sessionDistinctMilestoneDec15 (2).csv")

df_session$id = paste(as.character(df_session$user_id) , as.POSIXct(df_session$date, format="%Y-%m-%d"))


# Transpose source
df_session= ddply(df_session,c("id"),function(dfl)paste(dfl$milestone_name, collapse=","))

# Remove id field
df_session$id = NULL
df_session$date = NULL
df_session$user_id = NULL

# Write to temp file
write.table(df_session,"D:/#Spring 2023/5580 - Text Mining/Assignment3/session2.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)

# Read temp file
tr = read.transactions("D:/#Spring 2023/5580 - Text Mining/Assignment3/session2.csv",format="basket",sep=",")

# Plot Frequent items
#summary(tr)
itemFrequencyPlot(tr, topN=10)

# List the rules
rules<- apriori(tr, parameter= list(supp=0.25, conf=0.5))

inspect(sort(rules,by='lift'))

# Visualize the rules
plot(rules,
     method="graph",
     engine="visNetwork")

# List item set 
itemsets=unique(generatingItemsets(rules))
inspect(sort(itemsets,by='support'))




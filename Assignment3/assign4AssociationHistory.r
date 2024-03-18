	Please use this as a guideline instead of following it literally.
	With changing datasets, the numbers may be off.
	Using your judgement instead of blindly following instructions is
	an important trait.
	Creativity with justification is encouraged.

User Based Association
SQL commands:
To get frequently used milestones:

SELECT `milestone_name`, count(*) as frequency FROM `rawdataDec15`
group by milestone_name order by frequency desc

Getting list of features for each user:
create table userMilestoneDec15 as
SELECT user_id, milestone_name FROM `rawdataDec15` order by user_id,milestone

create table userDistinctMilestoneDec15 as select distinct * from userMilestoneDec15

Having difficulty with phymyadmin with export: downloading used command line:
mysql -u cs4477200 -p < temp.sql > userDistinctMilestoneDec15.txt
temp.sql:  select * from cs4477203.userDistinctMilestoneDec15;
vi the file convert tabs to spaces, delete column names

Create file for association
java flattenForRassociation userDistinctMilestoneDec15.txt | \
grep ',' > userFeatureAssociationRDec15.csv

R commands:
install.packages("arules")
library("arules")
tr=read.transactions("userFeatureAssociationRDec15.csv",format="basket",sep=",")
rules<- apriori(tr, parameter= list(supp=0.4, conf=0.5))
itemsets=unique(generatingItemsets(rules))
write(rules,file=”userRules.txt”)
write(itemsets)

#To get maximal sets
maximal.sets<- apriori(tr, parameter= list(supp=0.4, conf=0.5, target="maximally frequent itemsets"))
write(maximal.sets)

Miscellaneous vi command to get precison of 2
%s/\([0,1]\)\.\([0-9][0-9]\)[0-9][0-9]*/\1\.\2/g
</pre>

<b>Session based Association</b>
<pre>
SQL commands:
Getting list of features for each session:
create table sessionMilestoneDec15 as
SELECT user_id,date, milestone_name FROM `rawdataDec15` order by user_id,date,milestone_name

create table sessionDistinctMilestoneDec15 as select distinct * from sessionMilestoneDec15

Having difficulty with phymyadmin with export: downloading used command line:
mysql -u cs4477200 -p < temp.sql > sessionDistinctMilestoneDec15.txt
temp.sql:  select * from cs4477203.sessionDistinctMilestoneDec15;
Tricky processing
vi the file
delete column names
convert first tab to -, so user-id and date are collapsed into single key.
change second tab to space

Create file for association
java flattenForRassociation sessionDistinctMilestoneDec15.txt | \
grep ',' > sessionFeatureAssociationRDec15.csv

R commands:
install.packages("arules")
library("arules")
tr=read.transactions("sessionFeatureAssociationRDec15.csv",format="basket",sep=",")
#Note had to use lower support level
rules<- apriori(tr, parameter= list(supp=0.2, conf=0.5))
itemsets=unique(generatingItemsets(rules))
write(rules,file=”sessionRules.txt”)
write(itemsets)

#To get maximal sets
maximal.sets<- apriori(tr, parameter= list(supp=0.2, conf=0.5, target="maximally frequent itemsets"))
write(maximal.sets)

Miscellaneous vi command to get precios of 2
%s/\([0,1]\)\.\([0-9][0-9]\)[0-9][0-9]*/\1\.\2/g

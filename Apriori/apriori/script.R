library(arules)    

train<-read.csv("data/MBA_data_new.csv")
#We cannot directly use imported data to run apriori algorithm.
#We need to aggregate it first by customer id and transform into different format.
trans <- split(train$Products, train$Customer_Id,"transactions")

#We have transformed the data into the desired format to run the apriori algorithm.
#In order to run apriori algorithm, first,
#we need to install and load arules library package using below code.
rules = apriori(trans, parameter=list(support=0.10, confidence=0.5,maxlen=2,minlen=2))

inspect(rules)        # to get the rules 
write.csv(rules,file="mba_rules1.csv",sep=",",row.names = FALSE)
# Support(Bread) = 50% and Support(Butter) =40%
#   
# Support(Bread, Butter) =50/200 = 25%
# 
# Confidence (Bread, Butter)=50%.

# Percentage of customers who buy Butter only = 40%
# 
# Percentage of confidence of buying butter if someone buys bread = 50%
# 
# So basically, given we bundle up Bread and Butter together,
# 
# Percentage increase = ((0.50/0.40) - 1)  = 25% or
# 
# Value of the lift would be 1.25(.50/.40) but
#we can interpret it that chance of buying butter has been increased by 25%.

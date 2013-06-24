
setwd('/home/gaurav/Downloads')
# Load all data 
Alldata <-read.csv('train_50k.csv')

#split data into training and test sets 
train.idx <- sample(1:nrow(Alldata), .75*nrow(Alldata))
trainingset <- Alldata[train.idx,]
testset <- Alldata[-train.idx,]

# create a linear regression model 
model1<-lm(SalaryNormalized ~ Category +ContractType+ContractTime , data=trainingset)
summary(model1)

# MEAN ABSOLUTE ERROR
mae <- function(x,y)
{
  sum( abs(x-y) ) /length(x)
}
    
# MEAN SQUARED ERROR
mse <- function(x,y)
{
      sum( (x-y)^2 ) /length(x)
}

#making the prediction 
test.predict1 <- predict(model1, testset)


# How good is the prediction
mae(test.predict1, testset$SalaryNormalized)
mse(test.predict1, testset$SalaryNormalized)

## some cleaning up before using read.csv . 
##sed 's/"//g' Location_Tree.txt > Location_Tree_new.txt
##sed 's/~/,/g' Location_Tree_new.txt > Location_Tree_newer.txt
##awk -F',' 'NF == 3 {print $0 }' Location_Tree_newer.txt > Location_Tree_newest.txt

location<-read.csv(file='Location_Tree_newest.txt' ,fill=T , sep = ',',col.names=c('loc1' ,'loc2' ,'loc3' ))
Alldata$Loc3 <-toupper(lapply(strsplit(as.character(Alldata$LocationRaw),","),'[',2))
Alldata$Loc3<-gsub(' ','',Alldata$Loc3)
location$loc1<-toupper(location$loc1)
location$loc2<-toupper(location$loc2)
location$loc3<-toupper(location$loc3)
Loc_Alldata<- merge(location,Alldata,by, by.x="loc3", by.y="Loc3",all.y=TRUE )

#split data into training and test sets 

trainingset <- Loc_Alldata[train.idx,]
testset <- Loc_Alldata[-train.idx,]


model2<-lm(SalaryNormalized ~ Category +ContractType+ContractTime +loc2, data=trainingset)
summary(model2)

#making the prediction 
test.predict2 <- predict(model2, testset)
test.predict2<- test.predict2[!is.na(test.predict2)]


##how to get rid of NAs
# How good is the prediction
mae(test.predict2, testset$SalaryNormalized)
mse(test.predict2, testset$SalaryNormalized)


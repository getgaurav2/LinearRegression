
setwd('/home/gaurav/Downloads')
# Load all data
Alldata <-read.csv('train_50k.csv')
#Alldata <-read.csv('train.csv')


#split data into training and test sets
train.idx <- sample(1:nrow(Alldata), .85*nrow(Alldata))



trainingset <- Alldata[train.idx,]
testset <- Alldata[-train.idx,]

# create a linear regression model
model1<-lm(SalaryNormalized ~ Category +ContractType+ContractTime , data=trainingset)
summary(model1) ##--Multiple R-squared: 0.1919,

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
mae(test.predict1, testset$SalaryNormalized) ## 11637.72
mse(test.predict1, testset$SalaryNormalized) ## 254049834


set.seed(42)
Alldata$fold <- sample(1:10, nrow(Alldata), replace=TRUE)

error_from_fold <- function(n) {
  model<-lm(SalaryNormalized ~ Category +ContractType+ContractTime , data=subset(Alldata, fold != n))
  test <- subset(Alldata, fold == n)
  error <- mae(predict(model, testset), testset$SalaryNormalized)
  return(error)
}

sapply(1:10, error_from_fold)

mean(sapply(1:10, error_from_fold)) ## 11665



#################USING THE LOCATION TREE ############################################################################################################

## some cleaning up before using read.csv .
##sed 's/"//g' Location_Tree.txt > Location_Tree_new.txt
##sed 's/~/,/g' Location_Tree_new.txt > Location_Tree_newer.txt

## keep only records that have 3 columns in the tree
##awk -F',' 'NF == 3 {print $0 }' Location_Tree_newer.txt > Location_Tree_newest.txt

# merge the the Dataset on the 2nd value in the LocationRaw column
location<-read.csv(file='Location_Tree_newest.txt' ,fill=T , sep = ',',col.names=c('loc1' ,'loc2' ,'loc3' ))
Alldata$Loc3 <-toupper(lapply(strsplit(as.character(Alldata$LocationRaw),","),'[',2))
Alldata$Loc3<-gsub(' ','',Alldata$Loc3)
location$loc1<-toupper(location$loc1)
location$loc2<-toupper(location$loc2)
location$loc3<-toupper(location$loc3)
Loc_Alldata<- merge(location,Alldata,by, by.x="loc3", by.y="Loc3",all.y=TRUE )
Loc_Alldata$loc2[is.na(Loc_Alldata$loc2)]<-Loc_Alldata$loc3[is.na(Loc_Alldata$loc2)]
## default the NA's to UK
Loc_Alldata$loc2[is.na(Loc_Alldata$loc1)]<-'UK'

## getting rid of two categories ..some how I never find them in the test set when i go to predict
Loc_Alldata<-Loc_Alldata[!grepl('Maintenance Jobs',Loc_Alldata$Category),]
Loc_Alldata<-Loc_Alldata[!grepl('Domestic help & Cleaning Jobs',Loc_Alldata$Category),]

#split data into training and test sets
trainingset <- Loc_Alldata[train.idx,]
testset <- Loc_Alldata[-train.idx,]


model2<-lm(SalaryNormalized ~ Category +ContractType+ContractTime + loc2, data=trainingset)
summary(model2) 


#making the prediction
test.predict2 <- predict(model2, testset)



# How good is the prediction
mae(test.predict2, testset$SalaryNormalized) # 11604.69
mse(test.predict2, testset$SalaryNormalized) #246348345

# Randomly select fold assignments for n-fold cross-validation
set.seed(42)
Loc_Alldata$fold <- sample(1:10, nrow(Loc_Alldata), replace=TRUE)

error_from_fold <- function(n) {
   model<-lm(SalaryNormalized ~ Category +ContractType+ContractTime +loc2, data=subset(Loc_Alldata, fold != n))
  test <- subset(Loc_Alldata, fold == n)
  error <- mae(predict(model, testset), testset$SalaryNormalized)
  return(error)
}

sapply(1:10, error_from_fold)

mean(sapply(1:10, error_from_fold)) ## 11647.83

####################################################################################
install.packages("glmnet")
library(glmnet)

##3) WHAT IS WRONG WITH GLMNET FUNCTION
model3 <- cv.glmnet(model.matrix(~trainingset$Category), as.matrix(trainingset$SalaryNormalized))

as.vector(predict(model, model.matrix(~test$ContractType), s="lambda.min"))





###Initialization
library(rpart)
library(rpart.plot)
library(caTools)
set.seed(123)

###Load the data
###Data is taken from https://www.kaggle.com/uciml/mushroom-classification/version/1

x=read.csv("mushrooms.csv")
ind=sample.split(x$class,SplitRatio = 0.7)
train=x[which(ind==T),] #70% of data is used for training
test=x[which(ind==F),]  #30% of data is used for testing

###Train the decision tree model
mod=rpart(class~.,train)

###Predict test data using the trained model
prd=predict(mod,test[,-1])
prd=round(prd)
prd=prd[,-2]

###Transform test[,1] into a binary variable, 1<->e,0<->p
real=array(0,length(prd))
for(i in 1:length(real)){
if(test[i,1]=="e"){
  real[i]=1} else{
  real[i]=0
  }
}

###Confusion matrix calculation. Looking good :)
table(predicted=prd,reality=real)

###Visualization of the tree
pdf("Mushrooms_Results.pdf")
rpart.plot::prp(mod)
rpart.plot::rpart.plot(mod)
dev.off()


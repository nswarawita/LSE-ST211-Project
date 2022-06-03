library(arm)
library(car)
library(dplyr)
library(forcats)  
library(ggplot2)
library(gridExtra)
library(tidyr)

data.1<-read.csv("EOTST2112020.csv", header = T)

head(data.1)
summary(data.1)
str(data.1)
dim(data.1)

#Delete NSID, W4Childck1YP, W6Childliv, W6NEETAct, W8DAGEYCH and W8PUSA
data.1[,c(1,43,55,57,61,70)]
data.2<-data.1[,-c(1,43,55,57,61,70)]
summary(data.2)
str(data.2)
dim(data.2)


#Logistic regression to predict W8QMAFI

sort(table(data.2$W8QMAFI))

#Remove missing rows in W8QMAFI 
missing <- (which(data.2$W8QMAFI<0))
data.3<-data.2[-missing,]
dim(data.3)
#5656 rows

#Convert W8QMAFI into a binary variable.
data.3$W8QMAFI<-with(data.3,Recode(W8QMAFI,"c('1','2')='1'")) #Well
data.3$W8QMAFI<-with(data.3,Recode(W8QMAFI,"c('5','4','3')='0 '")) #Poorly
str(data.3)

#Delete predictors heavily overlapping with others
#W1GrssyrMP, W8DACTIVITY, W8GROW, W8NETW 
summary(data.3[,c(1,62,63,64)])
data.5 <- data.3[,-c(1,62,63,64)] 
summary(data.5)

#Removing missing rows in the continuous predictors
missing_continuous<-c(which(data.5$W1GrssyrHH<0),
                      which(data.5$W1yschat1<0),which(data.5$W2ghq12scr<0),
                      which(data.5$W4schatYP<0),which(data.5$W6DebtattYP<0),
                      which(data.5$W8DINCW<0),which(data.5$W8DGHQSC<0),which(data.5$W8NETA<0),which(data.5$W8QDEB2<0))
data.6<-data.5[-missing_continuous,]
dim(data.6)
#1222 rows
summary(data.6)

#Merge the missing values into one level for categorical predictors
data_index.1 <- c(1:63)
for(i in data_index.1){data.6[,i] <- ifelse(data.6[,i]<0,"missing",data.6[,i])}

dim(data.6)

#Centering continuous predictors where necessary
summary(data.6[,c(1, 31, 32, 40,52,53,55,61,62)])
data.6$cent.W1GrssyrHH<-with(data.6,W1GrssyrHH-mean(W1GrssyrHH))
data.6$cent.W8DINCW<-with(data.6,W8DINCW-mean(W8DINCW))
summary(data.6[,c(1,53)])
data.7<-data.6[,-c(1,53)]
dim(data.7)

#Code the catagorical predictors as factors
data_index.2<-c(1:63) 
summary(data.7[,c(30, 31, 39,51,53,59,60,62,63)])
categorical_index<-data_index.2[-c(30, 31, 39,51,53,59,60,62,63)]
for (i in categorical_index){data.7[,i]<-as.factor(data.7[,i])}
summary(data.7)
str(data.7)
dim(data.7)



#Model 1 (62 predictors)
W8QMAFI.glm.1 <- glm(W8QMAFI~., data = data.7, family = binomial(link = "logit"))
summary(W8QMAFI.glm.1)
Anova(W8QMAFI.glm.1)

#Didn't converge
glm.control()
#Max iterations were 25.  Increased this to 100.

W8QMAFI.glm.1 <- glm(W8QMAFI~., data = data.7, family = binomial(link = "logit"),
                     control = list(maxit = 100, epsilon=1e-08)   )
Anova(W8QMAFI.glm.1)
# Remove all predictors that aren't significant at a 5% level



#Model 2 (7 predictors)
#Removing missing rows of continuous predictors
missing_continuous.2<-c(which(data.5$W8DGHQSC<0),which(data.5$W8NETA<0))
data.8<-data.5[-missing_continuous.2,]

dim(data.8)
#4141 rows

#Merge the missing values into one level for categorical predictors
data_index.1 <- c(1:63)
summary(data.8[,c(1, 31, 32, 40,52,53,55,61,62)])
categorical_index<-data_index.2[-c(1, 31, 32, 40,52,53,55,61,62)]
for(i in data_index.1){data.8[,i] <- ifelse(data.8[,i]<0,"missing",data.8[,i])}

dim(data.8)
summary(data.8)

#Code the catagorical predictors as factors
data_index.2<-c(1:63) 
summary(data.8[,c(1, 31, 32, 40,52,53,55,61,62)])
categorical_index<-data_index.2[-c(1, 31, 32, 40,52,53,55,61,62)]
for (i in categorical_index){data.8[,i]<-as.factor(data.8[,i])}
summary(data.8)
str(data.8)
dim(data.8)

data.8$W1empsmum<-relevel(data.8$W1empsmum,ref = "2")
data.8$W4NamesYP<-relevel(data.8$W4NamesYP,ref = "2")
data.8$W4empsYP<-relevel(data.8$W4empsYP,ref = "5")
data.4$W8TENURE<-relevel(data.8$W8TENURE,ref = "4")


W8QMAFI.glm.2 <- glm(W8QMAFI~W1wrk1aMP+W1empsmum+W4NamesYP+
                       W4empsYP+W8DGHQSC+
                       W8TENURE+W8NETA,data = data.8, family = binomial(link = "logit"),
                     control = list(maxit = 100, epsilon=1e-08))
display(W8QMAFI.glm.2 )
summary(W8QMAFI.glm.2)
Anova(W8QMAFI.glm.2)

#Create a function to create a classification table for model 2

ct.op<-function(predicted,observed){ #arguments
  #create the data frame  
  df.op<-data.frame(predicted=predicted,observed=observed)
  #create a table 
  op.tab<-table(df.op)
  #use the prop.table function to obtain the proportions we need:
  #those who were correctly predicted as 0 
  #@position 1,1 in the table of proportions
  obs0.tab<-round(prop.table(op.tab,2)[1,1],2)
  #those who were correctly predicted as 1
  #@position 2,2 in the table of proportions
  obs1.tab<-round(prop.table(op.tab,2)[2,2],2)
  #and put them under the table 
  op.tab<-rbind(op.tab,c(obs0.tab,obs1.tab))
  #name the rows
  rownames(op.tab)<-c("pred=0","pred=1","%corr")
  #name the columns
  colnames(op.tab)<-c("obs=0","obs=1")
  #return the table
  op.tab
}

pred.W8QMAFI<-as.numeric(W8QMAFI.glm.2$fitted.values>0.5)
#pass the fitted values and the observed values to ct.op
ct.op(pred.W8QMAFI,data.8$W8QMAFI)
#79.35%

#Remove W1empsmum as it is not significant at the 5% level anymore



#Model 3 (6 Predictors)
W8QMAFI.glm.3 <- glm(W8QMAFI~W1wrk1aMP+W4NamesYP+
                       W4empsYP+W8DGHQSC+
                       W8TENURE+W8NETA,data = data.8, family = binomial(link = "logit"),
                     control = list(maxit = 100, epsilon=1e-08))
display(W8QMAFI.glm.3)
summary(W8QMAFI.glm.3)
Anova(W8QMAFI.glm.3)


#Model 3 is nested in 2 so compare them
anova(W8QMAFI.glm.3,W8QMAFI.glm.2,test  ="Chisq")
qchisq(0.95, 1)


#Renaming the levels of continuous predictors 
summary(data.8$W4NamesYP)

#W1wrk1aMP
summary(data.8$W1wrk1aMP)
data.8$W1wrk1aMP.1<- factor(data.8$W1wrk1aMP,
                            levels = c("1","2","3","4","5","6","7","8",
                                       "9","10","11","12","missing"),
                            labels = c("FT Paid", "PT Paid","FT Self employed",
                                       "PT Self employed","Unemployed","FT Education",
                                       "Government scheme for employment training","Temporarily sick/ disabled","Permanently sick/ disabled",
                                       "Looking after family","Retired","Other","Missing"))
summary(data.8$W1wrk1aMP.1)

#W4NamesYP
summary(data.8$W4NamesYP)
data.8$W4NamesYP.1 <- factor(data.8$W4NamesYP,
                             levels = c("2","1","missing"),
                             labels = c("No ", "Yes","Missing"))
summary(data.8$W4NamesYP.1)

#W4empsYP
summary(data.8$W4empsYP)
data.8$W4empsYP.1 <- factor(data.8$W4empsYP,
                            levels = c("5","1","2","3","4","6","9","missing"),
                            labels = c("FT Education", "Paid work >30","Paid work <30",
                                       "Unemployed","Training course","Looking after family",
                                       "Other","Missing"))
summary(data.8$W4empsYP.1)

#W8TENURE
summary(data.8$W8TENURE)
data.8$W8TENURE.1 <- factor(data.8$W8TENURE,
                            levels = c("4","1","2","3","5","7","missing"),
                            labels = c("Rent including housing benefits ", "Owned outright","Bought on a mortgage",
                                       "Part rent/mortgage","Rent free","Other","Missing"))
summary(data.8$W8TENURE.1)


W8QMAFI.glm.3 <- glm(W8QMAFI~W1wrk1aMP.1+W4NamesYP.1+
                       W4empsYP.1+W8DGHQSC+
                       W8TENURE.1+W8NETA,data = data.8, family = binomial(link = "logit"))
display(W8QMAFI.glm.3 )
summary(W8QMAFI.glm.3)
Anova(W8QMAFI.glm.3)

qchisq(0.95,6)

summary(data.8$W8QMAFI)

#Create a visual representation of the logistic model
predicted.data <- data.frame(
  probability.of.W8QMAFI=W8QMAFI.glm.3$fitted.values,
  W8QMAFI=data.8$W8QMAFI)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.W8QMAFI, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)


ggplot(data=predicted.data, aes(x=rank, y=probability.of.W8QMAFI)) +
  geom_point(aes(color=W8QMAFI), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of W8QMAFI")



#Create a function to create a classification table for model 3
ct.op<-function(predicted,observed){ #arguments
  #create the data frame  
  df.op<-data.frame(predicted=predicted,observed=observed)
  #create a table 
  op.tab<-table(df.op)
  #use the prop.table function to obtain the proportions we need:
  #those who were correctly predicted as 0 
  #@position 1,1 in the table of proportions
  obs0.tab<-round(prop.table(op.tab,2)[1,1],2)
  #those who were correctly predicted as 1
  #@position 2,2 in the table of proportions
  obs1.tab<-round(prop.table(op.tab,2)[2,2],2)
  #and put them under the table 
  op.tab<-rbind(op.tab,c(obs0.tab,obs1.tab))
  #name the rows
  rownames(op.tab)<-c("pred=0","pred=1","%corr")
  #name the columns
  colnames(op.tab)<-c("obs=0","obs=1")
  #return the table
  op.tab
}

pred.W8QMAFI<-as.numeric(W8QMAFI.glm.3$fitted.values>0.5)
#pass the fitted values and the observed values to ct.op
ct.op(pred.W8QMAFI,data.8$W8QMAFI)

#79.43%









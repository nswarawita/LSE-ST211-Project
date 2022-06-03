#EOT Project MLR2

library(arm)
library(car)
library(dplyr)
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
#5792 rows


##MLR to predict W8QDEB2

#Reformat data

#For continuous predictors - Remove rows with missing data
missing_continuous<-c(which(data.2$W1GrssyrMP<0),which(data.2$W1GrssyrHH<0),
                      which(data.2$W1yschat1<0),which(data.2$W2ghq12scr<0),
                      which(data.2$W4schatYP<0),which(data.2$W6DebtattYP<0),
                      which(data.2$W8DGHQSC<0),which(data.2$W8GROW<0), 
                      which(data.2$W8NETW<0), which(data.2$W8NETA<0),
                      which(data.2$W8DINCW<0),which(data.2$W8QDEB2<0) )
data.3<-data.2[-missing_continuous,]

summary(data.3)
str(data.3)
dim(data.3) 
#1069 rows 

#Merge the missing values into one level for categorical predictors
data_index.1 <- c(1:67)
for(i in data_index.1){data.3[,i] <- ifelse(data.3[,i]<0,"missing",data.3[,i])}

#Centering continuous predictors where necessary
summary(data.3)
data.3$cent.W1GrssyrMP<-with(data.3,W1GrssyrMP-mean(W1GrssyrMP))
data.3$cent.W1GrssyrHH<-with(data.3,W1GrssyrHH-mean(W1GrssyrHH))
data.3$cent.W8DINCW<-with(data.3,W8DINCW-mean(W8DINCW))
data.3[,c(1,2,54)]
data.4<-data.3[,-c(1,2,54)]

summary(data.4)
summary(data.4[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
str(data.4)
dim(data.4)

#Coding categorical variables as factors 
data_index.2<-c(1:67)
summary(data.4[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
categorical_index<-data_index.2[-c(30,31,39,51,53,60,61,62,63,65,66,67)]
for (i in categorical_index){data.4[,i]<-as.factor(data.4[,i])}

cols <- c("W1wrk1aMP","W1condur5MP","W1hea2MP","W1NoldBroHS",
          "W1InCarHH", "W1hous12HH", "W1usevcHH","W1hiqualmum","W1wrkfulldad",
          "W1wrkfullmum", "W1empsmum", "W1empsdad","W1ch0_2HH","W1ch3_11HH",
          "W1ch12_15HH", "W1ch16_17HH", "IndSchool", "W1marstatmum","W1depkids",
          "W1famtyp2", "W1nssecfam", "W1ethgrpYP", "W1heposs9YP", "W1hwndayYP",
          "W1truantYP", "W1alceverYP", "W1bulrc", "W1disabYP",
          "W2disc1YP", "W2depressYP", "W4CannTryYP", "W4NamesYP",
          "W4RacismYP","W4empsYP","W5JobYP","W5EducYP",
          "W5Apprent1YP","W6JobYP","W6UnivYP","W6acqno","W6gcse",
          "W6als","W6OwnchiDV","W8DMARSTAT","W8DACTIVITYC",
          "W8DWRK","W8CMSEX","W8TENURE","W8DACTIVITY","W8QMAFI")
data.4[cols] <- lapply(data.4[cols], factor)

str(data.4)

#Changing the baseline for some predictors
#Most common is set as the reference level

summary(data.4)
str(data.4)

data.4$W1hea2MP<-relevel(data.4$W1hea2MP,ref = "2")
data.4$W1hous12HH<-relevel(data.4$W1hous12HH,ref = "2")
data.4$W1hiqualmum<-relevel(data.4$W1hiqualmum,ref = "15")
data.4$W1hiqualdad<-relevel(data.4$W1hiqualdad,ref= "15") #most common is missing so second highest
data.4$W1wrkfullmum<-relevel(data.4$W1wrkfullmum,ref = "2")
data.4$W1empsmum<-relevel(data.4$W1empsmum,ref = "2")
data.4$W1ch12_15HH<-relevel(data.4$W1ch12_15HH,ref = "1")
data.4$W1marstatmum<-relevel(data.4$W1marstatmum, ref = "2")
data.4$W1depkids<-relevel(data.4$W1depkids,ref = "2")
data.4$W1nssecfam<-relevel(data.4$W1nssecfam,ref = "2")
data.4$W1hwndayYP<-relevel(data.4$W1hwndayYP,ref = "3")
data.4$W1truantYP<-relevel(data.4$W1truantYP,ref="2")
data.4$W1bulrc<-relevel(data.4$W1bulrc,ref = "2")
data.4$W1disabYP<-relevel(data.4$W1disabYP,ref = "3")#again
data.4$W2disc1YP<-relevel(data.4$W2disc1YP,ref = "2")
data.4$W4AlcFreqYP<-relevel(data.4$W4AlcFreqYP,ref = "2")
data.4$W4CannTryYP<-relevel(data.4$W4CannTryYP,ref = "2")#
data.4$W4NamesYP<-relevel(data.4$W4NamesYP,ref = "2")
data.4$W4RacismYP<-relevel(data.4$W4RacismYP,ref = "2")
data.4$W4empsYP<-relevel(data.4$W4empsYP,ref = "5")
data.4$W5Apprent1YP<-relevel(data.4$W5Apprent1YP,ref = "2")
data.4$W6UnivYP<-relevel(data.4$W6UnivYP,ref = "2")
data.4$W6EducYP<-relevel(data.4$W6EducYP,ref = "2") #Most occuring is missing
data.4$W6Apprent1YP<-relevel(data.4$W6Apprent1YP,ref = "2")
data.4$W6acqno<-relevel(data.4$W6acqno,ref = "8")
data.4$W6gcse<-relevel(data.4$W6gcse,ref = "4")
data.4$W6als<-relevel(data.4$W6als,ref = "4")
data.4$W6OwnchiDV<-relevel(data.4$W6OwnchiDV,ref = "2")
data.4$W8CMSEX<-relevel(data.4$W8CMSEX,ref = "2")
data.4$W8DDEGP<-relevel(data.4$W8DDEGP,ref = "1")#
data.4$W8TENURE<-relevel(data.4$W8TENURE,ref = "4")
data.4$W8QMAFI<-relevel(data.4$W8QMAFI,ref = "2")

dim(data.4)
summary(data.4)
range(data.4$W8QDEB2)



#Initial model - W8QDEB2.lm.1 (66 predictors)

W8QDEB2.lm.1<-lm(W8QDEB2~., data = data.4)

display(W8QDEB2.lm.1,detail=TRUE)
summary(W8QDEB2.lm.1)
Anova(W8QDEB2.lm.1)



#Second model - W8QDEB2.lm.2 (41 predictors)

missing_continuous.2<-c(which(data.2$W1GrssyrMP<0),which(data.2$W1GrssyrHH<0),
                        which(data.2$W1yschat1<0),which(data.2$W4schatYP<0),
                        which(data.2$W6DebtattYP<0),which(data.2$W8NETA<0),
                        which(data.2$W8DINCW<0),which(data.2$W8QDEB2<0) )
data.5<-data.2[-missing_continuous.2,]

dim(data.5)#1119 rows


#For categorical predictors - merge the missing values into one category
data_index.1 <- c(1:67)
for(i in data_index.1){data.5[,i] <- ifelse(data.5[,i]<0,"missing",data.5[,i])}


#Centering continuous predictors where necessary
summary(data.5)
data.5$cent.W1GrssyrMP<-with(data.5,W1GrssyrMP-mean(W1GrssyrMP))
data.5$cent.W1GrssyrHH<-with(data.5,W1GrssyrHH-mean(W1GrssyrHH))
data.5$cent.W8DINCW<-with(data.5,W8DINCW-mean(W8DINCW))
data.5[,c(1,2,54)]
data.6<-data.5[,-c(1,2,54)]

summary(data.6)
summary(data.6[,c(30,31,39,51,53,60,61,62,63,65,66,67)])

str(data.6)
dim(data.6)

#Coding categorical variables as factors 
data_index.2<-c(1:67)
summary(data.6[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
categorical_index<-data_index.2[-c(30,31,39,51,53,60,61,62,63,65,66,67)]
for (i in categorical_index){data.6[,i]<-as.factor(data.6[,i])}

cols <- c("W1wrk1aMP","W1condur5MP","W1hea2MP","W1NoldBroHS",
          "W1InCarHH", "W1hous12HH", "W1usevcHH","W1hiqualmum","W1wrkfulldad",
          "W1wrkfullmum", "W1empsmum", "W1empsdad","W1ch0_2HH","W1ch3_11HH",
          "W1ch12_15HH", "W1ch16_17HH", "IndSchool", "W1marstatmum","W1depkids",
          "W1famtyp2", "W1nssecfam", "W1ethgrpYP", "W1heposs9YP", "W1hwndayYP",
          "W1truantYP", "W1alceverYP", "W1bulrc", "W1disabYP",
          "W2disc1YP", "W2depressYP", "W4CannTryYP", "W4NamesYP",
          "W4RacismYP","W4empsYP","W5JobYP","W5EducYP",
          "W5Apprent1YP","W6JobYP","W6UnivYP","W6acqno","W6gcse",
          "W6als","W6OwnchiDV","W8DMARSTAT","W8DACTIVITYC",
          "W8DWRK","W8CMSEX","W8TENURE","W8DACTIVITY","W8QMAFI")
data.6[cols] <- lapply(data.6[cols], factor)


str(data.6)

#Changing the baseline for some predictors
#Most common is set as the reference level

summary(data.6)
str(data.6)

data.6$W1hea2MP<-relevel(data.6$W1hea2MP,ref = "2")
data.6$W1hous12HH<-relevel(data.6$W1hous12HH,ref = "2")
data.6$W1hiqualmum<-relevel(data.6$W1hiqualmum,ref = "15")
data.6$W1hiqualdad<-relevel(data.6$W1hiqualdad,ref= "15") #most common is missing so second highest
data.6$W1wrkfullmum<-relevel(data.6$W1wrkfullmum,ref = "2")
data.6$W1empsmum<-relevel(data.6$W1empsmum,ref = "2")
data.6$W1ch12_15HH<-relevel(data.6$W1ch12_15HH,ref = "1")
data.6$W1marstatmum<-relevel(data.6$W1marstatmum, ref = "2")
data.6$W1depkids<-relevel(data.6$W1depkids,ref = "2")
data.6$W1nssecfam<-relevel(data.6$W1nssecfam,ref = "2")
data.6$W1hwndayYP<-relevel(data.6$W1hwndayYP,ref = "3")
data.6$W1truantYP<-relevel(data.6$W1truantYP,ref="2")
data.6$W1bulrc<-relevel(data.6$W1bulrc,ref = "2")
data.6$W1disabYP<-relevel(data.6$W1disabYP,ref = "3")#again
data.6$W2disc1YP<-relevel(data.6$W2disc1YP,ref = "2")
data.6$W4AlcFreqYP<-relevel(data.6$W4AlcFreqYP,ref = "2")
data.6$W4CannTryYP<-relevel(data.6$W4CannTryYP,ref = "2")#
data.6$W4NamesYP<-relevel(data.6$W4NamesYP,ref = "2")
data.6$W4RacismYP<-relevel(data.6$W4RacismYP,ref = "2")
data.6$W4empsYP<-relevel(data.6$W4empsYP,ref = "5")
data.6$W5Apprent1YP<-relevel(data.6$W5Apprent1YP,ref = "2")
data.6$W6UnivYP<-relevel(data.6$W6UnivYP,ref = "2")
data.6$W6EducYP<-relevel(data.6$W6EducYP,ref = "2") #Most occuring is missing
data.6$W6Apprent1YP<-relevel(data.6$W6Apprent1YP,ref = "2")
data.6$W6acqno<-relevel(data.6$W6acqno,ref = "8")
data.6$W6gcse<-relevel(data.6$W6gcse,ref = "4")
data.6$W6als<-relevel(data.6$W6als,ref = "4")
data.6$W6OwnchiDV<-relevel(data.6$W6OwnchiDV,ref = "2")
data.6$W8CMSEX<-relevel(data.6$W8CMSEX,ref = "2")
data.6$W8DDEGP<-relevel(data.6$W8DDEGP,ref = "1")#
data.6$W8TENURE<-relevel(data.6$W8TENURE,ref = "4")
data.6$W8QMAFI<-relevel(data.6$W8QMAFI,ref = "2")


summary(data.6[,c(53,55,56,60,61)])
summary(data.6[,c(5,7,10,11,14,15,16,17,19,27,31,32,34,36,37,41,42,45,46,47,53,55,56,60,61)])
data.7<-data.6[,-c(5,7,10,11,14,15,16,17,19,27,31,32,34,36,37,41,42,45,46,47,53,55,56,60,61)]
summary(data.7)
dim(data.7)
range(data.7$W8QDEB2)

dim(data.7)
summary(data.7)
range(data.7$W8QDEB2)


W8QDEB2.lm.2<-lm(W8QDEB2~., data = data.7)

display(W8QDEB2.lm.2,detail=TRUE)
summary(W8QDEB2.lm.2)
Anova(W8QDEB2.lm.2)

alias(W8QDEB2.lm.2)

#Remove W1empsmum and W1empsdad as they are colinear with W1hiqualmum and W1hiqualdad, which are more significant
#Remove W1NoOldBro as it was colinear with W1disabYP
#Remove W1famtyp2 as it was colinear with W1depkids
#Remove W1nssecfam as it was colinear with W6JobYP
summary(data.7[,c(4,8,9,12,13)])
data.8 <- data.7[,-c(4,8,9,12,13)]
dim(data.8)
range(data.8$W8QDEB2)
summary(data.8)




#Third model (36 predictors)
W8QDEB2.lm.3<-lm(W8QDEB2~., data = data.8)

display(W8QDEB2.lm.3,detail=TRUE)
summary(W8QDEB2.lm.3)
Anova(W8QDEB2.lm.3)
vif(W8QDEB2.lm.3)


#Remove W1hous12HH(GVIF = 11.1) W1hiqualmum (GVIF = 21.0), W1hiqualdad (GVIF = 18.6)
#High VIF (over 10)
summary(data.8[,c(4,5,6)])
data.9 <- data.8[,-c(4,5,6)]
range(data.9$W8QDEB2)
dim(data.9)
range(data.9$W8QDEB2)



#Fourth model (33 predictors)

summary(data.9)
dim(data.9) 

W8QDEB2.lm.4<-lm(W8QDEB2~., data = data.9)
display(W8QDEB2.lm.4,detail=TRUE)
summary(W8QDEB2.lm.4)
Anova(W8QDEB2.lm.4)
vif(W8QDEB2.lm.4)

#Remove continuous predictors with a P value greater than 0.5
#Remove ,W6DebtattYP, cent.W1GrssyrMP,cent.W1GrssyrHH,cent.W8DINCW



#Fifth model (29 Predictors)
missing_continuous.6<-c(which(data.2$W1yschat1<0),
                        which(data.2$W4schatYP<0),which(data.2$W8NETA<0),
                        which(data.2$W8QDEB2<0) )
data.11<-data.2[-missing_continuous.6,]

dim(data.11)#2271 rows

#Centering continuous predictors where necessary
summary(data.11)
data.11$cent.W1GrssyrMP<-with(data.11,W1GrssyrMP-mean(W1GrssyrMP))
data.11$cent.W1GrssyrHH<-with(data.11,W1GrssyrHH-mean(W1GrssyrHH))
data.11$cent.W8DINCW<-with(data.11,W8DINCW-mean(W8DINCW))
summary(data.11[,c(1,2,54)])
data.12<-data.11[,-c(1,2,54)]
summary(data.12)
summary(data.12[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
str(data.12)
dim(data.12)

#For categorical predictors - merge the missing values into one category
data_index.1 <- c(1:67)
summary(data.12[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
categorical_index<-data_index.1[-c(30,31,39,51,53,60,61,62,63,65,66,67)]
for(i in data_index.1){data.12[,i] <- ifelse(data.12[,i]<0,"missing",data.12[,i])}

#Coding categorical variables as factors 
data_index.2<-c(1:67)
summary(data.12[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
categorical_index<-data_index.2[-c(30,31,39,51,53,60,61,62,63,65,66,67)]
for (i in categorical_index){data.12[,i]<-as.factor(data.12[,i])}

cols <- c("W1wrk1aMP","W1condur5MP","W1hea2MP","W1NoldBroHS",
          "W1InCarHH", "W1hous12HH", "W1usevcHH","W1hiqualmum","W1wrkfulldad",
          "W1wrkfullmum", "W1empsmum", "W1empsdad","W1ch0_2HH","W1ch3_11HH",
          "W1ch12_15HH", "W1ch16_17HH", "IndSchool", "W1marstatmum","W1depkids",
          "W1famtyp2", "W1nssecfam", "W1ethgrpYP", "W1heposs9YP", "W1hwndayYP",
          "W1truantYP", "W1alceverYP", "W1bulrc", "W1disabYP",
          "W2disc1YP", "W2depressYP", "W4CannTryYP", "W4NamesYP",
          "W4RacismYP","W4empsYP","W5JobYP","W5EducYP",
          "W5Apprent1YP","W6JobYP","W6UnivYP","W6acqno","W6gcse",
          "W6als","W6OwnchiDV","W8DMARSTAT","W8DACTIVITYC",
          "W8DWRK","W8CMSEX","W8TENURE","W8DACTIVITY","W8QMAFI")
data.12[cols] <- lapply(data.12[cols], factor)


str(data.12)

#Changing the baseline for some predictors
#Most common is set as the reference level
summary(data.12)
str(data.12)
summary(data.12$W1hiqualdad)
data.12$W1hea2MP<-relevel(data.12$W1hea2MP,ref = "2")
data.12$W1hous12HH<-relevel(data.12$W1hous12HH,ref = "2")
data.12$W1hiqualmum<-relevel(data.12$W1hiqualmum,ref = "15")
data.12$W1hiqualdad<-relevel(data.12$W1hiqualdad,ref= "15") #most common is missing so second highest
data.12$W1wrkfullmum<-relevel(data.12$W1wrkfullmum,ref = "2")
data.12$W1empsmum<-relevel(data.12$W1empsmum,ref = "2")
data.12$W1ch12_15HH<-relevel(data.12$W1ch12_15HH,ref = "1")
data.12$W1marstatmum<-relevel(data.12$W1marstatmum, ref = "2")
data.12$W1depkids<-relevel(data.12$W1depkids,ref = "2")
data.12$W1nssecfam<-relevel(data.12$W1nssecfam,ref = "2")
data.12$W1hwndayYP<-relevel(data.12$W1hwndayYP,ref = "3")
data.12$W1truantYP<-relevel(data.12$W1truantYP,ref="2")
data.12$W1bulrc<-relevel(data.12$W1bulrc,ref = "2")
data.12$W1disabYP<-relevel(data.12$W1disabYP,ref = "3")#again
data.12$W2disc1YP<-relevel(data.12$W2disc1YP,ref = "2")
data.12$W4AlcFreqYP<-relevel(data.12$W4AlcFreqYP,ref = "2")
data.12$W4CannTryYP<-relevel(data.12$W4CannTryYP,ref = "2")#
data.12$W4NamesYP<-relevel(data.12$W4NamesYP,ref = "2")
data.12$W4RacismYP<-relevel(data.12$W4RacismYP,ref = "2")
data.12$W4empsYP<-relevel(data.12$W4empsYP,ref = "5")
data.12$W5Apprent1YP<-relevel(data.12$W5Apprent1YP,ref = "2")
data.12$W6UnivYP<-relevel(data.12$W6UnivYP,ref = "2")
data.12$W6EducYP<-relevel(data.12$W6EducYP,ref = "2") #Most occuring is missing
data.12$W6Apprent1YP<-relevel(data.12$W6Apprent1YP,ref = "2")
data.12$W6acqno<-relevel(data.12$W6acqno,ref = "8")
data.12$W6gcse<-relevel(data.12$W6gcse,ref = "4")
data.12$W6als<-relevel(data.12$W6als,ref = "4")
data.12$W6OwnchiDV<-relevel(data.12$W6OwnchiDV,ref = "2")
data.12$W8CMSEX<-relevel(data.12$W8CMSEX,ref = "2")
data.12$W8DDEGP<-relevel(data.12$W8DDEGP,ref = "1")#
data.12$W8TENURE<-relevel(data.12$W8TENURE,ref = "4")
data.12$W8QMAFI<-relevel(data.12$W8QMAFI,ref = "2")

dim(data.12)
range(data.12$W8QDEB2)


W8QDEB2.lm.5<-lm(W8QDEB2~ W1wrk1aMP+  W1condur5MP+  W1hea2MP+  IndSchool+  W1depkids+  W1ethgrpYP+
                   W1heposs9YP + W1hwndayYP+ W1truantYP +W1bulrc +  W1disabYP  + W1yschat1+  W2depressYP +       
                 W4CannTryYP  +  W4empsYP  +    W4schatYP      +    W5JobYP    + W6JobYP  + 
                 W6UnivYP      + W6gcse +  W6als   + W6OwnchiDV  + W8DDEGP  +  W8DMARSTAT      +
                 W8CMSEX   +   W8TENURE  + W8DACTIVITY   +  W8NETA         + W8QMAFI  , data = data.12)

display(W8QDEB2.lm.5,detail=TRUE)
summary(W8QDEB2.lm.5)
Anova(W8QDEB2.lm.5)
vif(W8QDEB2.lm.5)
alias(W8QDEB2.lm.5)

#W1condur5MPmissing and W1wrk1aMPmissing are aliased
#W8TENUREmissing and W8DMARSTAT8 are aliased

#Remove all predictors with a P value greater than 0.5 

#Got rid of W1wrk1aMP and W8DMARSTAT. So problem solved




#Sixth Model (14 Predictors)
str(data.12)

W8QDEB2.lm.6<-lm(W8QDEB2~ W1condur5MP+ W1hea2MP+ IndSchool+
                   W1heposs9YP+ W1hwndayYP+ W1bulrc  +W1yschat1+ W2depressYP+ W4CannTryYP+  
                   W4schatYP+ W6OwnchiDV+  W8CMSEX +
                   W8TENURE+ W8NETA   , data = data.12)

display(W8QDEB2.lm.6,detail=TRUE)
summary(W8QDEB2.lm.6)
Anova(W8QDEB2.lm.6)
vif(W8QDEB2.lm.6)

#IndSchool, W1heposs9YP, W8CMSEX and W8TENURE are significant at a 5% level

#Remove predictors with a p-value greater than 0.3

#Remove W1hea2MP, W1hwndayYP, W1bulrc, W2depressYP



#Seventh model (10 Predictors)

W8QDEB2.lm.7<-lm(W8QDEB2~ W1condur5MP+ IndSchool+
                   W1heposs9YP  +W1yschat1+ W4CannTryYP+  
                   W4schatYP+ W6OwnchiDV+  W8CMSEX +
                   W8TENURE+ W8NETA   , data = data.12)

display(W8QDEB2.lm.7,detail=TRUE)
summary(W8QDEB2.lm.7)
Anova(W8QDEB2.lm.7)
vif(W8QDEB2.lm.7)

#Remove W1yschat1 as it is the least significant predictor



#Eighth model (9 Predictors)
missing_continuous.8<-c(which(data.2$W4schatYP<0),which(data.2$W8NETA<0),
                        which(data.2$W8QDEB2<0) )
data.13<-data.2[-missing_continuous.8,]

dim(data.13)
#2301 rows

#Centering continuous predictors where necessary
summary(data.13)
data.13$cent.W1GrssyrMP<-with(data.13,W1GrssyrMP-mean(W1GrssyrMP))
data.13$cent.W1GrssyrHH<-with(data.13,W1GrssyrHH-mean(W1GrssyrHH))
data.13$cent.W8DINCW<-with(data.13,W8DINCW-mean(W8DINCW))
summary(data.13[,c(1,2,54)])
data.14<-data.13[,-c(1,2,54)]

summary(data.14)
summary(data.14[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
str(data.14)
dim(data.14)

#For categorical predictors - merge the missing values into one category
data_index.1 <- c(1:67)
summary(data.14[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
categorical_index<-data_index.1[-c(30,31,39,51,53,60,61,62,63,65,66,67)]
for(i in data_index.1){data.14[,i] <- ifelse(data.14[,i]<0,"missing",data.14[,i])}

#Coding categorical variables as factors 
data_index.2<-c(1:67)
summary(data.14[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
categorical_index<-data_index.2[-c(30,31,39,51,53,60,61,62,63,65,66,67)]
for (i in categorical_index){data.14[,i]<-as.factor(data.14[,i])}

cols <- c("W1wrk1aMP","W1condur5MP","W1hea2MP","W1NoldBroHS",
          "W1InCarHH", "W1hous12HH", "W1usevcHH","W1hiqualmum","W1wrkfulldad",
          "W1wrkfullmum", "W1empsmum", "W1empsdad","W1ch0_2HH","W1ch3_11HH",
          "W1ch12_15HH", "W1ch16_17HH", "IndSchool", "W1marstatmum","W1depkids",
          "W1famtyp2", "W1nssecfam", "W1ethgrpYP", "W1heposs9YP", "W1hwndayYP",
          "W1truantYP", "W1alceverYP", "W1bulrc", "W1disabYP",
          "W2disc1YP", "W2depressYP", "W4CannTryYP", "W4NamesYP",
          "W4RacismYP","W4empsYP","W5JobYP","W5EducYP",
          "W5Apprent1YP","W6JobYP","W6UnivYP","W6acqno","W6gcse",
          "W6als","W6OwnchiDV","W8DMARSTAT","W8DACTIVITYC",
          "W8DWRK","W8CMSEX","W8TENURE","W8DACTIVITY","W8QMAFI")
data.14[cols] <- lapply(data.14[cols], factor)

str(data.14)

#Changing the baseline for some predictors
#Most common is set as the reference level

summary(data.14)
str(data.14)

data.14$W1hea2MP<-relevel(data.14$W1hea2MP,ref = "2")
data.14$W1hous12HH<-relevel(data.14$W1hous12HH,ref = "2")
data.14$W1hiqualmum<-relevel(data.14$W1hiqualmum,ref = "15")
data.14$W1hiqualdad<-relevel(data.14$W1hiqualdad,ref= "15") #most common is missing so second highest
data.14$W1wrkfullmum<-relevel(data.14$W1wrkfullmum,ref = "2")
data.14$W1empsmum<-relevel(data.14$W1empsmum,ref = "2")
data.14$W1ch12_15HH<-relevel(data.14$W1ch12_15HH,ref = "1")
data.14$W1marstatmum<-relevel(data.14$W1marstatmum, ref = "2")
data.14$W1depkids<-relevel(data.14$W1depkids,ref = "2")
data.14$W1nssecfam<-relevel(data.14$W1nssecfam,ref = "2")
data.14$W1hwndayYP<-relevel(data.14$W1hwndayYP,ref = "3")
data.14$W1truantYP<-relevel(data.14$W1truantYP,ref="2")
data.14$W1bulrc<-relevel(data.14$W1bulrc,ref = "2")
data.14$W1disabYP<-relevel(data.14$W1disabYP,ref = "3")#again
data.14$W2disc1YP<-relevel(data.14$W2disc1YP,ref = "2")
data.14$W4AlcFreqYP<-relevel(data.14$W4AlcFreqYP,ref = "2")
data.14$W4CannTryYP<-relevel(data.14$W4CannTryYP,ref = "2")#
data.14$W4NamesYP<-relevel(data.14$W4NamesYP,ref = "2")
data.14$W4RacismYP<-relevel(data.14$W4RacismYP,ref = "2")
data.14$W4empsYP<-relevel(data.14$W4empsYP,ref = "5")
data.14$W5Apprent1YP<-relevel(data.14$W5Apprent1YP,ref = "2")
data.14$W6UnivYP<-relevel(data.14$W6UnivYP,ref = "2")
data.14$W6EducYP<-relevel(data.14$W6EducYP,ref = "2") #Most occuring is missing
data.14$W6Apprent1YP<-relevel(data.14$W6Apprent1YP,ref = "2")
data.14$W6acqno<-relevel(data.14$W6acqno,ref = "8")
data.14$W6gcse<-relevel(data.14$W6gcse,ref = "4")
data.14$W6als<-relevel(data.14$W6als,ref = "4")
data.14$W6OwnchiDV<-relevel(data.14$W6OwnchiDV,ref = "2")
data.14$W8CMSEX<-relevel(data.14$W8CMSEX,ref = "2")
data.14$W8DDEGP<-relevel(data.14$W8DDEGP,ref = "1")#
data.14$W8TENURE<-relevel(data.14$W8TENURE,ref = "4")
data.14$W8QMAFI<-relevel(data.14$W8QMAFI,ref = "2")

dim(data.14)
range(data.14$W8QDEB2)


W8QDEB2.lm.8<-lm(W8QDEB2~ W1condur5MP+ IndSchool+
                   W1heposs9YP  + W4CannTryYP+  
                   W4schatYP+ W6OwnchiDV+  W8CMSEX +
                   W8TENURE+ W8NETA   , data = data.14)

display(W8QDEB2.lm.8,detail=TRUE)
summary(W8QDEB2.lm.8)
Anova(W8QDEB2.lm.8)
vif(W8QDEB2.lm.8)

#IndSchool, W1heposs9YP, W8CMSEX and W8TENURE are significant at a 5% level

#Remove W4schatYP as it is the least significant predictor



#Ninth model (8 Predictors)

missing_continuous.9<-c(which(data.2$W8NETA<0),
                        which(data.2$W8QDEB2<0) )
data.15<-data.2[-missing_continuous.9,]

dim(data.15)#2317 rows

#Centering continuous predictors where necessary
summary(data.15)
data.15$cent.W1GrssyrMP<-with(data.15,W1GrssyrMP-mean(W1GrssyrMP))
data.15$cent.W1GrssyrHH<-with(data.15,W1GrssyrHH-mean(W1GrssyrHH))
data.15$cent.W8DINCW<-with(data.15,W8DINCW-mean(W8DINCW))
summary(data.15[,c(1,2,54)])
data.16<-data.15[,-c(1,2,54)]

summary(data.16)
summary(data.16[,c(30,31,39,51,53,60,61,62,63,65,66,67)])

str(data.16)
dim(data.16)

#For categorical predictors - merge the missing values into one category
data_index.1 <- c(1:67)
summary(data.16[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
categorical_index<-data_index.1[-c(30,31,39,51,53,60,61,62,63,65,66,67)]
for(i in data_index.1){data.16[,i] <- ifelse(data.16[,i]<0,"missing",data.16[,i])}

#Coding categorical variables as factors 

data_index.2<-c(1:67)
summary(data.16[,c(30,31,39,51,53,60,61,62,63,65,66,67)])
categorical_index<-data_index.2[-c(30,31,39,51,53,60,61,62,63,65,66,67)]
for (i in categorical_index){data.16[,i]<-as.factor(data.16[,i])}

cols <- c("W1wrk1aMP","W1condur5MP","W1hea2MP","W1NoldBroHS",
          "W1InCarHH", "W1hous12HH", "W1usevcHH","W1hiqualmum","W1wrkfulldad",
          "W1wrkfullmum", "W1empsmum", "W1empsdad","W1ch0_2HH","W1ch3_11HH",
          "W1ch12_15HH", "W1ch16_17HH", "IndSchool", "W1marstatmum","W1depkids",
          "W1famtyp2", "W1nssecfam", "W1ethgrpYP", "W1heposs9YP", "W1hwndayYP",
          "W1truantYP", "W1alceverYP", "W1bulrc", "W1disabYP",
          "W2disc1YP", "W2depressYP", "W4CannTryYP", "W4NamesYP",
          "W4RacismYP","W4empsYP","W5JobYP","W5EducYP",
          "W5Apprent1YP","W6JobYP","W6UnivYP","W6acqno","W6gcse",
          "W6als","W6OwnchiDV","W8DMARSTAT","W8DACTIVITYC",
          "W8DWRK","W8CMSEX","W8TENURE","W8DACTIVITY","W8QMAFI")
data.16[cols] <- lapply(data.16[cols], factor)
str(data.16)

#Changing the baseline for some predictors
#Most common is set as the reference level
summary(data.16)
str(data.16)
data.16$W1hea2MP<-relevel(data.16$W1hea2MP,ref = "2")
data.16$W1hous12HH<-relevel(data.16$W1hous12HH,ref = "2")
data.16$W1hiqualmum<-relevel(data.16$W1hiqualmum,ref = "15")
data.16$W1hiqualdad<-relevel(data.16$W1hiqualdad,ref= "15") #most common is missing so second highest
data.16$W1wrkfullmum<-relevel(data.16$W1wrkfullmum,ref = "2")
data.16$W1empsmum<-relevel(data.16$W1empsmum,ref = "2")
data.16$W1ch12_15HH<-relevel(data.16$W1ch12_15HH,ref = "1")
data.16$W1marstatmum<-relevel(data.16$W1marstatmum, ref = "2")
data.16$W1depkids<-relevel(data.16$W1depkids,ref = "2")
data.16$W1nssecfam<-relevel(data.16$W1nssecfam,ref = "2")
data.16$W1hwndayYP<-relevel(data.16$W1hwndayYP,ref = "3")
data.16$W1truantYP<-relevel(data.16$W1truantYP,ref="2")
data.16$W1bulrc<-relevel(data.16$W1bulrc,ref = "2")
data.16$W1disabYP<-relevel(data.16$W1disabYP,ref = "3")#again
data.16$W2disc1YP<-relevel(data.16$W2disc1YP,ref = "2")
data.16$W4AlcFreqYP<-relevel(data.16$W4AlcFreqYP,ref = "2")
data.16$W4CannTryYP<-relevel(data.16$W4CannTryYP,ref = "2")#
data.16$W4NamesYP<-relevel(data.16$W4NamesYP,ref = "2")
data.16$W4RacismYP<-relevel(data.16$W4RacismYP,ref = "2")
data.16$W4empsYP<-relevel(data.16$W4empsYP,ref = "5")
data.16$W5Apprent1YP<-relevel(data.16$W5Apprent1YP,ref = "2")
data.16$W6UnivYP<-relevel(data.16$W6UnivYP,ref = "2")
data.16$W6EducYP<-relevel(data.16$W6EducYP,ref = "2") #Most occuring is missing
data.16$W6Apprent1YP<-relevel(data.16$W6Apprent1YP,ref = "2")
data.16$W6acqno<-relevel(data.16$W6acqno,ref = "8")
data.16$W6gcse<-relevel(data.16$W6gcse,ref = "4")
data.16$W6als<-relevel(data.16$W6als,ref = "4")
data.16$W6OwnchiDV<-relevel(data.16$W6OwnchiDV,ref = "2")
data.16$W8CMSEX<-relevel(data.16$W8CMSEX,ref = "2")
data.16$W8DDEGP<-relevel(data.16$W8DDEGP,ref = "1")#
data.16$W8TENURE<-relevel(data.16$W8TENURE,ref = "4")
data.16$W8QMAFI<-relevel(data.16$W8QMAFI,ref = "2")

dim(data.16)
range(data.16$W8QDEB2)
#0 400000

str(data.16)

W8QDEB2.lm.9<-lm(W8QDEB2~ W1condur5MP+ IndSchool+
                   W1heposs9YP  + W4CannTryYP+  
                   W6OwnchiDV+  W8CMSEX +
                   W8TENURE+ W8NETA    , data = data.16)

display(W8QDEB2.lm.9,detail=TRUE)
summary(W8QDEB2.lm.9)
Anova(W8QDEB2.lm.9)
vif(W8QDEB2.lm.9)

#Remove W4CannTryYP as it is the least significant predictor



#Tenth model (7 Predictors)

W8QDEB2.lm.10<-lm(W8QDEB2~ W1condur5MP+ IndSchool+
                   W1heposs9YP+  
                   W6OwnchiDV+  W8CMSEX +
                   W8TENURE+ W8NETA    , data = data.16)

display(W8QDEB2.lm.10,detail=TRUE)
summary(W8QDEB2.lm.10)
Anova(W8QDEB2.lm.10)
vif(W8QDEB2.lm.10)


#Scatter plots of continuous predictors
summary(data.16)

p1<- ggplot(data.16, aes(x=W8NETA, y=W8QDEB2)) +geom_point()
p1<-p1+geom_smooth(method = "lm", fill=NA)
p1


#Boxplots of categorical predictors

p2<- ggplot(data.16, aes(y=W8QDEB2, x=W1condur5MP, colour=factor(W1condur5MP))) +geom_boxplot()
p2

p3<- ggplot(data.16, aes(y=W8QDEB2, x=IndSchool, colour=factor(IndSchool))) +geom_boxplot()
p3

p4<- ggplot(data.16, aes(y=W8QDEB2, x=W1heposs9YP, colour=factor(W1heposs9YP))) +geom_boxplot()
p4

p5<- ggplot(data.16, aes(y=W8QDEB2, x=W6OwnchiDV, colour=factor(W6OwnchiDV))) +geom_boxplot()
p5

p6<- ggplot(data.16, aes(y=W8QDEB2, x=W8CMSEX, colour=factor(W8CMSEX))) +geom_boxplot()
p6

p7<- ggplot(data.16, aes(y=W8QDEB2, x=W8TENURE, colour=factor(W8TENURE))) +geom_boxplot()
p7

#Rename levels of categorical predictors

#Rename levels of W1condur5MP
summary(data.16$W1condur5MP)
data.16$W1condur5MP.1 <- factor(data.16$W1condur5MP,
                              levels = c("1","2","missing"),
                              labels = c("Yes", "No","Missing"))
summary(data.16$W1condur5MP.1)

#Rename levels of IndSchool
summary(data.16$IndSchool)
data.16$IndSchool.1 <- factor(data.16$IndSchool,
                              levels = c("0","1"),
                              labels = c("Maintained", "Independent"))
summary(data.16$IndSchool.1)

#Rename levels of W1heposs9YP
summary(data.16$W1heposs9YP)
data.16$W1heposs9YP.1 <- factor(data.16$W1heposs9YP,
                                levels = c("1","2","3","4","missing"),
                                labels = c("Very likely", "Fairly likely","Not very likely",
                                           "Not at all likely","Missing"))
summary(data.16$W1heposs9YP.1)

#Rename levels of W6OwnchiDV
summary(data.16$W6OwnchiDV)
data.16$W6OwnchiDV.1 <- factor(data.16$W6OwnchiDV,
                              levels = c("2","1","missing"),
                              labels = c("No", "Yes","Missing"))
summary(data.16$W6OwnchiDV.1)

#Rename levels of W8CMSEX
summary(data.16$W8CMSEX)
data.16$W8CMSEX.1 <- factor(data.16$W8CMSEX,
                            levels = c("2","1"),
                            labels = c("Female", "Male"))
summary(data.16$W8CMSEX.1)

#Rename the levels of W8TENURE
summary(data.16$W8TENURE)
data.16$W8TENURE.1 <- factor(data.16$W8TENURE,
                              levels = c("4","1","2","3","5","7","missing"),
                              labels = c("Rent including housing benefits ", "Owned outright","Bought on a mortgage",
                                         "Part rent/mortgage","Rent free","Other","missing"))
summary(data.16$W8TENURE.1)
str(data.16)


W8QDEB2.lm.10<-lm(W8QDEB2~ W1condur5MP.1+ IndSchool.1+
                    W1heposs9YP.1+  
                    W6OwnchiDV.1+  W8CMSEX.1 +
                    W8TENURE.1+ W8NETA    , data = data.16)

display(W8QDEB2.lm.10,detail=TRUE)
summary(W8QDEB2.lm.10)
Anova(W8QDEB2.lm.10)
vif(W8QDEB2.lm.10)

par(mfrow=c(1,3))
plot(W8QDEB2.lm.10,which=c(1,2),cex=0.7,pch=".")
hist(W8QDEB2.lm.10$residuals, main="Histogram of standardised residuals", xlab="std. residuals")

#No funnel shape but normality is violated

#Exploratory analysis - Remove zero debt points
dim(data.16) #2317 rows
data.17<-data.16[-c(which(data.16$W8QDEB2==0)),]
dim(data.17)#2249 rows
#Therefore 68 points with debt=0

range(log(data.17$W8QDEB2))

#Use a log of model 10



#Eleventh model (7 predictors)

W8QDEB2.lm.11<-lm(log(W8QDEB2)~ W1condur5MP.1+ IndSchool.1+
                    W1heposs9YP.1+  
                    W6OwnchiDV.1+  W8CMSEX.1 +
                    W8TENURE.1+ W8NETA    , data = data.17)

display(W8QDEB2.lm.11,detail=TRUE)
summary(W8QDEB2.lm.11)
Anova(W8QDEB2.lm.11)
vif(W8QDEB2.lm.11)


#Residual analysis on model 11
plot(W8QDEB2.lm.11)
hist(W8QDEB2.lm.11$residuals, main="histogram", xlab="std. residuals")

#Normality isn't violated in model 11.
#However coefficients are more significant, diagnostics are better and more data points in model 10.

#Use model 10

#Outlier analysis on model 10

show_outliers <- function(W8QDEB2.lm.10, top50) { # length of data
  n = length(fitted(W8QDEB2.lm.10))
  # number of parameters estimated
  p = length(coef(W8QDEB2.lm.10))
  # standardised residuals over 3
  res.out <- which(abs(rstandard(W8QDEB2.lm.10)) > 3) #sometimes >2
  # top50 values
  res.top <- head(rev(sort(abs(rstandard(W8QDEB2.lm.10)))), top50)
  # high leverage values
  lev.out <- which(lm.influence(W8QDEB2.lm.10)$hat > 2 * p/n)
  # top50 values
  lev.top <- head(rev(sort(lm.influence(W8QDEB2.lm.10)$hat)), top50)
  # high diffits
  dffits.out <- which(dffits(W8QDEB2.lm.10) > 2 * sqrt(p/n))
  # top50 values
  dffits.top <- head(rev(sort(dffits(W8QDEB2.lm.10))), top50)
  # Cook's over 1
  cooks.out <- which(cooks.distance(W8QDEB2.lm.10) > 1)
  # top50 cooks
  cooks.top <- head(rev(sort(cooks.distance(W8QDEB2.lm.10))), top50)
  # Create a list with the statistics -- cant do a data frame as different
  # lengths
  list.of.stats <- list(Std.res = res.out, Std.res.top = res.top, Leverage = lev.out,
                        Leverage.top = lev.top, DFFITS = dffits.out, DFFITS.top = dffits.top,
                        Cooks = cooks.out, Cooks.top = cooks.top) # return the statistics
  list.of.stats
}
show_outliers(W8QDEB2.lm.10, 50)


W8QDEB2.lm.10.out<-show_outliers(W8QDEB2.lm.10,50)


print("Standardised residuals")
W8QDEB2.lm.10.out$Std.res
print("Leverage values")
W8QDEB2.lm.10.out$Leverage
print("DFFITS")
W8QDEB2.lm.10.out$DFFITS
print("Cook's d")
W8QDEB2.lm.10.out$Cooks

Reduce(intersect,list(W8QDEB2.lm.10.out$Std.res,W8QDEB2.lm.10.out$DFFITS,W8QDEB2.lm.10.out$Leverage))
#4   35  755  920 1700

data.16[c(4 ,  35 , 755 , 920 , 1700),]

#Cook's distance is zero. Can't justify removing points
#Check for similarities

#Run some cross-validation on the final model

#3 80/20 splits
for(i in 1:3){
  cross.val<-sample(1:nrow(data.16),0.8*nrow(data.16) , replace=FALSE)
  training.set<-data.16[cross.val,]
  test.set<-data.16[-cross.val,]
  
  W8QDEB2.lm.10<-lm(W8QDEB2~ W1condur5MP.1+ IndSchool.1+
                               W1heposs9YP.1+  
                               W6OwnchiDV.1+  W8CMSEX.1 +
                               W8TENURE.1+ W8NETA  , data=training.set)
  
  W8QDEB2.lm.10
  pred.val.set<-data.frame(predicted=predict(W8QDEB2.lm.10,test.set), 
                           original=test.set$W8QDEB2,error=(predict(W8QDEB2.lm.10,test.set)-test.set$W8QDEB2))
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    p1<-p1+geom_smooth(method="lm", se=FALSE) 
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    if(i==2){
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkred") 
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkgreen") 
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)



#Final Model

W8QDEB2.lm.10<-lm(W8QDEB2~ W1condur5MP.1+ IndSchool.1+
                    W1heposs9YP.1+  
                    W6OwnchiDV.1+  W8CMSEX.1 +
                    W8TENURE.1+ W8NETA    , data = data.16)

display(W8QDEB2.lm.10,detail=TRUE)
summary(W8QDEB2.lm.10)
Anova(W8QDEB2.lm.10)
vif(W8QDEB2.lm.10)


par(mfrow=c(1,3))
plot(W8QDEB2.lm.10,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.10$residuals, main="histogram", xlab="std. residuals")

str(data.16)


#For the purpose of the lay report
p10<- ggplot(data.16, aes(y=W8QDEB2, x=W8TENURE.1, colour=factor(W8TENURE.1))) +
  geom_bar(stat='identity',show.legend = FALSE)+coord_flip()+ 
  labs(y='Total amount owed by a 25 year old (£)' , x='Tenure of house ')
p10










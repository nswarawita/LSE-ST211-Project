#EOT Project MLR1

library(arm)
library(car)
library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(tidyr)


data.1<-read.csv("EOTST2112020.csv", header = T)
#data.1 is the original dataset 

head(data.1)
summary(data.1)
str(data.1)
dim(data.1)

#Delete NSID,W4Childck1YP, W6Childliv,W6NEETAct,W8DAGEYCH and W8PUSA
data.2<-data.1[,-c(1,43,55,57,61,70)]
summary(data.2)
str(data.2)
dim(data.2)



##MLR to predict W8DINCW
  
#Delete W8GROW,W8NETW,W8NETA as they are also predictors of income
#Delete W8QDEB2
summary(data.2[,c(63:66)])
data.3<- data.2[,-c(63:66)]
summary(data.3)
str(data.3)
dim(data.3)

#Reformat data

#Removing missing rows in the continuous predictors
missing_continuous<-c(which(data.3$W1GrssyrMP<0),which(data.3$W1GrssyrHH<0),
                      which(data.3$W1yschat1<0),which(data.3$W2ghq12scr<0),
                      which(data.3$W4schatYP<0),which(data.3$W6DebtattYP<0),
                      which(data.3$W8DGHQSC<0))
data.4<-data.3[-missing_continuous,]

summary(data.4)
str(data.4)
dim(data.4) #2355 rows therefore big enough

#Merge the missing values into one level for categorical predictors
data_index.1 <- c(1:63)
for(i in data_index.1){data.4[,i] <- ifelse(data.4[,i]<0,"missing",data.4[,i])}
summary(data.4)
str(data.4)
dim(data.4) 


#Centre the continuous predictors where necessary
summary(data.4[,c(1,2,32,33,41,53,54,56)])
data.4$cent.W1GrssyrMP<-with(data.4,W1GrssyrMP-mean(W1GrssyrMP))
data.4$cent.W1GrssyrHH<-with(data.4,W1GrssyrHH-mean(W1GrssyrHH))
data.4<-data.4[,-c(1,2)]
str(data.4)
dim(data.4)

#Coding categorical variables as factors 
data_index.2<-c(1:63)
summary(data.4[,c(30,31,39,51,52,54,62,63)]) 
categorical_index<-data_index.2[-c(30,31,39,51,52,54,62,63)]
for (i in categorical_index){data.4[,i]<-as.factor(data.4[,i])}
  
str(data.4)

#Changing the baseline for some predictors
data.4$W1hea2MP<-relevel(data.4$W1hea2MP,ref = 2)
data.4$W1hous12HH<-relevel(data.4$W1hous12HH,ref = 2)
data.4$W1hiqualmum<-relevel(data.4$W1hiqualmum,ref = 20)
data.4$W1hiqualdad<-relevel(data.4$W1hiqualdad,ref= "2") #most common is missing so second highest
data.4$W1wrkfullmum<-relevel(data.4$W1wrkfullmum,ref = 2)
data.4$W1empsmum<-relevel(data.4$W1empsmum,ref = 2)
data.4$W1ch12_15HH<-relevel(data.4$W1ch12_15HH,ref = 1)
data.4$W1marstatmum<-relevel(data.4$W1marstatmum, ref = 2)
data.4$W1depkids<-relevel(data.4$W1depkids,ref = 2)
data.4$W1nssecfam<-relevel(data.4$W1nssecfam,ref = 2)
data.4$W1hwndayYP<-relevel(data.4$W1hwndayYP,ref = 3)
data.4$W1truantYP<-relevel(data.4$W1truantYP,ref=2)
data.4$W1alceverYP<-relevel(data.4$W1alceverYP,ref = 1)
data.4$W1bulrc<-relevel(data.4$W1bulrc,ref = 2)
data.4$W1disabYP<-relevel(data.4$W1disabYP,ref = 3)#again
data.4$W2disc1YP<-relevel(data.4$W2disc1YP,ref = 2)
data.4$W4AlcFreqYP<-relevel(data.4$W4AlcFreqYP,ref = "2")
data.4$W4CannTryYP<-relevel(data.4$W4CannTryYP,ref = 2)#
data.4$W4NamesYP<-relevel(data.4$W4NamesYP,ref = 2)
data.4$W4RacismYP<-relevel(data.4$W4RacismYP,ref = 2)
data.4$W4empsYP<-relevel(data.4$W4empsYP,ref = 5)
data.4$W5Apprent1YP<-relevel(data.4$W5Apprent1YP,ref = 2)
data.4$W6UnivYP<-relevel(data.4$W6UnivYP,ref = 2)
data.4$W6EducYP<-relevel(data.4$W6EducYP,ref = 2)
data.4$W6Apprent1YP<-relevel(data.4$W6Apprent1YP,ref = 2)
data.4$W6acqno<-relevel(data.4$W6acqno,ref = 8)
data.4$W6gcse<-relevel(data.4$W6gcse,ref = 4)
data.4$W6als<-relevel(data.4$W6als,ref = 4)
data.4$W6OwnchiDV<-relevel(data.4$W6OwnchiDV,ref = 2)
data.4$W8CMSEX<-relevel(data.4$W8CMSEX,ref = 2)#
data.4$W8TENURE<-relevel(data.4$W8TENURE,ref = 4)
data.4$W8QMAFI<-relevel(data.4$W8QMAFI,ref = 2)

range(data.4$W8DINCW) 
#156.2 480.0



#Initial model - W8DINCW.lm.1 (62 predictors)
W8DINCW.lm.1<-lm(W8DINCW~.,data = data.4)
display(W8DINCW.lm.1,detail=TRUE)
summary(W8DINCW.lm.1)
Anova(W8DINCW.lm.1)



#Model 2 - W8DINCW.lm.2 (19 predictors)

#None of the continuous predictors are significant. So use data.3
#So no need to center the continuous predictors 

str(data.3)
dim(data.3)#Complete data set, 5792 Rows

#For categorical predictors - merge the missing values into one category
summary(data.3[,c(1,2,32,33,41,53,54,56)])
data_index.2 <- c(1:63)
categorical_index<-data_index.2[-c(1,2,32,33,41,53,54,56)]
for(i in data_index.1){data.3[,i] <- ifelse(data.3[,i]<0,"missing",data.3[,i])
}
str(data.3)
dim(data.3)


#Coding categorical variables as factors 
data_index.2<-c(1:63)
summary(data.3[,c(1,2,32,33,41,53,54,56)]) 
categorical_index<-data_index.2[-c(1,2,32,33,41,53,54,56)]
for (i in categorical_index){data.3[,i]<-as.factor(data.3[,i])}
summary(data.3)
str(data.3)
dim(data.3) 
range(data.3$W8DINCW)

#Changing the baseline for some predictors
#Most common is set as the reference level
data.3$W1hea2MP<-relevel(data.3$W1hea2MP,ref = 2)
data.3$W1hous12HH<-relevel(data.3$W1hous12HH,ref = 2)
data.3$W1hiqualmum<-relevel(data.3$W1hiqualmum,ref = 15)
data.3$W1marstatmum<-relevel(data.3$W1marstatmum, ref = 2)
data.3$W1nssecfam<-relevel(data.3$W1nssecfam,ref = 2)
data.3$W1hwndayYP<-relevel(data.3$W1hwndayYP,ref = 3)
data.3$W1disabYP<-relevel(data.3$W1disabYP,ref = 3)#again
data.3$W2disc1YP<-relevel(data.3$W2disc1YP,ref = 2)
data.3$W4AlcFreqYP<-relevel(data.3$W4AlcFreqYP,ref = "2") # missing is the most occuring
data.3$W4CannTryYP<-relevel(data.3$W4CannTryYP,ref = 2)#
data.3$W6Apprent1YP<-relevel(data.3$W6Apprent1YP,ref = 2)
data.3$W8CMSEX<-relevel(data.3$W8CMSEX,ref = 2)#
summary(data.3$W1hwndayYP)

data.3$W1hwndayYP<- factor(data.3$W1hwndayYP,
                          levels = c("3","0" ,"1", "2","4","5","missing"))

data.3$W1hiqualmum<- factor(data.3$W1hiqualmum,
  levels = c("15","1", "2","3","4","5","6","7","8","9","10","11","12","13",
             "14","16","17","18","19","20","missing"))

data.3$W1hous12HH<- factor(data.3$W1hous12HH,
                            levels = c("2","1","3","4","5","6","7","8","missing"))

data.3$W8DDEGP<- factor(data.3$W8DDEGP,
                           levels = c("1","0","missing"))


W8DINCW.lm.2<-lm(W8DINCW~W1wrk1aMP+W1hea2MP+W1hous12HH+W1hiqualmum+W1marstatmum+
                   W1famtyp2+W1nssecfam+W1ethgrpYP+W1heposs9YP+W1hwndayYP+
                   W1disabYP+W2disc1YP+W4AlcFreqYP+W4CannTryYP+W5EducYP+W6Apprent1YP+
                   W8DDEGP+W8DMARSTAT+W8CMSEX ,data = data.3)

display(W8DINCW.lm.2,detail=TRUE)
summary(W8DINCW.lm.2)
Anova(W8DINCW.lm.2)
#Remove W1famtyp2 and W8DMARSTAT

vif(W8DINCW.lm.2)
#Remove W1wrk1aMP(GVIF=18), W1marstatmum(GVIF=33) and W1famtyp2(GVIF=18)


#Third model(15 Predictors)

W8DINCW.lm.3<-lm(W8DINCW~W1hea2MP+W1hous12HH+W1hiqualmum+
                   W1nssecfam+W1ethgrpYP+W1heposs9YP+W1hwndayYP+
                   W1disabYP+W2disc1YP+W4AlcFreqYP+W4CannTryYP+W5EducYP+W6Apprent1YP+
                   W8DDEGP+W8CMSEX ,data = data.3)
display(W8DINCW.lm.3,detail=TRUE)
summary(W8DINCW.lm.3)
Anova(W8DINCW.lm.3)
vif(W8DINCW.lm.3)

#Boxplots of all the categorical predictors

#W1hea2MP (MP: Whether have long-standing illness, disability or infirmity)
data.3$W1hea2MP.1<- factor(data.3$W1hea2MP,
                          levels = c("2","1","missing"),
                          labels = c("No", "Yes","Missing"))
W1hea2MP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W1hea2MP.1), y=W8DINCW,
                                           colour=factor(W1hea2MP.1)))+geom_boxplot()
W1hea2MP.1_boxplot
summary(data.3$W1hea2MP.1)

#W1hous12HH (HH: Tenure)
data.3$W1hous12HH.1 <- factor(data.3$W1hous12HH,
                            levels = c("2","1","3","4","5","6","7","8","missing"),
                            labels = c("Bought on a mortgage","Owned outright","Shared ownership",
                                       "Rented from Council","Rented from Housing Association","Rented privately",
                                       "Rent free","Some other arrangement","Missing"))
W1hous12HH.1_boxplot <- ggplot(data=data.3,aes(x=factor(W1hous12HH.1), y=W8DINCW,
                                             colour=factor(W1hous12HH.1)))+geom_boxplot()
W1hous12HH.1_boxplot
#Bought on a mortgage and Rented from a housing association have outliers 
#highest is other  and lowest is for rent free
summary(data.3$W1hous12HH.1)

#W1hiqualmum
W1hiqualmum_boxplot <- ggplot(data=data.3,aes(x=factor(W1hiqualmum), y=W8DINCW,
                                              colour=factor(W1hiqualmum)))+geom_boxplot()
W1hiqualmum_boxplot
data.3$W1hiqualmum<- factor(data.3$W1hiqualmum,
                            levels = c("15","1", "2","3","4","5","6","7","8","9","10","11","12","13",
                                       "14","16","17","18","19","20","missing"))

summary(data.3$W1hiqualmum)

#W1nssecfam (DV: Family's NS-SEC class (from household reference person))
data.3$W1nssecfam.1 <- factor(data.3$W1nssecfam,
                            levels = c("2","1","3","4","5","6","7","8","missing"),
                            labels = c("Lower managerial", "Higher managerial","Intermediate",
                                       "Small employers","Technical","Semi-routine",
                                       "Routine","Unemployed","Missing"))
W1nssecfam.1_boxplot <- ggplot(data=data.3,aes(x=factor(W1nssecfam.1), y=W8DINCW,
                                             colour=factor(W1nssecfam.1)))+geom_boxplot()
W1nssecfam.1_boxplot
summary(data.3$W1nssecfam.1)
#higher managerial and unemployed have outliers. 
#median of 6 and 7 are similar-merge?
#Lots of outliers for unemployed-wealth so rent income,other income

#W1ethgrpYP (DV: Young person's ethnic group (grouped))
data.3$W1ethgrpYP.1 <- factor(data.3$W1ethgrpYP,
                            levels = c("1","2","3","4","5","6","7","8","missing"),
                            labels = c("White", "Mixed","Indian",
                                       "Pakistani","Bangladeshi","Black Caribbean",
                                       "Black African","Other","Missing"))
W1ethgrpYP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W1ethgrpYP.1), y=W8DINCW,
                                             colour=factor(W1ethgrpYP.1)))+geom_boxplot()
W1ethgrpYP.1_boxplot
#medians of 4,5,6,7 are similar
#1 missing data Why is it higher than the others? Outlier?
#1(white is clearly higher) than the others
#outliers in 1,3 and 8. Also outliers are below Q1
#merge other and missing
summary(data.3$W1ethgrpYP.1)

#W1heposs9YP (YP: Likelihood of YP applying for university)
data.3$W1heposs9YP.1 <- factor(data.3$W1heposs9YP,
                             levels = c("1","2","3","4","missing"),
                             labels = c("Very likely", "Fairly likely","Not very likely",
                                        "Not at all likely","Missing"))
W1heposs9YP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W1heposs9YP.1), y=W8DINCW,
                                              colour=factor(W1heposs9YP.1)))+geom_boxplot()
W1heposs9YP.1_boxplot
#median of very likely and fairly likely similar-merge?
summary(data.3$W1heposs9YP.1)

#W1hwndayYP (YP: Number of evenings do homework)
data.3$W1hwndayYP<- factor(data.3$W1hwndayYP,
                           levels = c("3","0" ,"1", "2","4","5","missing"))

W1hwndayYP_boxplot <- ggplot(data=data.3,aes(x=factor(W1hwndayYP), y=W8DINCW,
                                             colour=factor(W1hwndayYP)))+geom_boxplot()
W1hwndayYP_boxplot
summary(data.3$W1hwndayYP)

#W1disabYP (DV: Whether young person has a disability/long term illness or health problem)
data.3$W1disabYP.1 <- factor(data.3$W1disabYP,
                           levels = c("3","1","2","missing"),
                           labels = c("No","Yes;schooling affected", "Yes;schooling not affected","Missing"))
W1disabYP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W1disabYP.1), y=W8DINCW,
                                            colour=factor(W1disabYP.1)))+geom_boxplot()
W1disabYP.1_boxplot
summary(data.3$W1disabYP.1)
#Yes, schooling not affected and No have a similar median

#W2disc1YP (YP: Whether YP thinks ever been treated unfairly by teachers because of skin colour or ethnic origin)
data.3$W2disc1YP.1 <- factor(data.3$W2disc1YP,
                           levels = c("2","1","missing"),
                           labels = c("No","Yes","Missing"))
W2disc1YP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W2disc1YP.1), y=W8DINCW,
                                            colour=factor(W2disc1YP.1)))+geom_boxplot()
W2disc1YP.1_boxplot
#1 and missing have similar medians, 1 has outliers higher than Q3

#W4AlcFreqYP (YP: Frequency of YP having alcoholic drink in last 12 months)
data.3$W4AlcFreqYP.1 <- factor(data.3$W4AlcFreqYP,
                             levels = c("2","1","3","4","5","6","missing"),
                             labels = c("1-2 times a week","Most days", "2-3 times a month",
                                        "Once a month","Once every 2 months","Less often",
                                        "Missing"))
W4AlcFreqYP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W4AlcFreqYP.1), y=W8DINCW,
                                              colour=factor(W4AlcFreqYP.1)))+geom_boxplot()
W4AlcFreqYP.1_boxplot
#median for missing is much lower than the others 
#1-4 has similar medians
#outliers for 2,3 and missing

#W4CannTryYP (YP: Whether YP ever tried Cannabis)
summary(data.3$W4CannTryYP)
data.3$W4CannTryYP.1 <- factor(data.3$W4CannTryYP,
                             levels = c("2","1","missing"),
                             labels = c("No", "Yes","Missing"))
W4CannTryYP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W4CannTryYP.1), y=W8DINCW,
                                              colour=factor(W4CannTryYP.1)))+geom_boxplot()
W4CannTryYP.1_boxplot
#No has a lower median than Yes-attributed to the number of people who answered?
#medians of yes and missing are similar

#W5EducYP (YP: Whether currently going to school or college)
summary(data.3$W5EducYP)
data.3$W5EducYP.1 <- factor(data.3$W5EducYP,
                          levels = c("1","2","missing"),
                          labels = c("Yes", "No","Missing"))
W5EducYP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W5EducYP.1), y=W8DINCW,
                                           colour=factor(W5EducYP.1)))+geom_boxplot()
W5EducYP.1_boxplot
#as expected if yes,income is higher

#W6Apprent1YP (YP: Whether currently doing an Apprenticeship)
summary(data.3$W6Apprent1YP)
data.3$W6Apprent1YP.1 <- factor(data.3$W6Apprent1YP,
                              levels = c("2","1","missing"),
                              labels = c("No", "Yes","Missing"))
W6Apprent1YP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W6Apprent1YP.1), y=W8DINCW,
                                               colour=factor(W6Apprent1YP.1)))+geom_boxplot()
W6Apprent1YP.1_boxplot
#median of missing is higher,similar IQR for all 3
#Similar medians for yes and no
summary(data.3$W6Apprent1YP.1)

#W8DDEGP (DV: Whether achieved first degree or higher)
summary(data.3$W8DDEGP)
data.3$W8DDEGP.1 <- factor(data.3$W8DDEGP,
                         levels = c("1","0","missing"),
                         labels = c("First or higher degree","No degree", "Missing"))
W8DDEGP.1_boxplot <- ggplot(data=data.3,aes(x=factor(W8DDEGP.1), y=W8DINCW,
                                          colour=factor(W8DDEGP.1)))+geom_boxplot()
W8DDEGP.1_boxplot
#similar medians for no degree and missing
#as expected higher income for people with a first or higher degree

#W8CMSEX (Gender)
data.3$W8CMSEX.1 <- factor(data.3$W8CMSEX,
                         levels = c("2","1"),
                         labels = c("Female", "Male"))
data.3$W8CMSEX.1<-relevel(data.3$W8CMSEX.1,ref="Female")
summary(data.3$W8CMSEX.1)
W8CMSEX.1_boxplot <- ggplot(data=data.3,aes(x=factor(W8CMSEX.1), y=W8DINCW,
                                          colour=factor(W8CMSEX.1)))+geom_boxplot()
W8CMSEX.1_boxplot
#Female is higher than male
str(data.3)

W8DINCW.lm.3<-lm(W8DINCW~W1hea2MP.1+W1hous12HH.1+W1hiqualmum+
                   W1nssecfam.1+W1ethgrpYP.1+W1heposs9YP.1+W1hwndayYP+
                   W1disabYP.1+W2disc1YP.1+W4AlcFreqYP.1+W4CannTryYP.1+W5EducYP.1+W6Apprent1YP.1+
                   W8DDEGP.1+W8CMSEX.1 ,data = data.3)
display(W8DINCW.lm.3,detail=TRUE)
summary(W8DINCW.lm.3)
Anova(W8DINCW.lm.3)

str(data.3)


#Merge levels of predictors where necessary

#W1hous12HH
#Rented from council and rented from h association, ??rent free and other
W1hous12HH.1_boxplot
data.3$W1hous12HH.2<-with(data.3,Recode(W1hous12HH.1,"c('Rent free','Missing')='Rent free or missing'"))
data.3$W1hous12HH.3<-with(data.3,Recode(W1hous12HH.2,"c('Rented from Council','Rented from Housing Association')='Rented from other'"))
data.3$W1hous12HH.3<- factor(data.3$W1hous12HH.3,
                           levels = c("Bought on a mortgage", "Owned outright", "Rented privately","Rented from other",
                                      "Rent free or missing","Shared ownership","Some other arrangement"))
W1hous12HH.3_boxplot <- ggplot(data=data.3,aes(x=factor(W1hous12HH.3), y=W8DINCW,
                                             colour=factor(W1hous12HH.3)))+geom_boxplot()
W1hous12HH.3_boxplot
summary(data.3$W1hous12HH.3)

#W1hiqualmum
#1 & 2-First degree
summary(data.3$W1hiqualmum)
W1hiqualmum_boxplot

data.3$W1hiqualmum.1 <- factor(data.3$W1hiqualmum,
                              levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13",
                              "14","15","16","17","18","19","20","missing"),
        labels = c("Higher degree", "First degree or HE diploma","First degree or HE diploma",
        "HNC/HND/NVQ4","Non-degree qualifications","Non-degree qualifications",
        "A Levels or equivalent","A Levels or equivalent","A Levels or equivalent",
        "A Levels or equivalent","AS Levels or equivalent","AS Levels or equivalent",
        "GCSEs or equivalent","GCSEs or equivalent","GCSEs or equivalent",
        "GCSEs or equivalent","Other","Other",
        "Other","No qualifications","Missing"))

#For A/l or equivalent means weren't similar

data.3$W1hiqualmum.1<-as.factor(data.3$W1hiqualmum.1)
data.3$W1hiqualmum.1<-relevel(data.3$W1hiqualmum.1,ref="GCSEs or equivalent")
data.3$W1hiqualmum.2<- factor(data.3$W1hiqualmum.1,
    levels = c("GCSEs or equivalent","Higher degree" ,"First degree or HE diploma", "HNC/HND/NVQ4",
                "Non-degree qualifications","A Levels or equivalent","AS Levels or equivalent",
                 "Other","No qualifications","Missing"))

W1hiqualmum.2_boxplot <- ggplot(data=data.3,aes(x=factor(W1hiqualmum.1), y=W8DINCW,
                                               colour=factor(W1hiqualmum.1)))+geom_boxplot()
W1hiqualmum.2_boxplot

#W1nssecfam
#Small employers and technical, Routine and semi routine
W1nssecfam.1_boxplot
data.3$W1nssecfam.2<-with(data.3,Recode(W1nssecfam.1,"c('Small employers','Technical')='Small employers or technical'"))
data.3$W1nssecfam.3<-with(data.3,Recode(W1nssecfam.2,"c('Routine','Semi-routine')='Routine or Semi-routine'"))
data.3$W1nssecfam.3<-relevel(data.3$W1nssecfam.3,ref="Lower managerial")
data.3$W1nssecfam.3<- factor(data.3$W1nssecfam.3,
                           levels = c("Lower managerial", "Higher managerial", "Intermediate","Small employers or technical",
                                      "Routine or Semi-routine","Unemployed","Missing"))
W1nssecfam.3_boxplot <- ggplot(data=data.3,aes(x=factor(W1nssecfam.3), y=W8DINCW,
                                             colour=factor(W1nssecfam.3)))+geom_boxplot()
W1nssecfam.3_boxplot
summary(data.3$W1nssecfam.1)
summary(data.3$W1nssecfam.3)

#W1ethgrpYP
#Pakistani and Bangladeshi-Other S Asian,Black Caribbean and Black African-Black, Other and Missing-Other
W1ethgrpYP_boxplot
summary(data.3$W1ethgrpYP.1)
data.3$W1ethgrpYP.2<-with(data.3,Recode(W1ethgrpYP.1,"c('Pakistani','Bangladeshi')='Other South Asian'"))
data.3$W1ethgrpYP.3<-with(data.3,Recode(W1ethgrpYP.2,"c('Black Caribbean','Black African')='Black'"))
data.3$W1ethgrpYP.4<-with(data.3,Recode(W1ethgrpYP.3,"c('Other','Missing')='Other'"))
data.3$W1ethgrpYP.4<-relevel(data.3$W1ethgrpYP.4,ref="White")
data.3$W1ethgrpYP.4<- factor(data.3$W1ethgrpYP.4,
                           levels = c("White", "Mixed", "Indian","Other South Asian",
                                      "Black","Other"))
W1ethgrpYP.4_boxplot <- ggplot(data=data.3,aes(x=factor(W1ethgrpYP.4), y=W8DINCW,
                                             colour=factor(W1ethgrpYP.4)))+geom_boxplot()
W1ethgrpYP.4_boxplot
str(data.3)
summary(data.3$W1ethgrpYP.4)

#W1heposs9YP
#Very likely and fairly likely
W1heposs9YP.1_boxplot
data.3$W1heposs9YP.2<- gsub('Very likely', 'Somewhat likely', data.3$W1heposs9YP.1)
data.3$W1heposs9YP.3<- gsub('Fairly likely', 'Somewhat likely', data.3$W1heposs9YP.2)
data.3$W1heposs9YP.3<-as.factor(data.3$W1heposs9YP.3)
data.3$W1heposs9YP.3<-relevel(data.3$W1heposs9YP.3, ref="Somewhat likely") 
W1heposs9YP.3_boxplot <- ggplot(data=data.3,aes(x=factor(W1heposs9YP.3), y=W8DINCW,
                                             colour=factor(W1heposs9YP.3)))+geom_boxplot()
W1heposs9YP.3_boxplot
summary(data.3$W1heposs9YP.3)

#W4AlcFreqYP
#1-2 times a week,2-3 times a month and once a month
W4AlcFreqYP.1_boxplot
data.3$W4AlcFreqYP.2<-with(data.3,Recode(W4AlcFreqYP.1,"c('1-2 times a week','2-3 times a month',
                                  'Once a month')='Once a week-Once a month'"))
data.3$W4AlcFreqYP.2<-relevel(data.3$W4AlcFreqYP.2,ref="Once a week-Once a month")
data.3$W4AlcFreqYP.2<- factor(data.3$W4AlcFreqYP.2,
                           levels = c("Once a week-Once a month", "Most days", "Once every 2 months",
                                      "Less often","Missing"))
W4AlcFreqYP.2_boxplot <- ggplot(data=data.3,aes(x=factor(W4AlcFreqYP.2), y=W8DINCW,
                                              colour=factor(W4AlcFreqYP.2)))+geom_boxplot()
W4AlcFreqYP.2_boxplot
summary(data.3$W4AlcFreqYP.2)

str(data.3)



#Model 4 (15 predictors)
W8DINCW.lm.4<-lm(W8DINCW~W1hea2MP.1+W1hous12HH.3+W1hiqualmum.2+
                   W1nssecfam.3+W1ethgrpYP.4+W1heposs9YP.3+W1hwndayYP+
                   W1disabYP.1+W2disc1YP.1+W4AlcFreqYP.2+W4CannTryYP.1+W5EducYP.1+W6Apprent1YP.1+
                   W8DDEGP.1+W8CMSEX.1 ,data = data.3)
display(W8DINCW.lm.4,detail=TRUE)
summary(W8DINCW.lm.4)
Anova(W8DINCW.lm.4)

#Residual analysis for model 4
par(mfrow=c(2,2))
plot(W8DINCW.lm.4)
hist(rstandard(W8DINCW.lm.4,freq=TRUE,
               main="Histogram of standardised residuals",cex.main=0.8,xlab="Standardised residuals"))

#Slight kurtosis-tails are fatter
#Funnel shape, therefore heteroscedasticity-not a constant variance
#Normality holds

range(data.3$W8DINCW) #115.51 - 491.71
#Can use a log transform cos funnel shape and income is positive



#Model 5 (15 Predictors)

str(data.3)
W8DINCW.lm.5<-lm(log(W8DINCW)~W1hea2MP.1+W1hous12HH.3+W1hiqualmum.2+
                   W1nssecfam.3+W1ethgrpYP.4+W1heposs9YP.3+W1hwndayYP+
                   W1disabYP.1+W2disc1YP.1+W4AlcFreqYP.2+W4CannTryYP.1+W5EducYP.1+W6Apprent1YP.1+
                   W8DDEGP.1+W8CMSEX.1 ,data = data.3)


display(W8DINCW.lm.5,detail=TRUE)
summary(W8DINCW.lm.5)
Anova(W8DINCW.lm.5)

range(log(data.3$W8DINCW))#4.749357 6.197889

par(mfrow=c(2,2))
plot(W8DINCW.lm.5)
hist(rstandard(W8DINCW.lm.5,freq=TRUE,
               main="Histogram of standardised residuals",cex.main=0.8,xlab="Standardised residuals"))

#Outlier analysis for model 5
show_outliers <- function(W8DINCW.lm.5, top50) { 
  # length of data
  n = length(fitted(W8DINCW.lm.5))
  # number of parameters estimated
  p = length(coef(W8DINCW.lm.5))
  # standardised residuals over 3
  res.out <- which(abs(rstandard(W8DINCW.lm.5)) > 3) #sometimes >2
  # topN values
  res.top <- head(rev(sort(abs(rstandard(W8DINCW.lm.5)))), top50)
  # high leverage values
  lev.out <- which(lm.influence(W8DINCW.lm.5)$hat > 2 * p/n)
  # topN values
  lev.top <- head(rev(sort(lm.influence(W8DINCW.lm.5)$hat)), top50)
  # high diffits
  dffits.out <- which(dffits(W8DINCW.lm.5) > 2 * sqrt(p/n))
  # topN values
  dffits.top <- head(rev(sort(dffits(W8DINCW.lm.5))), top50)
  # Cook's over 1
  cooks.out <- which(cooks.distance(W8DINCW.lm.5) > 1)
  # topN cooks
  cooks.top <- head(rev(sort(cooks.distance(W8DINCW.lm.5))), top50)
  # Create a list with the statistics -- cant do a data frame as different
  # lengths
  list.of.stats <- list(Std.res = res.out, Std.res.top = res.top, Leverage = lev.out,
                        Leverage.top = lev.top, DFFITS = dffits.out, DFFITS.top = dffits.top,
                        Cooks = cooks.out, Cooks.top = cooks.top) # return the statistics
  list.of.stats
}

show_outliers(W8DINCW.lm.5, 50)


W8DINCW.lm.5.out<-show_outliers(W8DINCW.lm.5,50)


print("Standardised residuals")
W8DINCW.lm.5.out$Std.res
print("Leverage values")
W8DINCW.lm.5.out$Leverage
print("DFFITS")
W8DINCW.lm.5.out$DFFITS
print("Cook's d")
W8DINCW.lm.5.out$Cooks

Reduce(intersect,list(W8DINCW.lm.5.out$Std.res,W8DINCW.lm.5.out$DFFITS,W8DINCW.lm.5.out$Leverage))
# 1597 and 5552

#Cook's distances are all way below 1. Therefore difficult to justify removing an outlier.
#Try to iddentify common elements

data.3[c(1597,5552),]
#Point 5552 has a lot of missing data

common.out<-intersect(intersect(W8DINCW.lm.5.out$Std.res,W8DINCW.lm.5.out$DFFITS),W8DINCW.lm.5.out$Leverage)
common.out  
#1597 and 5552




#Final model (Model 5)

W8DINCW.lm.5<-lm(log(W8DINCW)~W1hea2MP.1+W1hous12HH.3+W1hiqualmum.2+
                   W1nssecfam.3+W1ethgrpYP.4+W1heposs9YP.3+W1hwndayYP+
                   W1disabYP.1+W2disc1YP.1+W4AlcFreqYP.2+W4CannTryYP.1+W5EducYP.1+W6Apprent1YP.1+
                   W8DDEGP.1+W8CMSEX.1 ,data = data.3)
Anova(W8DINCW.lm.5)
summary(W8DINCW.lm.5)
display(W8DINCW.lm.5,detail=TRUE)

par(mfrow=c(1,3))
plot(W8DINCW.lm.5,which=c(1,2), cex=0.7, pch=".")
hist(W8DINCW.lm.5$residuals, main="histogram", xlab="std. residuals")






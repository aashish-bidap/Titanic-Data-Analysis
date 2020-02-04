library(ggplot2)
library(gridExtra)
library(dbplyr)
library(dplyr)
library(tidyr)
#data importing
my_titanic <- read.csv("/Users/abhishekbidap/Downloads/train.csv")
my_titanic_test <- read.csv("/Users/abhishekbidap/Downloads/test.csv")

#combine test and train dataset
colnames(my_titanic_test)
colnames(my_titanic)

#assigned value of 2 in order to distinquish between the two sets of data and combinig thr 2 datasets.
my_titanic_test$Survived <- 2
my_titanic_merged <- rbind(my_titanic,my_titanic_test)
dim(my_titanic_merged)

#data overview
head(my_titanic_merged)
#checking datatypes
sapply(my_titanic_merged,class)
#dimensions check
dim(my_titanic_merged)
#summary of data 
summary(my_titanic_merged)
#checking for missing values
colSums(is.na(my_titanic_merged))

my_titanic_merged[which(my_titanic$Embarked==""),]

##########################################
#######------NULL VALUES FIXING
##########################################
#checking for NA values
#1.Age
colSums(is.na(my_titanic_merged))

#2.Embarked 
#as the null values in embarked were as blank R wasn't able to identify them changed the blank to NA
my_titanic_merged$Embarked = as.character(my_titanic_merged$Embarked)
my_titanic_merged$Embarked[my_titanic_merged$Embarked ==""] <- NA
colSums(is.na(my_titanic_merged))
#Null values fixing
#Age -> mean of column
my_titanic_merged[is.na(my_titanic_merged[,6]), 6] <- round(mean(my_titanic_merged$Age[!is.na(my_titanic_merged$Age)]))

#emarked fixing
unique(my_titanic_merged$Embarked)
table(my_titanic_merged$Embarked)
my_titanic_merged[is.na(my_titanic_merged[,12]),12] <- 'S'

#fixing fare in test dataset
my_titanic_merged$Fare[is.na(my_titanic_merged$Fare)] <- round(mean(my_titanic_merged$Fare[!is.na(my_titanic_merged$Fare)]))


#Age binning 
summary(my_titanic_merged$Age)
my_titanic_merged <- mutate(my_titanic_merged,Age_bin=Age)
my_titanic_merged$Age_bin[my_titanic_merged$Age_bin >= 0 & my_titanic_merged$Age_bin <= 9.99]   <- "0 - 9 years"
my_titanic_merged$Age_bin[my_titanic_merged$Age_bin >= 10 & my_titanic_merged$Age_bin <= 19.99]  <- "10 - 19 years"
my_titanic_merged$Age_bin[my_titanic_merged$Age_bin >= 20 & my_titanic_merged$Age_bin <= 29.99]  <- "20 - 29 years"
my_titanic_merged$Age_bin[my_titanic_merged$Age_bin >= 30 & my_titanic_merged$Age_bin <= 39.99]  <- "30 - 39 years"
my_titanic_merged$Age_bin[my_titanic_merged$Age_bin >= 40 & my_titanic_merged$Age_bin <= 49.99]  <- "40 - 49 years"
my_titanic_merged$Age_bin[my_titanic_merged$Age_bin >= 50 & my_titanic_merged$Age_bin <= 59.99]  <- "50 - 59 years"
my_titanic_merged$Age_bin[my_titanic_merged$Age_bin >= 60 & my_titanic_merged$Age_bin <= 69.99]  <- "60 - 69 years"
my_titanic_merged$Age_bin[my_titanic_merged$Age_bin >= 70 & my_titanic_merged$Age_bin <= 80.99]  <- "70 - 80 years"

table(my_titanic_merged$Age_bin)

#data transformation (NAME -> TITLE)
my_titanic_merged["Name"]
names <- my_titanic_merged$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)
my_titanic_merged$title <- title 

table(my_titanic_merged$title)

my_titanic_merged$title[title == 'Capt'] <- 'Officer'
my_titanic_merged$title[title == 'Col']<- 'Officer'
my_titanic_merged$title[title == 'Don'] <- 'Mr'
my_titanic_merged$title[title == 'Dona']<-'Mrs'
my_titanic_merged$title[title == 'Dr']<-'Officer'
my_titanic_merged$title[title == 'Lady']<- 'Mrs'
my_titanic_merged$title[title == 'Major']<-'Officer'
my_titanic_merged$title[title == 'Mlle']<- 'Miss'
my_titanic_merged$title[title == 'Mme']<- 'Miss'
my_titanic_merged$title[title == 'Rev']<- 'Mr'
my_titanic_merged$title[title == 'Sir']<-'Officer'
my_titanic_merged$title[title == 'the Countess'] <- 'Officer'
my_titanic_merged$title[title == 'Jonkheer'] <- 'Officer'
my_titanic_merged$title[title == 'Ms'] <- 'Miss'

table(my_titanic_merged$title)

#changing the sex into 0,1
my_titanic_merged$Sex <- as.character(my_titanic_merged$Sex)
my_titanic_merged$Sex[which(my_titanic_merged$Sex == "male")] <- 0
my_titanic_merged$Sex[which(my_titanic_merged$Sex == "female")] <- 1

#changinf embarked into numeric
table(my_titanic_merged$Embarked)
my_titanic_merged$Embarked[which(my_titanic_merged$Embarked == 'C')] <- 1
my_titanic_merged$Embarked[which(my_titanic_merged$Embarked == 'Q')] <- 2
my_titanic_merged$Embarked[which(my_titanic_merged$Embarked == 'S')] <- 3 

head(my_titanic_merged)

#one hot encode the title column
title_ <- factor(my_titanic_merged$title)
dumm <- as.data.frame(model.matrix(~title_)[,-1])
my_titanic_merged <- cbind(dumm,my_titanic_merged)

#one hot encode the age_bin column
Agebin_ <- factor(my_titanic_merged$Age_bin)
dumm <- as.data.frame(model.matrix(~Agebin_)[,-1])
my_titanic_merged <- cbind(dumm,my_titanic_merged)

#one hot encode the title column
Embarked_ <- factor(my_titanic_merged$Embarked)
dumm <- as.data.frame(model.matrix(~Embarked_)[,-1])
my_titanic_merged <- cbind(dumm,my_titanic_merged)

head(my_titanic_merged)
###----------
#Plotting of column

plot_a <- ggplot(data = my_titanic,aes(x=Survived)) + geom_bar(stat = "count",width = 0.4,fill="Steelblue")
plot_b <- ggplot(data = my_titanic,aes(x=Sex)) + geom_bar(stat = "count",width = 0.4,fill="Steelblue")
plot_c <- ggplot(data = my_titanic,aes(x=Pclass)) + geom_bar(stat = "count",width = 0.4,fill="Steelblue")
plot_d <- ggplot(data = my_titanic,aes(x=Embarked)) + geom_bar(stat = "count",width = 0.4,fill="Steelblue")
plot_e <- ggplot(data = my_titanic,aes(x=SibSp)) + geom_bar(stat = "count",width = 0.4,fill="Steelblue") 
plot_f <- ggplot(data = my_titanic,aes(x=Age_bin)) + geom_bar(stat = "count",width = 0.4,fill="Steelblue")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f)

#Fare vs other variables
plot_1 <- ggplot(data = my_titanic,aes(x=Sex,y=Fare)) + geom_bar(stat="identity",fill="Steelblue",width = 0.4)
plot_2 <-ggplot(data = my_titanic,aes(x=Pclass,y=Fare)) + geom_bar(stat="identity",fill="Steelblue",width = 0.4)
plot_3 <-ggplot(data = my_titanic,aes(x=SibSp,y=Fare)) + geom_bar(stat="identity",fill="Steelblue",width = 0.4)
plot_4 <-ggplot(data = my_titanic,aes(x=Parch,y=Fare)) + geom_bar(stat="identity",fill="Steelblue",width = 0.4)
plot_5 <-ggplot(data = my_titanic,aes(x=Embarked,y=Fare)) + geom_bar(stat="identity",fill="Steelblue",width = 0.4)
plot_6 <-ggplot(data = my_titanic,aes(x=Age_bin,y=Fare)) + geom_bar(stat="identity",fill="Steelblue",width = 0.4)+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
grid.arrange(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6)

###############
###------------
###_###########

#fixing the datatyopes before doing the modelling
sapply(my_titanic_merged,class)
my_titanic_merged$Survived <- as.factor(my_titanic_merged$Survived)
my_titanic_merged$Pclass <- as.factor(my_titanic_merged$Pclass)
my_titanic_merged$Sex <- as.factor(my_titanic_merged$Sex)
#--------
colnames(my_titanic_merged)
final_data <- my_titanic_merged[,-c(14,17,19,22,24,25,26,27)]

#spllitng the data
titanic_data = final_data[which(final_data$Survived != 2),]
titanic_test_data = final_data[which(final_data$Survived == 2),]

droplevels(titanic_data$Survived)
droplevels(titanic_test_data$Survived)
sapply(titanic_data,class)

#spliiting the titanic_data
num =sample(2,nrow(titanic_data),replace=T,prob=c(0.7,0.3))
train_data = titanic_data[num==1,]
test_data = titanic_data[num==2,]

#logistic modelling
log_model <- glm(Survived ~.,family=binomial,train_data)
summary(log_model)
predict_log_model <- predict(log_model,newdata=test_data)
y_pred_num <- ifelse(predict_log_model > 0.5, 1, 0)
table(y_pred_num)
y_observed <- test_data$Survived
Accuracy_LM <- mean(y_observed == y_pred_num)
Accuracy_LM

titanic_test_data$Survived <- NULL
predict_test_data <- predict(log_model,newdata=titanic_test_data)
final_SUbmission_values <- ifelse(predict_test_data > 0.5, 1, 0)

final_SUbmission_values

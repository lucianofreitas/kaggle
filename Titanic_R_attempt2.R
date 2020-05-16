#1 Reading Train and Test files into R

titanic.train2<-read.csv(file = "train.csv", stringsAsFactors = FALSE, header=TRUE)
titanic.test2<-read.csv(file = "test.csv", stringsAsFactors = FALSE, header=TRUE)

#2 Designating Train and non-Train Datasets

titanic.train2$IsTrainSet<-TRUE
titanic.test2$IsTrainSet<-FALSE

#3 Binding Tables "Test" and "Train"

names(titanic.test2)
names(titanic.train2)
ncol(titanic.test2)
ncol(titanic.train2)

#4 Creating a column "Survived" and filling it up with NA in the Test Table

titanic.test2$Survived<-NA


#5 Binding Tables Test and Train Together

titanic.full2<-rbind(titanic.train2,titanic.test2)
ncol(titanic.full2)


#6 Examining and Cleaning Datasets
##6.1 Replace empty by NA in Embarked Collumn

titanic.full2[titanic.full2$Embarked=='',"Embarked"]<-'NA'

##6.2 Replace empty by Median in Age Collumn

age.median<-median(titanic.full2$Age,na.rm=TRUE)
titanic.full2[is.na(titanic.full2$Age),"Age"]<-age.median

median(titanic.full2$Age)


##6.3 Replace empty by Median in Fare Collumn
is.na(titanic.full2$Fare)

fare.median<-median(titanic.full2$Fare,na.rm=TRUE)
titanic.full2[is.na(titanic.full2$Fare),"Fare"]<-fare.median

median(titanic.full2$Fare)

#7 Categorical Casting
titanic.full2$Pclass<- as.factor(titanic.full2$Pclass)
titanic.full2$Sex<-as.factor(titanic.full2$Sex)
titanic.full2$Embarked<-as.factor(titanic.full2$Embarked)
titanic.full2$Survived<-as.factor(titanic.full2$Survived)


#8 Spliting dataset back out into train and test

titanic.train2<-titanic.full2[titanic.full2$IsTrainSet==TRUE,]
titanic.test2<-titanic.full2[titanic.full2$IsTrainSet==FALSE,]

(titanic.train2)
(titanic.test2)


#9 Modeling with Random Forest

Survived2.equation<-"Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived2.formula<-as.formula(Survived2.equation)
install.packages("randomForest")
library(randomForest)


randomForest(Survived2~.)
titanic.model2<-randomForest(formula = Survived2.formula,data=titanic.train2, ntree=500,mtry=3,nodesize = 0.01*nrow(titanic.test2))

features.equation<-"Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived2<-predict(titanic.model2, newdata = titanic.test2)


#10 Setting up the final Output Table

PassengerId<-titanic.test2$PassengerId
output2.df<-as.data.frame(PassengerId)
output2.df$Survived<-Survived2


#11 Save Final Output .cvs file
write.csv(output2.df,file="Kaggle_submission2.csv", row.names = FALSE)

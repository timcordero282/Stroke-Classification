################################################################################
# Course: IST 687
# Project: Stroke Classification
# Name: Tim Cordero
# Date: 09/23/2021
################################################################################
#Libraries

#install.packages("ggplot2")
#library(ggplot2)
#install.packages("kernlab")
#library(kernlab)
#install.packages("e1071")
#library(e1071)
################################################################################
#read in data set
strokeData <- read.csv('C:/Users/timco/OneDrive - Syracuse University/Semester1/ist_687/Project/healthcare-dataset-stroke-data.csv')

View(strokeData)

# replace N/A's in BMI field with mean
strokeData$bmi <- as.numeric(strokeData$bmi)
length(strokeData$bmi[is.na(strokeData$bmi)])
strokeData$bmi[is.na(strokeData$bmi)]<-mean(strokeData$bmi,na.rm=TRUE)
length(strokeData$bmi[is.na(strokeData$bmi)])

strokeData <- strokeData[, -1]

################################################################################
#high level outline of dataset
#structure
str(strokeData)
#summary
summary(strokeData)
# first 6
head(strokeData)
# last 6
tail(strokeData)
# tabular view
View(strokeData)

################################################################################
#some descriptive statistics on numeric attributes

desc_stats <- function(vec){
  cat("Mean: ", mean(vec), "\n")
  cat("Min: ", min(vec), "\n")
  cat("Max: ", max(vec), "\n")
  cat("Standard Dev: ",sd(strokeData$avg_glucose_level), "\n")
  cat("Median: ", median(strokeData$avg_glucose_level), "\n")
}

# age attribute
strokeData$age <- as.numeric(strokeData$age)
desc_stats(strokeData$age)

# gulcose attribute
strokeData$avg_glucose_level <- as.numeric(strokeData$avg_glucose_level)
desc_stats(strokeData$avg_glucose_level)


# bmi attribute
strokeData$bmi <- as.numeric(strokeData$bmi)
desc_stats(strokeData$bmi)

################################################################################
#exploring non numeric attributes
# how many of each for attributes
#gender
length(strokeData$gender[strokeData$gender == 'Male'])
length(strokeData$gender[strokeData$gender == 'Female'])
# fairly even amount of each

#hypertension
length(strokeData$hypertension[strokeData$hypertension == 0])
length(strokeData$hypertension[strokeData$hypertension == 1])
# many more people without hypertensions comapred to positive hypertension indicator

#married
length(strokeData$ever_married[strokeData$ever_married == 'Yes'])
length(strokeData$ever_married[strokeData$ever_married == "No"])
# many more people have been married comapred to those who have not

#work type
unique(strokeData$work_type)
length(strokeData$work_type[strokeData$work_type == 'Private'])
length(strokeData$work_type[strokeData$work_type == "Self-employed"])
length(strokeData$work_type[strokeData$work_type == 'Govt_job'])
length(strokeData$work_type[strokeData$work_type == "children"])
length(strokeData$work_type[strokeData$work_type == "Never_worked"])
# majority of people work in the private sector

#smoker
unique(strokeData$smoking_status)
length(strokeData$smoking_status[strokeData$smoking_status == 'formerly smoked'])
length(strokeData$smoking_status[strokeData$smoking_status == "never smoked"])
length(strokeData$smoking_status[strokeData$smoking_status == 'smokes'])
length(strokeData$smoking_status[strokeData$smoking_status == "Unknown"])
# results are mixed

#Residence
unique(strokeData$Residence_type)
length(strokeData$Residence_type[strokeData$Residence_type == 'Urban'])
length(strokeData$Residence_type[strokeData$Residence_type == "Rural"])
# results are mixed

#prediction attribute: stroke indicator
length(strokeData$stroke[strokeData$stroke == 0])
length(strokeData$stroke[strokeData$stroke == 1])
# majority of people in dataset did NOT have a stroke


################################################################################
#sampling some data from Stoke dataset for numeric attributes
# creating function for code reuse
sampling <- function(vec){
  sampleMeans <- replicate(1000,mean(sample(vec, 50, replace=TRUE)), simplify = TRUE)
  cat("Mean: ", mean(sampleMeans))  
  hist(sampleMeans)
}
#age
sampling(strokeData$age)
# has a fairly normal disribution with 50 samples taken 1000 times

#bmi
sampling(strokeData$bmi)
# has a fairly normal disribution with 50 samples taken 1000 times

#glucose
sampling(strokeData$avg_glucose_level)
# has a fairly normal disribution with 50 samples taken 1000 times

################################################################################
#visualizations

# box plot to compare ages across genders
ggplot(strokeData, aes(x=gender, y=age)) + geom_boxplot(aes(fill=factor(gender)))
# majority of ages between genders is between 25 and 60

# scatter plot to compare glucose level to age, as well as considering hypertension
ggplot(strokeData, aes(x=avg_glucose_level, y=age)) +geom_point(aes(color=hypertension))
# the older someone is the more likely they are to have high glucose
#   also the older someone is the more likely they can have hypertension

# bar chart to see smoking status' along with stroke indicator
ggplot(strokeData, aes(x=smoking_status)) + geom_bar(aes(fill=factor(stroke)))
# majority of dataset did not have stroke, however distributions of strokes across smoking
#   status is fairly even

# bar chart to see marital status along with stroke indicator
ggplot(strokeData, aes(x=ever_married)) + geom_bar(aes(fill=factor(stroke)))
# even though much of data set was married before, there is still strong indication that 
#   those who have been married are more likely to have a stroke


################################################################################
# Modeling

# MODEL 1: KSVM
# set non-numerics to factors so KSVM will work
strokeData$gender <- as.factor(strokeData$gender)
strokeData$ever_married <- as.factor(strokeData$ever_married)
strokeData$work_type <- as.factor(strokeData$work_type)
strokeData$Residence_type <- as.factor(strokeData$Residence_type)
strokeData$smoking_status <- as.factor(strokeData$smoking_status)
strokeData$stroke <- as.factor(strokeData$stroke)
str(strokeData)

# 1st Model: using Support Vecotor Machines (ksvm)
nrows <- nrow(strokeData)

#Q: What should our cutpoint be for 5110 observations (2/3)?
#A : Estimaed: Between ~ 3,372-3,423 should be cutpoint
cutpoint <- floor(nrows/3*2)
cutpoint
rand <- sample(1:nrows)
head(rand)

# creating training and test data
stroke.train <- strokeData[rand[1:cutpoint],]
stroke.test <- strokeData[rand[(cutpoint+1):nrows],]
str(stroke.train)
str(stroke.test)

# create model and train with training data
model <- ksvm(stroke~.,data=stroke.train)
model

#use model to predict and analyze accuracy
pred <- predict(model, stroke.test)
results <- table(pred,stroke.test$stroke)

#finding percentage classified correctly from test data
totalCorrect <- results[1,1] + results[2,2]
totalInTest <- nrow(stroke.test)
totalCorrect/totalInTest
# ~95%
# Not applicable, always predicting No stroke

# Using same model structure with single column as predictor
model <- ksvm(stroke~bmi,data=stroke.train)
model

#use model to predict and analyze accuracy
pred <- predict(model, stroke.test)
results <- table(pred,stroke.test$stroke)

#finding percentage classified correctly from test data
totalCorrect <- results[1,1] + results[2,2]
totalInTest <- nrow(stroke.test)
totalCorrect/totalInTest
#~95%
# Not applicable, always predicting No stroke


#KSVM insights
# - Whether I use all columns to predict or a single column, ksvm will always predict no stroke.
#   this is because a majority of the dataset has not had a stroke before so the alogirthm can
#   achieve a high accuracy rate with always predicting no stroke. However, there is a key 
#   difference in the number of support vector machines as the model with a single predictor
#   only uses ~334 SVM's, whereas the model with all predictors uses ~602 SVM's. To get more
#   accurate results, I am going to use a dataset of 500 records, 249 who have had a stoke, 
#   and 251 who have not. This will give us a more accurate model for predicting those who
#   have had a stroke.

limited_strokeData <- strokeData[0:500, ]
nrows <- nrow(limited_strokeData)


cutpoint <- floor(nrows/3*2)
cutpoint
rand <- sample(1:nrows)
head(rand)

# creating training and test data
limited_stroke.train <- limited_strokeData[rand[1:cutpoint],]
limited_stroke.test <- limited_strokeData[rand[(cutpoint+1):nrows],]
str(limited_stroke.train)
str(limited_stroke.test)

# create model and train with training data
model <- ksvm(stroke~.,data=limited_stroke.train)
model

#use model to predict and analyze accuracy
pred <- predict(model, limited_stroke.test)
results <- table(pred,limited_stroke.test$stroke)

#finding percentage classified correctly from test data
totalCorrect <- results[1,1] + results[2,2]
totalInTest <- nrow(limited_stroke.test)
L_KSVM <- totalCorrect/totalInTest
L_KSVM
# ~75%
# predictions are now mixed and we have a more accurate model because KSVM is not always predicting
#   no stroke due to majority of original dataset not having a stroke.


#try model with single predictor: AGE
model <- ksvm(stroke~age,data=limited_stroke.train)
model

#use model to predict and analyze accuracy
pred <- predict(model, limited_stroke.test)
results <- table(pred,limited_stroke.test$stroke)

#finding percentage classified correctly from test data
totalCorrect <- results[1,1] + results[2,2]
totalInTest <- nrow(limited_stroke.test)
L_age_KSVM <- totalCorrect/totalInTest
L_age_KSVM
# ~73%
# We can get about the same accuracy with only using age as a predictor compared to using
#   all columns as a predictor. The SVM's are slightly lower with only age as a predictor as
#   well. 



#MODEL 2: NAIVE BAYES
#compute model in Naive Bayes using e1071 package
mod_NB <- naiveBayes(stroke ~ ., data = stroke.train)
mod_NB_pred <- predict(mod_NB, stroke.test)
mod_NB_results <- table(mod_NB_pred, stroke.test$stroke)

# calculate correct and plot for Naive Bayes.
totalCorrectNB <- mod_NB_results[1,1] + mod_NB_results[2,2]
totalInTestNB <- nrow(stroke.test)
NB <- totalCorrectNB/totalInTestNB
NB
# ~85%

#compute model in Naive Bayes using single predictor
mod_NB <- naiveBayes(stroke ~ avg_glucose_level, data = stroke.train)
mod_NB_pred <- predict(mod_NB, stroke.test)
mod_NB_results <- table(mod_NB_pred, stroke.test$stroke)

# calculate correct and plot for Naive Bayes.
totalCorrectNB <- mod_NB_results[1,1] + mod_NB_results[2,2]
totalInTestNB <- nrow(stroke.test)
gluc_NB <- totalCorrectNB/totalInTestNB
gluc_NB 
# ~93%

# Naive Bayes insights
# - Naive Bayes is not always predicting no stroke with the full dataset. 
#   However, for the model with a single predictor, the majority columns resulted in a model that
#   always predicts no stroke. The only columns that did not generate model which always predict
#   no were heart_disease and avg_glucose_level, and avg_glucose_level was 3% more accurate.
vec <- c(L_KSVM, L_age_KSVM, NB, gluc_NB)
name <- c("Limited KSVML", "Limited age KSVM", "Naive Bayes", "AVG Glucose Naive Bayes")
accuracies <- data.frame(name, vec)
g <- ggplot(accuracies) + geom_col(aes(x=name, y=vec*100, fill=factor(name)))
g + theme(axis.text.x = element_text(angle = 45, hjust=1))

# Conclusion
# The most thorough, parsimonious, and most accurate model is the Naive Bayes model with only 
#   the average glucose column as the predictor. With this model we can limit the amount of
#   predictors as well as achieve a high accuracy rate in predicting. Also, we can use the 
#   entire dataset because Naive Bayes did not limit the predictions to no stroke only even 
#   though the majority of the dataset did not a have stroke. KSVM did do this with the full
#   dataset which is why I had to create a limited dataset to even the distribution of stroke
#   to non-stroke. Not only were the KSVM accuracies lower, but the models were less accurate
#   and less trustworthy. 

# AVG age for those who have had stroke
mean(strokeData$age[strokeData$stroke==1])

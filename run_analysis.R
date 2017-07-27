## Open required libraries
library(dplyr)

## Clean up workspace 
rm(list=ls()) 

## Set working directory to the location where the dataset was unzipped 
setwd("C:/Users/amurr002/Desktop/R_dir/Getting&CleaningData/UCI HAR Dataset")


# (1) Merge datasets ------------------------------------------------------


## Read in the train data from files 
features     = read.table('./features.txt',header=FALSE)
activityType = read.table('./activity_labels.txt',header=FALSE)
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain       = read.table('./train/x_train.txt',header=FALSE)
yTrain       = read.table('./train/y_train.txt',header=FALSE) 

## Assign column names to the data imported above 
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId" 

## Create the train set by merging yTrain, subjectTrain, and xTrain 
trainData = cbind(yTrain,subjectTrain,xTrain)

## Read in the test data from files 
subjectTest = read.table('./test/subject_test.txt',header=FALSE) 
xTest       = read.table('./test/x_test.txt',header=FALSE)
yTest       = read.table('./test/y_test.txt',header=FALSE)

## Assign column names to the data imported above 
colnames(subjectTest)  = "subjectId"
colnames(xTest)        = features[,2]
colnames(yTest)        = "activityId" 

## Create the test set by merging yTest, subjectTest, and xTest 
testData = cbind(yTest,subjectTest,xTest)

## Create the full data set by merging trainData and testData
Data_Full = rbind(trainData,testData)


# (2) Retrieve means and standard deviations ------------------------------

## Create a vector for the column names of the full dataset
colNames  = colnames(Data_Full)  

## Extract only the measurements on the mean and standard deviation for each measurement.  
isNeeded = (grepl("activity",colNames) | grepl("subject",colNames) | grepl("mean",colNames) & !grepl("meanFreq",colNames) | grepl("std",colNames)) 
Data_Full_Needed = Data_Full[,which(isNeeded)]

# (3) Use descriptive activity names --------------------------------------

## Merge the full dataset set with the acitivityType table to include descriptive activity names 
Data_Full_Needed = merge(Data_Full_Needed,activityType,by="activityId",all.x=TRUE)
setcolorder(Data_Full_Needed,c(1,69,2:68)) #sets column order


# (4) Label the data set with descriptive variables names -----------------
colNames = colnames(Data_Full_Needed)
colNames = gsub("\\()","",colNames)
colNames = gsub("std","StdDev",colNames)
colNames = gsub("mean","Mean",colNames)
colNames = gsub("^(t)","Time-",colNames)
colNames = gsub("^(f)","Freq-",colNames)
colNames = gsub("Mag","Magnitude",colNames)
colNames = gsub("BodyBody","Body",colNames); colNames

colnames(Data_Full_Needed) = colNames


# (5) Create a tidy data set with the average of each variable per activity & subject--------
Avg_Data = aggregate(.~activityType+subjectId, Data_Full_Needed, mean)
write.csv(Avg_Data, "Avg_Data.csv", row.names = F)


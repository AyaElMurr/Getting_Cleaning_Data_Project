# Getting and Cleaning Data Project
Aya El Murr

## Description 
This code book provides additional information about the variables, data and transformations used in the course project for the Johns Hopkins Getting and Cleaning Data course. The data used in this project can be found at The UCI Machine Learning Repository. 

## Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz were captured. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Attribute Information
For each record in the dataset are provided
1. Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
2. Triaxial Angular velocity from the gyroscope. 
3. A 561-feature vector with time and frequency domain variables.
4. Its activity label.
5. An identifier of the subject who carried out the experiment.

## Tasks performed
1. Merge the training and the test sets to create one data set: Set the working directory, read the datasets from the tables provided and merge these datasets. 
2. Extract only the measurements on the mean and standard deviation for each measurement: Create a logcal vector that contains TRUE values for the ID, mean and stdev columns and FALSE values for the others. Subset this data to keep only the necessary columns.
3. Use descriptive activity names to name the activities in the data set: Merge data subset with the activityType table to cinlude the descriptive activity namesSection 
4. Appropriately label the data set with descriptive activity names: Use gsub function for pattern replacement to clean up the data labels.
5. Create a second, independent tidy data set with the average of each variable for each activity and each subject: as per the project instructions, this is the dataset included in this repo. 


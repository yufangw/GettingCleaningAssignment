---
title: "ReadMe"
author: "Yufang"
date: "Tuesday, September 16, 2014"
output: html_document
---

## How the script work

### Extracts only the measurements on the mean and standard deviation for each measurement. 

```{r}
# read feature names as strings
featureNames = read.table('./features.txt', sep = ' ', stringsAsFactors = FALSE)

# find features that contain mean() or std()
findMeanStd = function(s){
    length(grep('mean()',s))>0 | length(grep('std()',s))>0
}
isMeanStd = sapply(featureNames$V2, findMeanStd)
colNames = c('subject','activity',featureNames$V2[isMeanStd])
```
There are 561 feature vaiables, 79 of them contain mean() or std()

### Appropriately labels the data set with descriptive variable names. 
```{r}
# read training data and extract mean()'s and std()'s
s = read.table('./train/subject_train.txt')
y = read.table('./train/Y_train.txt')
x = read.table('./train/X_train.txt', 
               colClass = 'numeric', 
               nrow = length(s$V1), 
               comment.char = "")
x = x[,isMeanStd]
Dtr = cbind(s$V1, as.factor(y$V1),x)
colnames(Dtr) = colNames
```
There are 7352 observations in the training data set, coming from subjects
1  3  5  6  7  8 11 14 15 16 17 19 21 22 23 25 26 27 28 29 30

```{r}
# read test data and extract mean()'s and std()'s
s = read.table('./test/subject_test.txt')
y = read.table('./test/Y_test.txt')
x = read.table('./test/X_test.txt', 
               colClass = 'numeric', 
               nrow = length(s$V1), 
               comment.char = "")
x = x[,isMeanStd]
Dts = cbind(s$V1, as.factor(y$V1),x)
colnames(Dts) = colNames
```
There are 2947 observations in the test data set, coming from subjects
 2  4  9 10 12 13 18 20 24


### Merges the training and the test sets to create one data set.
```{r}
# combine training and test data
Dt = rbind(Dtr, Dts)
```

* Uses descriptive activity names to name the activities in the data set
```{r}
# read activity names as strings
aNames = read.table('./activity_labels.txt', as.is = TRUE)
aLabel = aNames$V2
names(aLabel) = as.character(aNames$V1)

# use descriptive activity names to name the activities
library(plyr)
Dt$activity = revalue(Dt$activity, aLabel)
```
Replacing activity id 1,2,3,4,5,6 with names WALKING, WALKING_UPSTAIRS, 
WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING

### creates a second, independent tidy data set with the average of each variable for each activity and each subject.

```{r}
# creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject
Dt = arrange(Dt, activity, subject)
a1 = unique(Dt$activity)
s1 = unique(Dt$subject)
a1 = rep(a1, each = length(s1))
s1 = rep(s1, times = length(aLabel))
cfac = interaction(Dt$activity, Dt$subject, drop = TRUE, lex.order = TRUE)
colidx = 3:ncol(Dt)
newD = sapply(colidx, function(ci) tapply(Dt[,ci], cfac, mean))
newD = as.data.frame(newD)
newD2 = cbind(s1, a1, newD)
colnames(newD2) = colNames
newD2[sample.int(nrow(newD2), 6),1:6]
```
New tidy dataset has 180 observations (30 subjects x 6 activities), and 79 averaged feature vaiables

### write results
```{r}
# write results
write.table(newD2, 'UCIHARnew.txt', row.name = F, quote = F)
```

## code book
### subjects: 

volunteer id, from 1 to 30

### activities: 6 types:

* WALKING, 
* WALKING_UPSTAIRS, 
* WALKING_DOWNSTAIRS, 
* SITTING, 
* STANDING, 
* LAYING

### features and variables

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

* mean(): Mean value
* std(): Standard deviation

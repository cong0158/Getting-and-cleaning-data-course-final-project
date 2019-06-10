library(tidyr)
library(dplyr)
library(readr)

## read test data
x_test = read.table("UCI Har Dataset/test/X_test.txt", colClasses = "numeric")
y_test = read.table("UCI Har Dataset/test/y_test.txt", colClasses = "factor")
subject_test = read.table("UCI Har Dataset/test/subject_test.txt", colClasses = "numeric")

## read train data
x_train = read.table("UCI Har Dataset/train/X_train.txt", colClasses = "numeric")
y_train = read.table("UCI Har Dataset/train/y_train.txt", colClasses = "factor")
subject_train = read.table("UCI Har Dataset/train/subject_train.txt", colClasses = "numeric")

##read other data
feature = read.table("UCI Har Dataset/features.txt", colClasses = "character")
activity = read.table("UCI HAR Dataset/activity_labels.txt", colClasses = "character")

## step 1: merge them into one table
xdat = rbind(x_test, x_train)
ydat = rbind(y_test, y_train)
subjectdat = rbind(subject_test, subject_train)


## step 2: extract the colunms with mean() and std()
mean_std = grep("(mean[(][)]|std)", feature[,2])
xdat = xdat[, mean_std]

## step 3: Use descriptive activity names to name the activities in the data set 
ydat[,1] = mapvalues(ydat[,1], from = levels(ydat[,1]), to = activity[,2])

## step 4: Appropriately labels the data set with descriptive variable names.
names(xdat) = feature[mean_std, 2]
names(ydat) = "activity"
names(subjectdat) = "subject"

## step 5: From the data set in step 4, creates a second, 
##         independent tidy data set with the average of 
##         each variable for each activity and each subject.
dat = cbind(xdat, ydat, subjectdat)
cm  = function(x){colMeans(x[,1:66])}
average_dat = ddply(dat, .(subject, activity), cm)
write.table(average_dat, "averags_data.txt", row.name=FALSE)
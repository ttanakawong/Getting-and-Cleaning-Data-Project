## 0)  Read files
train.data<-read.table("./train/X_train.txt")
test.data<-read.table("./test/X_test.txt")

train.label<-read.table("./train/y_train.txt")
test.label<-read.table("./test/y_test.txt")

train.subject<-read.table("./train/subject_train.txt")
test.subject<-read.table("./test/subject_test.txt")

## 1)  Merge the training and the test sets to create one data set
data <- rbind(train.data, test.data)
label <- rbind(train.label, test.label)
subject <- rbind(train.subject, test.subject)

## 2) Extract only the measurements on the mean and standard deviation for each measurement. 

features <- read.table('./features.txt')
measurement<-grep("mean\\(\\)|std\\(\\)", features[, 2])
data<-data[,measurement]
names(data)<-gsub("\\(\\)", "", features[measurement,2])

## 3) Use descriptive activity names to name the activities in the data set.


activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
activity[, 2] <- gsub("_", "", activity[, 2])
activitylabel <- activity[label[, 1], 2]
label[,1]<-activitylabel 
names(label)<-"activity"


## 4) Appropriately label the data set with descriptive variable names. 

names(subject)<-"subject"
tidydata1<-cbind(subject,label,data)
write.table(tidydata1,"tidydata1.txt")

## 5) From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

NActivities = length(activity[,1])
NSubjects = length(unique(subject)[,1])
USubjects = unique(subject)[,1]
NCols = dim(tidydata1)[2]
FinalData = tidydata1[1:(NSubjects*NActivities), ]
row = 1
for (subject in 1:NSubjects) {
  for (a in 1:NActivities) {
    FinalData[row, 1] = USubjects[subject]
    FinalData[row, 2] = activity[a, 2]
    dta <- tidydata1[tidydata1$subject==subject & tidydata1$activity==activity[a, 2], ]
    FinalData[row, 3:NCols] <- colMeans(dta[, 3:NCols])
    row = row+1
  }
}
write.table(FinalData, "tidydata2.txt")





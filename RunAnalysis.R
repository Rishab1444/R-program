library(dplyr)
getwd()
#assigning all the data frame
feature <- read.table("F:/rishab program/data cleaning/week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt",col.names = c("n","Function"))
activities <- read.table("F:/rishab program/data cleaning/week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt",col.names = c("code","Activity"))
##TEST SET
subject_test <- read.table("F:/rishab program/data cleaning/week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt",col.names = "Subject")
X_test <- read.table("F:/rishab program/data cleaning/week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt",col.names = feature$Function)
y_test <- read.table("F:/rishab program/data cleaning/week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt",col.names = "code")
##TRAINING SET
subject_train <- read.table("F:/rishab program/data cleaning/week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt",col.names = "Subject")
X_train <- read.table("F:/rishab program/data cleaning/week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt",col.names = feature$Function)
y_train <- read.table("F:/rishab program/data cleaning/week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt",col.names = "code")

#STEP 1 making all into one set
X <- rbind(X_train,X_test)
y <- rbind(y_train,y_test)
subject<-rbind(subject_test,subject_train)
Merged <- cbind(merged1,X)

#step 2 taking out the mean
columnname <-colnames(Merged)
mean_sd <-(grepl("code" ,columnname ) | 
             grepl("Subject" ,columnname ) | 
             grepl("mean.." ,columnname ) | 
             grepl("std.." ,columnname )) 
tidy_data <- Merged[,mean_sd ==TRUE]
#STEP 3 to name the descriptive data set
tidy_data1 <- merge(tidy_data,activities,by = "code",all.x = TRUE)
#step 4 naming all the coloumn
names(tidy_data1)[1] = "Subject"
names(tidy_data1)[2] = "activity"
names(tidy_data1)<-gsub("Acc", "Accelerometer", names(tidy_data1))
names(tidy_data1)<-gsub("Gyro", "Gyroscope", names(tidy_data1))
names(tidy_data1)<-gsub("BodyBody", "Body", names(tidy_data1))
names(tidy_data1)<-gsub("Mag", "Magnitude", names(tidy_data1))
names(tidy_data1)<-gsub("^t", "Time", names(tidy_data1))
names(tidy_data1)<-gsub("^f", "Frequency", names(tidy_data1))
names(tidy_data1)<-gsub("tBody", "TimeBody", names(tidy_data1))
names(tidy_data1)<-gsub("-mean()", "Mean", names(tidy_data1), ignore.case = TRUE)
names(tidy_data1)<-gsub("-std()", "STD", names(tidy_data1), ignore.case = TRUE)
names(tidy_data1)<-gsub("-freq()", "Frequency", names(tidy_data1), ignore.case = TRUE)
names(tidy_data1)<-gsub("angle", "Angle", names(tidy_data1))
names(tidy_data1)<-gsub("gravity", "Gravity", names(tidy_data1))
#step 5 creating into a independent file

secTidySet <- aggregate(. ~activity + Subject, tidy_data1, mean)
secTidySet <- secTidySet[order(secTidySet$Subject, secTidySet$activity),]

#5.2 Writing second tidy data set in txt file
write.table(secTidySet, "FinalData.txt", row.name=FALSE)

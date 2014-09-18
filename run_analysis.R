# The purpose of this project is to demonstrate your ability to collect, work with, 
# and clean a data set. The goal is to prepare tidy data that can be used for later
# analysis. You will be graded by your peers on a series of yes/no questions related
# to the project. You will be required to submit: 
#         1) a tidy data set as described below 
#         2) a link to a Github repository with your script for performing the analysis
#         3) a code book that describes the variables, the data, and any transformations 
#         or work that you performed to clean up the data called CodeBook.md. 
#         4)You should also include a README.md in the repo with your scripts. 
#         This repo explains how all of the scripts work and how they are connected.  
# 
# One of the most exciting areas in all of data science right now is wearable computing
# - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing
# to develop the most advanced algorithms to attract new users. The data linked to from the
# course website represent data collected from the accelerometers from the Samsung Galaxy S
# smartphone. A full description is available at the site where the data was obtained: 
#         
#         http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#         
#         https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.





        #0. READ THE DATA
#Please set your working directory with data in the next line. It should contain a test and a train folder.
setwd("/Users/pelayogonzalez/Desktop/Coursera/Getting_Cleaning_Data/data/run_data") #Replace this working directory with yours
datadir  <- getwd()  # Path to Data directory
testdir  <- paste(datadir, "test", sep="/") # Complete the path to test folder
traindir <- paste(datadir, "train", sep="/") # Complete the path to train folder

subject_test  <- read.table(paste(testdir, "subject_test.txt", sep="/"), quote="\"", col.names = "Subject.id")
subject_train <- read.table(paste(traindir, "subject_train.txt", sep="/"), quote="\"",col.names = "Subject.id")
train_y <- read.table(paste(traindir, "y_train.txt", sep="/"), quote="\"",col.names = "Label")
test_y  <- read.table(paste(testdir, "y_test.txt", sep="/"), quote="\"",col.names = "Label")
train_x <- read.table(paste(traindir, "X_train.txt", sep="/"), quote="\"")
test_x  <- read.table(paste(testdir, "X_test.txt", sep="/"), quote="\"")

features <- read.table(paste(datadir, "features.txt", sep="/"),quote="\"")
activity_labels <- read.table(paste(datadir,"/activity_labels.txt",sep=""), quote="\"")
        


        # Merge auxiliary files containing labels and subjects ids
Label <- rbind(test_y, train_y)
Subject <- rbind(subject_test, subject_train)

        #Use descriptive names to name activities
Label.act <- merge(Label, activity_labels, by=1) 
Label.act<- Label.act[,2] #(ASSIGNMENT POINT 3.)

        #Merge datasets containing quantitative data 
Data <- rbind(test_x, train_x) #(ASSIGNMENT POINT 1.)
colnames(Data) <- c(as.character(features[,2])) #(ASSIGNMENT POINT 4.)

###### Merge info on Subjects, Labels and actual Data to create a big Data set
Dataset <- cbind(Subject, Label.act, Data)

#Pick just variables containing mean and std.dev. for each measurement
mean.var<-grep("mean()",colnames(Dataset),fixed=TRUE) 
std.var<-grep("std()",colnames(Dataset),fixed=TRUE)
DataMeanStd <- Dataset[,c(mean.var,std.var)] #(ASSIGNMENT POINT 2.)

# Prints the data table to a file
write.table(DataMeanStd, paste(datadir,"/TidyData.txt",sep=""), sep = ";")

# install reshape2 package if dosn't exist
if (!require("reshape2")) {
        install.packages("reshape2")
        require("reshape2")
}

#Creates a second, independent tidy data set with the average of each variable 
#for each activity and each subject. #(ASSIGNMENT POINT 5.)
Melt.Data = melt(Dataset, id.vars = c("Subject.id", "Label.act"))
Tidy.Avg.Data = dcast(Melt.Data, formula = Subject.id + Label.act ~ variable, mean)

write.table(Tidy.Avg.Data, paste(datadir,"/tidy_avg_data.txt",sep=""), sep = ";", row.name=FALSE )

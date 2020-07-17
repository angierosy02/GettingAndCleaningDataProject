library(dplyr)
library(gsubfn)
library(tidyr)

setwd("C:/Users/angie/Desktop/GettingAndCleaningDataProject")

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")
unzip(zipfile="./data/Dataset.zip",exdir="./data")
path_rf <- file.path("./data" , "UCI HAR Dataset")
list_files<- list.files(path_rf, full.names = T, recursive = TRUE)

activity_labels <-read.table(list_files[[1]], header = F, col.names = c("id","Activity"),stringsAsFactors = TRUE) #rows
col_names <- read.table(list_files[[2]], header = F) #features list


test_data <- cbind(read.table(list_files[[16]], header = F, col.names = "Activity"),
                   read.table(list_files[[14]], header = F, col.names = "Subject"),
                   read.table(list_files[[15]], header = F, col.names = col_names[,2]))

train_data <- cbind(read.table(list_files[[28]], header = F,col.names = "Activity"),
                    read.table(list_files[[26]], header = F, col.names = "Subject"),
                    read.table(list_files[[27]], header = F, col.names = col_names[,2]))

# Merges the training and the test sets to create one data set.
allData <- rbind(test_data,train_data)

# Uses descriptive activity names to name the activities in the data set.
activity_labels <- gsub("_"," ",activity_labels$Activity) #Make the labels descriptive
allData$Activity<-factor(allData$Activity, labels = activity_labels)


# Extracts only the measurements on the mean and standard deviation for each measurement (step 4).
subData <- allData %>% select(Activity, Subject, contains(".mean."),contains(".std."))

# From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.
tidyData <- subData %>% group_by(Subject, Activity) %>% 
        summarize(across(.cols = everything(),.fns = mean)) %>%
        gather(key = "Feature", value = "Average", - Activity,- Subject)

# Labels the data set with descriptive variable names by separating the parts of each feature into a unique column.
patterns <- c("^t","^f","(Body){1}|(Body){2}","Acc","Gyro","Jerk","Gravity","Mag","\\.mean|\\.mean\\.\\.",
              "\\.std|\\.std\\.\\.","\\.X|\\.\\.\\.X","\\.Y|\\.\\.\\.Y","\\.Z|\\.\\.\\.Z")
replacements <-c("Time","Frequency","_Body","_Accelerometer","_Gyroscope",
                 " Jerk","_Gravity"," Magnitude","_Mean","_std"," in X direction"," in Y direction"," in Z direction")
for(i in 1:length(patterns)){
        tidyData$Feature <- gsub(patterns[i],replacements[i],tidyData$Feature)}

tidyData <- tidyData %>%  separate(Feature, c("Domain","Motion","Sensor","Values"),sep = "_")

# To print tidy data into a text file use:
# write.table(tidyData,file = "tidyData.txt", sep = "\t", row.names = FALSE)

#To create the Codebook: library(dataMaid), makeCodebook(tidyData) and add variable labels and descriptions

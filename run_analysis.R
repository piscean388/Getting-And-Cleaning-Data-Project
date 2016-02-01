
## training set
x_train <- read.table("./Samsung Dataset/train/X_train.txt")
y_train <- read.table("./Samsung Dataset/train/y_train.txt")
subject_train <- read.table("./Samsung Dataset/train/subject_train.txt")

training_set <- cbind(subject_train, y_train, x_train)

## test set
x_test <- read.table("./Samsung Dataset/test/X_test.txt")
y_test <- read.table("./Samsung Dataset/test/y_test.txt")
subject_test <- read.table("./Samsung Dataset/test/subject_test.txt")

test_set <- cbind(subject_test, y_test, x_test)

## merge training and test set
merged_set <- rbind(test_set, training_set)

## extracting only variables representing mean or standard deviation
feature_names <- read.table("./Samsung Dataset/features.txt", stringsAsFactors = FALSE)
names(merged_set) <- c("subject", "activity", feature_names[,2])
col_indices <- grep("mean()|std()|subject|activity", names(merged_set))
merged_set <- merged_set[, col_indices]

## replacing activites with their descriptive names
activity_labels <- read.table("./Samsung Dataset/activity_labels.txt")
merged_set$activity <- sapply(merged_set$activity, function(x){activity_labels[x, 2]})

## updating variable names
names(merged_set) <- gsub("-","", names(merged_set))
names(merged_set) <- gsub("\\(","", names(merged_set))
names(merged_set) <- gsub(")","", names(merged_set))
names(merged_set) <- gsub("^t","time", names(merged_set))
names(merged_set) <- gsub("^f","freq", names(merged_set))

## creating tidy data set with average value of each variable for
## each subject and each activity
library(dplyr)
tidy_data <- merged_set %>% 
                        group_by(subject, activity) %>% 
                                                summarize_each(funs(mean))

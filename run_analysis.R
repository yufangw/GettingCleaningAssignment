# read feature names as strings
featureNames = read.table('./features.txt', sep = ' ', stringsAsFactors = FALSE)

# find features that contain mean() or std()
findMeanStd = function(s){
    length(grep('mean()',s))>0 | length(grep('std()',s))>0
}
isMeanStd = sapply(featureNames$V2, findMeanStd)
colNames = c('subject','activity',featureNames$V2[isMeanStd])

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

# combine training and test data
Dt = rbind(Dtr, Dts)

# read activity names as strings
aNames = read.table('./activity_labels.txt', as.is = TRUE)
aLabel = aNames$V2
names(aLabel) = as.character(aNames$V1)

# use descriptive activity names to name the activities
library(plyr)
Dt$activity = revalue(Dt$activity, aLabel)

# generate factors to label data belonging to the same subject and activity
Dt = arrange(Dt, activity, subject)
a1 = unique(Dt$activity)
s1 = unique(Dt$subject)
a1 = rep(a1, each = length(s1))
s1 = rep(s1, times = length(aLabel))
cfac = interaction(Dt$activity, Dt$subject, drop = TRUE, lex.order = TRUE)

# generate new tidy data with the average of each variable 
# for each activity and each subject
colidx = 3:ncol(Dt)
newD = sapply(colidx, function(ci) tapply(Dt[,ci], cfac, mean))
newD = as.data.frame(newD)
newD2 = cbind(s1, a1, newD)
colnames(newD2) = colNames

# write results
write.table(newD2, 'UCIHARnew.txt', row.name = F, quote = F)
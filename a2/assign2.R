setwd("/GitHub/dataanalysis_001/a2")
setInternet2(T)
url <- "https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda"
dest <- "raw/samsungData.rda"
download.file(url, dest)
dateDownloaded <- date()
load("raw/samsungData.rda")
sam <- samsungData

##  Data exploration
table(is.na(sam))
colnames(sam)
lapply(sam,class)

##  Data munging
sam$activity <- as.factor(sam$activity)
names(sam)[1:561] <- c(1:561)
colnames(sam) = paste("x", colnames(sam), sep="")
names(sam)[562:563] <- c("subject","activity")

##   Load Libraries
library(tree)
library(randomForest)
library(ipred)
library(boot)

##  Split data (train, test, validate)
t <- sam[sam$subject == c(27,28,29,30),]
nont <- sam[!sam$subject %in% t$subject,] 
test <- sam[!sam$subject %in% nont$subject,] 
t <- nont[sam$subject == c(22,23,25,26),]
train <- nont[!nont$subject %in% t$subject,] 
validate <- nont[!nont$subject %in% train$subject,]

trainless <- train[,c(1:561,563)]

##  Define Error Rate
##  True Positive?

##  Predictive features

##  Predictive function
forest <- randomForest(activity ~ .,data=train,prox=T)
forest2 <- randomForest(activity ~ .,data=trainless,prox=T)

##  Validate
p1 <- predict(forest, validate)
p2 <- predict(forest2, validate)
sum(p1 != validate$activity)/length(validate$activity)
sum(p2 != validate$activity)/length(validate$activity)

##  Graphs
par(mfrow=c(1,2))

act = as.numeric(as.factor(train$activity))
svd1 = svd(scale(train[,-c(562,563)]))
# max.1 is 296
# max.2 is 249
plot(svd1$v[,2],col=act,pch=19, cex=1)
hist(act)

##
names(sam) = names(samsungData)
names(train) = names(sam)
# heatmap(as.matrix(train[,-c(562,563)]))


heat = table(c("subject",train[train$subject == 1,]$activity))
for(i in c(3,5,6,7,8,11,14,15,16,17,19,21)) {
  heat <- rbind(heat,c(tabulate(train[train$subject == i,]$activity),i))
}
heat <- as.data.frame(heat)
row.names(heat) <- heat$subject
heat <- heat[,1:6]
colnames(heat) = names(table(train$activity))
heatmap(as.matrix(heat),scale="column",Rowv=NA,Colv=NA, ylab="Subject",main="Heatmap of Each Subject's Activity (Training Set)",col=cm.colors(256))
mtext(text="Activity", side=1, line=4.2)

##  Test
p1 <- predict(forest, test)
p2 <- predict(forest2, test)
sum(p1 != test$activity)/length(test$activity)
sum(p2 != test$activity)/length(test$activity)


# library(scatterplot3d)
# attach(train)
# scatterplot3d(train[,294],train[,295],train[,296])
# train$pcolor[train$activity=="laying"] <- "green4"
# train$pcolor[train$activity=="sitting"] <- "green3"
# train$pcolor[train$activity=="standing"] <- "green2"
# train$pcolor[train$activity=="walk"] <- "green1"
# train$pcolor[train$activity=="walkdown"] <- "green"
# train$pcolor[train$activity=="walkup"] <- "greenyellow"
# scatterplot3d(train[,294],train[,295],train[,296],color=train$pcolor)
# scatterplot3d(train[,282],train[,283],train[,284],color=train$pcolor)
# scatterplot3d(train[,559],train[,560],train[,561],color=train$pcolor, pch=19,type="h")
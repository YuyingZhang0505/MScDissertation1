fored<-read.table("fdnew.txt",sep=" ",header=TRUE,stringsAsFactors = TRUE)
fored$level<-factor(fored$level)

library(patchwork)
library(randomForest)
set(1234)
# divide data into train set and test set
ind<-sample(2,nrow(fored),replace=TRUE,prob=c(0.7,0.3))
train.data<-fored[ind==1,]
test.data<-fored[ind==2,]

# Rbu 
# prepare train data 
Rbu.train<-train.data[train.data$tree_sp_eu=='Rbu', ]
# optimized mtry=24
rate.Rbu<-0
for (i in 1:(ncol(Rbu.train)-1)) {
  set.seed(1234)
  rf.train.Rbu<-randomForest(level~.,data=Rbu.train[,c(3:35)],mtry=i,ntree=100)
  rate.Rbu[i]<-mean(rf.train.Rbu$err.rate)
  print(rf.train.Rbu)
}
# display the rate value of each model
rate.Rbu
plot(rate.Rbu)
# model construction
rf.Rbu<-randomForest(level~.,data=Rbu.train[,c(3:35)],mtry=24,ntree=500,importance=TRUE,
                    na.action=na.omit)
print(rf.Rbu)
# predictor importance
importance.Rbu<-round(importance(rf.Rbu),2)
# Mean Decrease Accuracy
sort(importance.Rbu[ ,6],decreasing=TRUE)
p.Rbu<-varImpPlot(rf.Rbu,main="Variable Importance")
rbu1<-round(sort(importance.Rbu[ ,6],decreasing=TRUE)/max(importance.Rbu[ ,6]),3)
barplot(rbu1[rbu1>0.3],ylab='Predictor Importance (relative units)',main='Beech',
        axisnames=TRUE)


# Gki 
# prepare train data 
Gki.train<-train.data[train.data$tree_sp_eu=='Gki', ]
# optimized mtry=31
rate.Gki<-0
for (i in 1:(ncol(Gki.train)-1)) {
  set.seed(1234)
  rf.train.Gki<-randomForest(level~.,data=Gki.train[,c(3:35)],mtry=i,ntree=100)
  rate.Gki[i]<-mean(rf.train.Gki$err.rate)
  print(rf.train.Gki)
}
# display the rate value of each model
rate.Gki
plot(rate.Gki)
# model construction
rf.Gki<-randomForest(level~.,data=Gki.train[,c(3:35)],mtry=31,ntree=500,importance=TRUE,
                     na.action=na.omit)
print(rf.Gki)
# predictor importance
importance.Gki<-round(importance(rf.Gki),2)
# Mean Decrease Accuracy
sort(importance.Gki[,6],decreasing=TRUE)
p.Gki<-varImpPlot(rf.Gki,main="Variable Importance")
Gki1<-round(sort(importance.Gki[ ,6],decreasing=TRUE)/max(importance.Gki[ ,6]),3)
barplot(Gki1[Gki1>0.5],ylab='Predictor Importance (relative units)',main='Spruce',
        axisnames=TRUE)

# Gfi 
# prepare train data 
Gfi.train<-train.data[train.data$tree_sp_eu=='Gfi', ]
# optimized mtry=24
rate.Gfi<-0
for (i in 1:(ncol(Gfi.train)-1)) {
  set.seed(1234)
  rf.train.Gfi<-randomForest(level~.,data=Gfi.train[,c(3:35)],mtry=i,ntree=100)
  rate.Gfi[i]<-mean(rf.train.Gfi$err.rate)
  print(rf.train.Gfi)
}
# display the rate value of each model
rate.Gfi
plot(rate.Gfi)
# model construction
rf.Gfi<-randomForest(level~.,data=Gfi.train[,c(3:35)],mtry=24,ntree=500,importance=TRUE,
                     na.action=na.omit)
print(rf.Gfi)
# predictor importance
importance.Gfi<-round(importance(rf.Gfi),2)
# Mean Decrease Accuracy
sort(importance.Gfi[,6],decreasing=TRUE)
p.Gfi<-varImpPlot(rf.Gfi,main="Variable Importance")
Gfi1<-round(sort(importance.Gfi[ ,6],decreasing=TRUE)/max(importance.Gfi[ ,6]),3)
barplot(Gfi1[Gfi1>0.14],ylab='Predictor Importance (relative units)',main='Pine',
        axisnames=TRUE)

# Tei 
# prepare train data 
Tei.train<-train.data[train.data$tree_sp_eu=='Tei', ]
# optimized mtry=21
rate.Tei<-0
for (i in 1:(ncol(Tei.train)-1)) {
  set.seed(1234)
  rf.train.Tei<-randomForest(level~.,data=Tei.train[,c(3:35)],mtry=i,ntree=100)
  rate.Tei[i]<-mean(rf.train.Tei$err.rate)
  print(rf.train.Tei)
}
# display the rate value of each model
rate.Tei
plot(rate.Tei)
# model construction
rf.Tei<-randomForest(level~.,data=Tei.train[,c(3:35)],mtry=21,ntree=500,importance=TRUE,
                     na.action=na.omit)
print(rf.Tei)
# predictor importance
importance.Tei<-round(importance(rf.Tei),2)
# Mean Decrease Accuracy
sort(importance.Tei[,6],decreasing=TRUE)
p.Tei<-varImpPlot(rf.Tei,main="Variable Importance")
Tei1<-round(sort(importance.Tei[ ,6],decreasing=TRUE)/max(importance.Tei[ ,6]),3)
barplot(Tei1[Tei1>0.3],ylab='Predictor Importance (relative units)',main='Oaks',
        axisnames=TRUE)

# Wta 
# prepare train data 
Wta.train<-train.data[train.data$tree_sp_eu=='Wta', ]
# optimized mtry=18
rate.Wta<-0
for (i in 1:(ncol(Wta.train)-1)) {
  set.seed(1234)
  rf.train.Wta<-randomForest(level~.,data=Wta.train[,c(3:35)],mtry=i,ntree=100)
  rate.Wta[i]<-mean(rf.train.Wta$err.rate)
  print(rf.train.Wta)
}
# display the rate value of each model
rate.Wta
plot(rate.Wta)
# model construction
rf.Wta<-randomForest(level~.,data=Wta.train[,c(3:35)],mtry=18,ntree=500,importance=TRUE,
                     na.action=na.omit)
print(rf.Wta)
# predictor importance
importance.Wta<-round(importance(rf.Wta),2)
# Mean Decrease Accuracy
sort(importance.Wta[,6],decreasing=TRUE)
p.Wta<-varImpPlot(rf.Wta,main="Variable Importance")
Wta1<-round(sort(importance.Wta[ ,6],decreasing=TRUE)/max(importance.Wta[ ,6]),3)
barplot(Wta1[Wta1>0.2],ylab='Predictor Importance (relative units)',main='Silver fir',
        axisnames=TRUE)

# Dgl 
# prepare train data 
Dgl.train<-train.data[train.data$tree_sp_eu=='Dgl', ]
# optimized mtry=14
rate.Dgl<-0
for (i in 1:(ncol(Dgl.train)-1)) {
  set.seed(1234)
  rf.train.Dgl<-randomForest(level~.,data=Dgl.train[,c(3:35)],mtry=i,ntree=100)
  rate.Dgl[i]<-mean(rf.train.Dgl$err.rate)
  print(rf.train.Dgl)
}
# display the rate value of each model
rate.Dgl
plot(rate.Dgl)
# model construction
rf.Dgl<-randomForest(level~.,data=Dgl.train[,c(3:35)],mtry=14,ntree=500,importance=TRUE,
                     na.action=na.omit)
print(rf.Dgl)
# predictor importance
importance.Dgl<-round(importance(rf.Dgl),2)
# Mean Decrease Accuracy
sort(importance.Dgl[,6],decreasing=TRUE)
sort(importance.Dgl[,7],decreasing=TRUE)
p.Dgl<-varImpPlot(rf.Dgl,main="Variable Importance")
Dgl1<-round(sort(importance.Dgl[ ,6],decreasing=TRUE)/max(importance.Dgl[ ,6]),3)
barplot(Dgl1[Dgl1>0.2],ylab='Predictor Importance (relative units)',main='Douglas fir',
        axisnames=TRUE)

names(rbu1[rbu1>0.3])
names(Gfi1[Gfi1>0.14])
names(Gki1[Gki1>0.5])
names(Tei1[Tei1>0.3])
names(Wta1[Wta1>0.2])
names(Dgl1[Dgl1>0.2])


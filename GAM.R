library(mgcv)
library(dplyr)

# Beech
Rbu.data<-fored[fored$tree_sp_eu=='Rbu', ]%>%
  select(c('tree_age','year','cwb_y','cwb_y_lag1','n_tot_wd','ac_tot_wd',
           'tmean_y_lag1','alt_m','nfk','d_relawat04','d_psi1200','nbv_ratio',
           'psilogmean_mean','slope_deg'))

Rbu.model<-gam(nbv_ratio~tree_age+year+ac_tot_wd+cwb_y+cwb_y_lag1+
                 n_tot_wd+alt_m+nfk+d_relawat04+tmean_y_lag1+
                 d_psi1200+psilogmean_mean+slope_deg,
               data=Rbu.data,method='REML')
summary(Rbu.model)
Rbu.model$aic
# re-selection on the basis of p-value
Rbu.model1<-gam(nbv_ratio~s(tree_age,k=10)+s(year,k=15)+s(cwb_y)+s(cwb_y_lag1)+
                 s(n_tot_wd)+nfk+s(d_relawat04)+s(tmean_y_lag1)+s(alt_m)+
                 s(d_psi1200)+s(psilogmean_mean)+s(slope_deg,k=10),
               data=Rbu.data,method='REML')
summary(Rbu.model1)
# model check
plot(Rbu.model1,pages=1,main='Partial Effect Plots of Beech')
Rbu.model1$aic
gam.check(Rbu.model1)
qq.gam(Rbu.model1,main='Q-Q plot of Beech')

# if there are not enough basis functions, it may not be wiggly enough to 
# capture the relationships in data/capture pattern

# Spruce
Gfi.data<-fored[fored$tree_sp_eu=='Gfi', ]%>%
  select(c('tree_age','year','geol_no','n_tot_wd','ac_tot_wd','tmean_y','nbv_ratio'))
Gfi.model<-gam(nbv_ratio~tree_age+year+geol_no+te(ac_tot_wd,n_tot_wd)+
                 tmean_y,data=Gfi.data,method='REML')
summary(Gfi.model)
Gfi.model$aic

Gfi.model1<-gam(nbv_ratio~s(tree_age,k=20)+s(year,k=15)+s(geol_no,k=15)+s(n_tot_wd)+
                 s(ac_tot_wd)+
                 s(tmean_y,k=15),data=Gfi.data,method='REML')
Gfi.model2<-gam(nbv_ratio~s(tree_age)+s(year)+s(geol_no)+
                  te(ac_tot_wd,n_tot_wd)+
                  s(tmean_y),data=Gfi.data,method='REML')
summary(Gfi.model2)
summary(Gfi.model1)

# Partial effect plots to show the component effect of each of the smooth
# or linear terms in the model, which add up to the overall prediction
# partial residuals are the difference between the partial effect and the data
# the checks to make sure that we have well-fit models
# via gam.check function we can test for whether we have enough basis functions

# model check
plot(Gfi.model,pages=1,main='Partial Effect Plots of Spruce')
Gfi.model1$aic
qq.gam(Gfi.model1,main='Q-Q plot of Spruce')
gam.check(Gfi.model1)


# Pine
Gki.data<-fored[fored$tree_sp_eu=='Gki', ]%>%
  select(c('tree_age','year','cwb_y','n_tot_wd','ac_tot_wd',
           'tmean_y_lag1','tmean_y','alt_m','nfk','relawat_mean','nbv_ratio',
           'psilogmean_mean','Es','twi50','H_spec','spei_24_oct',
           'awat_mean'))

Gki.model1<-gam(nbv_ratio~tree_age+year+cwb_y+n_tot_wd+ac_tot_wd+
                  tmean_y_lag1+tmean_y+nfk+alt_m+relawat_mean+awat_mean+
                  psilogmean_mean+Es+twi50+H_spec+spei_24_oct,
                data=Gki.data,method='REML')
summary(Gki.model1)
Gki.model1$aic

Gki.model2<-gam(nbv_ratio~s(tree_age,k=20)+s(tmean_y)+s(relawat_mean,k=20)+
                  s(awat_mean)+
                  s(psilogmean_mean,k=15)+s(twi50,k=20),
                data=Gki.data,method='REML')
summary(Gki.model2)

Gki.model3<-gam(nbv_ratio~s(tree_age)+s(tmean_y,k=15)+s(cwb_y)+te(n_tot_wd,ac_tot_wd)+
                  te(awat_mean,relawat_mean)+
                  s(psilogmean_mean,k=15)+s(twi50),
                data=Gki.data,method='REML')
summary(Gki.model3)
# model check
plot(Gki.model3,pages=1)
qq.gam(Gki.model3,main='Q-Q plot of Pine')
gam.check(Gki.model3)
Gki.model3$aic

# Oaks
Tei.data<-fored[fored$tree_sp_eu=='Tei', ]%>%
  select('tree_age','year','cwb_y','n_tot_wd','ac_tot_wd',
         'tmean_y_lag1','alt_m','nfk','nbv_ratio')
Tei.model<-gam(nbv_ratio~tree_age+year+cwb_y+n_tot_wd+ac_tot_wd+
                 tmean_y_lag1+alt_m+nfk,
               data=Tei.data,method='REML')
summary(Tei.model)
Tei.model$aic
gam.check(Tei.model)

Tei.model1<-gam(nbv_ratio~s(tree_age,k=5)+s(year,k=15)+
                 s(tmean_y_lag1,k=10)+s(nfk,k=15),
               data=Tei.data,method='REML')
summary(Tei.model1)
# model check
Tei.model1$aic
plot(Tei.model1,pages=1,main='Partial Effect Plots of Oaks')
qq.gam(Tei.model1,main='Q-Q plot of Oaks')
gam.check(Tei.model1)


# Silver fir
Wta.data<-fored[fored$tree_sp_eu=='Wta', ]%>%
  select('tree_age','year','n_tot_wd','ac_tot_wd','soil_no',
         'H_spec','alt_m','spei_24_oct','nbv_ratio')
Wta.model<-gam(nbv_ratio~tree_age+year+n_tot_wd+alt_m+ac_tot_wd+
                 H_spec+spei_24_oct+soil_no,
               data=Wta.data,method='REML')
summary(Wta.model)
Wta.model$aic

Wta.model1<-gam(nbv_ratio~s(tree_age,k=10)+s(year,k=15)+s(n_tot_wd,k=15)+
                  s(alt_m,k=15)+s(ac_tot_wd)+
                 s(spei_24_oct),
               data=Wta.data,method='REML')
summary(Wta.model1)
# model check
gam.check(Wta.model1)
Wta.model1$aic
qq.gam(Wta.model1,main='Q-Q plot of Silver fir')
plot(Wta.model,pages=1,main='Partial Effect Plots of Silver fir')


# Douglas fir
Dgl.data<-fored[fored$tree_sp_eu=='Dgl', ]%>%
  select('tree_age','source','year','ac_tot_wd','Es','cwb_y',
         'H_spec','alt_m','tmean_y_lag1','nbv_ratio')
Dgl.model<-gam(nbv_ratio~tree_age+year+factor(source)+alt_m+ac_tot_wd+
                 H_spec+cwb_y+tmean_y_lag1+Es,
               data=Dgl.data,method='REML')
summary(Dgl.model)
Dgl.model$aic


Dgl.model1<-gam(nbv_ratio~s(tree_age,k=15)+s(year)+factor(source)+
                s(cwb_y),
               data=Dgl.data,method='REML')
summary(Dgl.model1)
# model check
gam.check(Dgl.model1)
Dgl.model1$aic
qq.gam(Dgl.model1,main='Q-Q plot of Douglas fir')

Dgl.model1<-gam(nbv_ratio~s(tree_age)+year+factor(source)+s(alt_m)+log(ac_tot_wd)+
                 s(H_spec)+cwb_y+tmean_y_lag1+s(Es),
               data=Dgl.data,method='REML')
summary(Dgl.model1)
# model check
gam.check(Dgl.model1)
plot(Dgl.model1,pages=1,main='Partial Effect Plots of Douglas fir')
Dgl.model1$aic

# the k-fold cross validation
library(plyr)
library(caret)
library(pROC)
max=0
num=0

for (i in 1:5){
  train_Rbu<-Rbu.data[-folds.Rbu[[i]],]
  test_Rbu<-Rbu.data[folds.Rbu[[i]],]
  print(i)
  Rbu.model1<-gam(nbv_ratio~s(tree_age,k=10)+s(year,k=15)+s(cwb_y)+s(cwb_y_lag1)+
                    s(n_tot_wd)+nfk+s(d_relawat04)+s(tmean_y_lag1)+s(alt_m)+
                    s(d_psi1200)+s(psilogmean_mean)+s(slope_deg,k=10),
                  data=train_Rbu,method='REML')
  fold.predict<-predict(Rbu.model1,type='response',newdata=test_Rbu)
  fold.predict<-ifelse(fold.predict>0.5,1,0)
  test_Rbu$predict<-fold.predict
  # calculate accuracy and precision of test data
  true_value<-test_Rbu[,"nbv_ratio"]
  predict_value<-test_Rbu[,"predict"]
  error<-predict_value-true_value
  accuracy<-(nrow(test_Rbu)-sum(abs(error)))/nrow(test_Rbu)

  # calculate accuracy and precision of train data
  fold.predict2<-predict(Rbu.model1,type='response',newdata=train_Rbu)
  fold.predict2<-ifelse(fold.predict2>0.5,1,0)
  train_Rbu$predict<-fold.predict2
  true_value2<-train_Rbu[,"nbv_ratio"]
  predict_value2<-train_Rbu[,"predict"]
  error2<-predict_value2-true_value2
  accuracy2<-(nrow(train_Rbu)-sum(abs(error2)))/nrow(train_Rbu)
  
  print(accuracy)
  print(accuracy2)
  
  if(accuracy>max){
    max=accuracy
    num=i
  }
  
}
print(max)
print(num)


# AUC
folds.Rbu<-createFolds(y=Rbu.data$nbv_ratio,k=5)
auc_value.Rbu<-as.numeric()
for (i in 1:5){
  train_Rbu<-Rbu.data[-folds.Rbu[[i]],]
  test_Rbu<-Rbu.data[folds.Rbu[[i]],]
  Rbu.model1<-gam(nbv_ratio~s(tree_age,k=10)+s(year,k=15)+s(cwb_y)+s(cwb_y_lag1)+
                    s(n_tot_wd)+nfk+s(d_relawat04)+s(tmean_y_lag1)+s(alt_m)+
                    s(d_psi1200)+s(psilogmean_mean)+s(slope_deg,k=10),
                  data=train_Rbu,method='REML')
  fold.predict<-predict(Rbu.model1,type='response',newdata=test_Rbu)
  auc_value.Rbu<-append(auc_value.Rbu,as.numeric(auc(as.numeric(test_Rbu[,'nbv_ratio']),fold.predict)))
}
# 0.597
round(mean(auc_value.Rbu),3)

folds.Gfi<-createFolds(y=Gfi.data$nbv_ratio,k=5)
auc_value.Gfi<-as.numeric()
for (i in 1:5){
  train_Gfi<-Gfi.data[-folds.Gfi[[i]],]
  test_Gfi<-Gfi.data[folds.Gfi[[i]],]
  Gfi.model1<-gam(nbv_ratio~s(tree_age)+s(year)+s(geol_no)+s(n_tot_wd)+
                    s(ac_tot_wd)+
                    s(tmean_y),data=train_Gfi,method='REML')
  fold.predict<-predict(Gfi.model1,type='response',newdata=test_Gfi)
  auc_value.Gfi<-append(auc_value.Gfi,as.numeric(auc(as.numeric(test_Gfi[,'nbv_ratio']),fold.predict)))
}
# 0.527
round(mean(auc_value.Gfi),3)

folds.Tei<-createFolds(y=Tei.data$nbv_ratio,k=5)
auc_value.Tei<-as.numeric()
for (i in 1:5){
  train_Tei<-Tei.data[-folds.Gfi[[i]],]
  test_Tei<-Tei.data[folds.Gfi[[i]],]
  Tei.model1<-gam(nbv_ratio~s(tree_age,k=5)+s(year,k=15)+
                    s(tmean_y_lag1,k=10)+s(nfk,k=15),
                  data=train_Tei,method='REML')
  fold.predict<-predict(Tei.model1,type='response',newdata=test_Tei)
  auc_value.Tei<-append(auc_value.Tei,as.numeric(auc(as.numeric(test_Tei[,'nbv_ratio']),fold.predict)))
}
# 0.676
round(mean(auc_value.Tei),3)

folds.Gki<-createFolds(y=Gki.data$nbv_ratio,k=5)
auc_value.Gki<-as.numeric()
for (i in 1:5){
  train_Gki<-Gki.data[-folds.Gki[[i]],]
  test_Gki<-Gki.data[folds.Gki[[i]],]
  Gki.model2<-gam(nbv_ratio~s(tree_age,k=20)+s(tmean_y)+s(relawat_mean,k=20)+
                    s(awat_mean)+
                    s(psilogmean_mean,k=15)+s(twi50,k=20),
                  data=train_Gki,method='REML')
  fold.predict<-predict(Gki.model2,type='response',newdata=test_Gki)
  auc_value.Gki<-append(auc_value.Gki,as.numeric(auc(as.numeric(test_Gki[,'nbv_ratio']),fold.predict)))
}
# 0.795
round(mean(auc_value.Gki),3)


folds.Wta<-createFolds(y=Wta.data$nbv_ratio,k=5)
auc_value.Wta<-as.numeric()
for (i in 1:5){
  train_Wta<-Wta.data[-folds.Wta[[i]],]
  test_Wta<-Wta.data[folds.Wta[[i]],]
  Wta.model1<-gam(nbv_ratio~s(tree_age,k=10)+s(year,k=15)+s(n_tot_wd,k=15)+
                    s(alt_m,k=15)+s(ac_tot_wd)+
                    s(spei_24_oct),
                  data=train_Wta,method='REML')
  fold.predict<-predict(Wta.model1,type='response',newdata=test_Wta)
  auc_value.Wta<-append(auc_value.Wta,as.numeric(auc(as.numeric(test_Wta[,'nbv_ratio']),fold.predict)))
}
# 0.703
round(mean(auc_value.Wta),3)

folds.Dgl<-createFolds(y=Dgl.data$nbv_ratio,k=5)
auc_value.Dgl<-as.numeric()
for (i in 1:5){
  train_Dgl<-Dgl.data[-folds.Dgl[[i]],]
  test_Dgl<-Dgl.data[folds.Dgl[[i]],]
  Dgl.model1<-gam(nbv_ratio~s(tree_age)+year+factor(source)+s(alt_m)+log(ac_tot_wd)+
                    s(H_spec)+cwb_y+tmean_y_lag1+s(Es),
                  data=train_Dgl,method='REML')
  fold.predict<-predict(Dgl.model1,type='response',newdata=test_Dgl)
  auc_value.Dgl<-append(auc_value.Dgl,as.numeric(auc(as.numeric(test_Dgl[,'nbv_ratio']),fold.predict)))
}
# 0.579
round(mean(auc_value.Dgl),3)



# RMSE
folds.Rbu<-createFolds(y=Rbu.data$nbv_ratio,k=5)
rmse_Rbu<-as.numeric()
for (i in 1:5){
  train_Rbu<-Rbu.data[-folds.Rbu[[i]],]
  test_Rbu<-Rbu.data[folds.Rbu[[i]],]
  Rbu.model1<-gam(nbv_ratio~s(tree_age,k=10)+s(year,k=15)+s(cwb_y)+s(cwb_y_lag1)+
                    s(n_tot_wd)+nfk+s(d_relawat04)+s(tmean_y_lag1)+s(alt_m)+
                    s(d_psi1200)+s(psilogmean_mean)+s(slope_deg,k=10),
                  data=train_Rbu,method='REML')
  fold.predict<-predict(Rbu.model1,type='response',newdata=test_Rbu)
  mse_Rbu<-sum((fold.predict-test_Rbu[,"nbv_ratio"])^2)/length(test_Rbu[,"nbv_ratio"])
  rmse_Rbu<-append(rmse_Rbu,mse_Rbu^0.5)
}
# 0.09
round(mean(rmse_Rbu),3)

folds.Gfi<-createFolds(y=Gfi.data$nbv_ratio,k=5)
rmse_Gfi<-as.numeric()
for (i in 1:5){
  train_Gfi<-Gfi.data[-folds.Gfi[[i]],]
  test_Gfi<-Gfi.data[folds.Gfi[[i]],]
  Gfi.model1<-gam(nbv_ratio~s(tree_age)+s(year)+s(geol_no)+s(n_tot_wd)+
                    s(ac_tot_wd)+
                    s(tmean_y),data=train_Gfi,method='REML')
  fold.predict<-predict(Gfi.model1,type='response',newdata=test_Gfi)
  mse_Gfi<-sum((fold.predict-test_Gfi[,"nbv_ratio"])^2)/length(test_Gfi[,"nbv_ratio"])
  rmse_Gfi<-append(rmse_Gfi,mse_Gfi^0.5)
}
# 0.083
round(mean(rmse_Gfi),3)

folds.Gki<-createFolds(y=Gki.data$nbv_ratio,k=5)
rmse_Gki<-as.numeric()
for (i in 1:5){
  train_Gki<-Gki.data[-folds.Gki[[i]],]
  test_Gki<-Gki.data[folds.Gki[[i]],]
  Gki.model2<-gam(nbv_ratio~s(tree_age,k=20)+s(tmean_y)+s(relawat_mean,k=20)+
                    s(awat_mean)+
                    s(psilogmean_mean,k=15)+s(twi50,k=20),
                  data=train_Gki,method='REML')
  fold.predict<-predict(Gki.model2,type='response',newdata=test_Gki)
  mse_Gki<-sum((fold.predict-test_Gki[,"nbv_ratio"])^2)/length(test_Gki[,"nbv_ratio"])
  rmse_Gki<-append(rmse_Gki,mse_Gki^0.5)
}
# 0.123
round(mean(rmse_Gki),3)

folds.Tei<-createFolds(y=Tei.data$nbv_ratio,k=5)
rmse_Tei<-as.numeric()
for (i in 1:5){
  train_Tei<-Tei.data[-folds.Tei[[i]],]
  test_Tei<-Tei.data[folds.Tei[[i]],]
  Tei.model1<-gam(nbv_ratio~s(tree_age,k=5)+s(year,k=15)+
                    s(tmean_y_lag1,k=10)+s(nfk,k=15),
                  data=train_Tei,method='REML')
  fold.predict<-predict(Tei.model1,type='response',newdata=test_Tei)
  mse_Tei<-sum((fold.predict-test_Tei[,"nbv_ratio"])^2)/length(test_Tei[,"nbv_ratio"])
  rmse_Tei<-append(rmse_Tei,mse_Tei^0.5)
}
# 0.102
round(mean(rmse_Tei),3)

folds.Wta<-createFolds(y=Wta.data$nbv_ratio,k=5)
rmse_Wta<-as.numeric()
for (i in 1:5){
  train_Wta<-Wta.data[-folds.Wta[[i]],]
  test_Wta<-Wta.data[folds.Wta[[i]],]
  Wta.model1<-gam(nbv_ratio~s(tree_age,k=10)+s(year,k=15)+s(n_tot_wd,k=15)+
                    s(alt_m,k=15)+s(ac_tot_wd)+
                    s(spei_24_oct),
                  data=train_Wta,method='REML')
  fold.predict<-predict(Wta.model1,type='response',newdata=test_Wta)
  mse_Wta<-sum((fold.predict-test_Wta[,"nbv_ratio"])^2)/length(test_Wta[,"nbv_ratio"])
  rmse_Wta<-append(rmse_Wta,mse_Wta^0.5) 
}
# 0.115
round(mean(rmse_Wta),3)

folds.Dgl<-createFolds(y=Dgl.data$nbv_ratio,k=5)
rmse_Dgl<-as.numeric()
for (i in 1:5){
  train_Dgl<-Dgl.data[-folds.Dgl[[i]],]
  test_Dgl<-Dgl.data[folds.Dgl[[i]],]
  Dgl.model1<-gam(nbv_ratio~s(tree_age)+year+factor(source)+s(alt_m)+log(ac_tot_wd)+
                    s(H_spec)+cwb_y+tmean_y_lag1+s(Es),
                  data=train_Dgl,method='REML')
  fold.predict<-predict(Dgl.model1,type='response',newdata=test_Dgl)
  mse_Dgl<-sum((fold.predict-test_Dgl[,"nbv_ratio"])^2)/length(test_Dgl[,"nbv_ratio"])
  rmse_Dgl<-append(rmse_Dgl,mse_Dgl^0.5) 
}
# 0.097
round(mean(rmse_Dgl),3)



fd<-read.table("foresthealth.txt",sep=";",header=TRUE,stringsAsFactors = TRUE)

# on the basis of references
# preliminary variable selection
fd.pre<-subset(fd,select=c(tree_sp_eu,source,year,tmean_y,d_tmean20,globrad_y,cwb_y,d_veg,tmean_y_lag1,
                           globrad_y_lag1,cwb_y_lag1,spei_12_oct,spei_24_oct,
                           n_tot_wd,ac_tot_wd,basa_lev,nfk,tpi750,twi50,geol_no,
                           soil_no,water_no,nutri_no,depth_no,alt_m,slope_deg,
                           H_spec,Es,relawat_mean,d_relawat04,awat_mean,
                           psilogmean_mean,d_psi1200,tree_age,nbv_ratio))

# explore NA pattern 查看缺失值情况
library(VIM)
aggr(fd.pre,prop=TRUE,numbers=TRUE)
# impute NA value
# define helper function - single imputation MEAN 
FillNA<-function(x){
  for (i in 1:length(x)){
    x[,i][is.na(x[,i])]<-mean(x[,i],na.rm=TRUE)
  }
  x
}
fd.imp<-FillNA(fd.pre[,c(3:35)])
fd.new<-cbind('tree_sp_eu'=fd.pre$tree_sp_eu,'source'=fd.pre$source,fd.imp)

# calculate the mean and standard deviation
fd.mean<-aggregate(fd.new[,c(4:35)],by=list(fd.new$tree_sp_eu,fd.new$year),function(x) mean(x,na.rm=TRUE))
fd.std<-aggregate(fd.new[,c(4:35)],by=list(fd.new$tree_sp_eu,fd.new$year),function(x) sd(x,na.rm=TRUE))
fd1<-data.matrix(fd.new[,c(4:35)])
# convert the class of tree species into factor variable
tree_sp_eu<-factor(fd.new$tree_sp_eu,levels=c("Rbu","Gfi","Gki","Tei","Wta","Dgl"),
                   labels=c(1,2,3,4,5,6))
fit1<-manova(fd1~tree_sp_eu)
# 多元方差分析
s1<-summary(fit1)
# 对每个变量做单因素方差分析
# 要检查正态性和方差齐性吗?
s2<-summary.aov(fit1)
# visualization aov ?
# display the F value and p value for each response variable
s2$` Response tmean_y`$`F value`[1]

# carry out SNK test 有问题
library(agricolae)
#model1<-aov(data.matrix(fd.new[,c(3:35)])~fd.new$tree_sp_eu)
out<-SNK.test(fit1,"tree_sp_eu")
out$group
plot(out) # visualization

# visualization bar plot or line graph
library(ggplot2)
fd.mean$nbv_ratio<-fd.mean$nbv_ratio*100
p<-ggplot(fd.mean,aes(x=Group.2,y=nbv_ratio))+
  geom_bar(
    stat='identity',aes(fill=Group.1),position=position_dodge(0.9))+
  xlab('Year')+ylab('Defoliation(%)')+
  theme(legend.position="bottom")+
  labs(fill="Tree Species")
p

# plot by tree species
library(patchwork)
p1<-ggplot(fd.mean[fd.mean$Group.1=='Rbu',],aes(x=Group.2,y=nbv_ratio))+
  geom_bar(stat='identity',width=0.5)+
  labs(x='Year',y='Defoliation(%)',title='Rbu')


p2<-ggplot(fd.mean[fd.mean$Group.1=='Gfi',],aes(x=Group.2,y=nbv_ratio))+
  geom_bar(stat='identity',width=0.5)+
  labs(x='Year',y='Defoliation(%)',title='Gfi')


p3<-ggplot(fd.mean[fd.mean$Group.1=='Gki',],aes(x=Group.2,y=nbv_ratio))+
  geom_bar(stat='identity',width=0.5)+
  labs(x='Year',y='Defoliation(%)',title='Gki')


p4<-ggplot(fd.mean[fd.mean$Group.1=='Tei',],aes(x=Group.2,y=nbv_ratio))+
  geom_bar(stat='identity',width=0.5)+
  labs(x='Year',y='Defoliation(%)',title='Tei')


p5<-ggplot(fd.mean[fd.mean$Group.1=='Wta',],aes(x=Group.2,y=nbv_ratio))+
  geom_bar(stat='identity',width=0.5)+
  labs(x='Year',y='Defoliation(%)',title='Wta')


p6<-ggplot(fd.mean[fd.mean$Group.1=='Dgl',],aes(x=Group.2,y=nbv_ratio))+
  geom_bar(stat='identity',width=0.5)+
  labs(x='Year',y='Defoliation(%)',title='Dgl')

(p1|p2)/(p3|p4)/(p5|p6)


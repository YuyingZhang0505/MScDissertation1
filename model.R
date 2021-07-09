fd<-read.table("foresthealth.txt",sep=";",header=TRUE,stringsAsFactors = TRUE)

# divide whole dataset into six sub-data
Rbu<-fd[fd$tree_sp_eu=='Rbu', ]
Gfi<-fd[fd$tree_sp_eu=='Gfi', ]
Gki<-fd[fd$tree_sp_eu=='Gki', ]
Tei<-fd[fd$tree_sp_eu=='Tei', ]
Wta<-fd[fd$tree_sp_eu=='Wta', ]
Dgl<-fd[fd$tree_sp_eu=='Dgl', ]
# on the basis of references
# preliminary variable selection
fd.pre<-subset(fd,select=c(tree_sp_eu,nbv_ratio,source,year,tmean_y,d_tmean20,
                           globrad_y,cwb_y,tmean_y_lag1,globrad_y_lag1,
                           cwb_y_lag1,spei_12_oct,spei_24_oct,n_tot_wd,ac_tot_wd,
                           basa_lev,nfk,tpi750,twi50,geol_no,soil_no,water_no,
                           nutri_no,depth_no,alt_m,slope_deg,H_spec,Es,
                           relawat_mean,d_relawat04,awat_mean,psilogmean_mean,
                           d_psi1200,tree_age))
# divide selected dataset into six sub-data
Rbu.pre<-fd.pre[fd.pre$tree_sp_eu=='Rbu', ]
Gfi.pre<-fd.pre[fd.pre$tree_sp_eu=='Gfi', ]
Gki.pre<-fd.pre[fd.pre$tree_sp_eu=='Gki', ]
Tei.pre<-fd.pre[fd.pre$tree_sp_eu=='Tei', ]
Wta.pre<-fd.pre[fd.pre$tree_sp_eu=='Wta', ]
Dgl.pre<-fd.pre[fd.pre$tree_sp_eu=='Dgl', ]

# exploratory analysis --- mean
Rbu.pmean<-round(sapply(Rbu.pre[ ,c(2,5:34)],function(x) mean(x,na.rm=TRUE)),2)
Gfi.pmean<-round(sapply(Gfi.pre[ ,c(2,5:34)],function(x) mean(x,na.rm=TRUE)),2)
Gki.pmean<-round(sapply(Gki.pre[ ,c(2,5:34)],function(x) mean(x,na.rm=TRUE)),2)
Tei.pmean<-round(sapply(Tei.pre[ ,c(2,5:34)],function(x) mean(x,na.rm=TRUE)),2)
Wta.pmean<-round(sapply(Wta.pre[ ,c(2,5:34)],function(x) mean(x,na.rm=TRUE)),2)
Dgl.pmean<-round(sapply(Dgl.pre[ ,c(2,5:34)],function(x) mean(x,na.rm=TRUE)),2)
# exploratory analysis --- standard deviation
Rbu.pstd<-round(apply(Rbu.pre[ ,c(2,5:34)],2,function(x) sd(x,na.rm=TRUE)),2)
Gfi.pstd<-round(apply(Gfi.pre[ ,c(2,5:34)],2,function(x) sd(x,na.rm=TRUE)),2)
Gki.pstd<-round(apply(Gki.pre[ ,c(2,5:34)],2,function(x) sd(x,na.rm=TRUE)),2)
Tei.pstd<-round(apply(Tei.pre[ ,c(2,5:34)],2,function(x) sd(x,na.rm=TRUE)),2)
Wta.pstd<-round(apply(Wta.pre[ ,c(2,5:34)],2,function(x) sd(x,na.rm=TRUE)),2)
Dgl.pstd<-round(apply(Dgl.pre[ ,c(2,5:34)],2,function(x) sd(x,na.rm=TRUE)),2)

# create a new data frame to display the mean and standard deviation of trees
df1<-data.frame(Rbu.pmean,Rbu.pstd,Gfi.pmean,Gfi.pstd,
                   Gki.pmean,Gki.pstd,Tei.pmean,Tei.pstd,
                   Wta.pmean,Wta.pstd,Dgl.pmean,Dgl.pstd)
# calculate the total number of trees in each species
n1<-length(Rbu.pre[,1]);n2<-length(Gfi.pre[,1])
n3<-length(Gki.pre[,1]);n4<-length(Tei.pre[,1])
n5<-length(Wta.pre[,1]);n6<-length(Dgl.pre[,1])
df2<-rbind('N'=c(n1,0,n2,0,n3,0,n4,0,n5,0,n6,0),df1)
colnames(df2)<-c('Beech(mean)','std','Spruce(mean)','std','Pine(mean)','std',
                 'Oaks(mean)','std','Silver fir(mean)','std','Douglas fir(mean)','std')
write.csv(df2,file="fdmean.csv",col.names=TRUE,row.names=TRUE)
library(xtable)
# the Latex code to display the df2 in the form of table
xtable(df2,caption='Initial Analysis of Defolation and environmental variables')


# explore NA pattern 
sum(is.na(fd.pre))
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
fd.imp<-FillNA(fd.pre[,c(4:34)])
fd.new<-cbind('tree_sp_eu'=fd.pre$tree_sp_eu,'nbv_ratio'=fd.pre$nbv_ratio,
              'source'=fd.pre$source,fd.imp)

# calculate the mean and standard deviation
fd.mean<-aggregate(fd.new[,c(2,5:34)],by=list(fd.new$tree_sp_eu,fd.new$year),
                   function(x) mean(x,na.rm=TRUE))
fd.std<-aggregate(fd.new[,c(2,5:34)],by=list(fd.new$tree_sp_eu,fd.new$year),
                  function(x) sd(x,na.rm=TRUE))

Rbum<-round(fd.mean[fd.mean$Group.1=='Rbu',][,-1],2)
Gfim<-round(fd.mean[fd.mean$Group.1=='Gfi',][,-1],2)
Gkim<-round(fd.mean[fd.mean$Group.1=='Gki',][,-1],2)
Teim<-round(fd.mean[fd.mean$Group.1=='Tei',][,-1],2)
Wtam<-round(fd.mean[fd.mean$Group.1=='Wta',][,-1],2)
Dglm<-round(fd.mean[fd.mean$Group.1=='Dgl',][,-1],2)


# bar plot of defoliation (shown in species)
par(mfrow=c(3,2))
barplot(Rbum$nbv_ratio,names.arg=Rbum$Group.2,ylim=c(0,0.4),
        xlab='Year',ylab='Defoliation',main='Rbu')
barplot(Gfim$nbv_ratio,names.arg=Gfim$Group.2,ylim=c(0,0.4),
        xlab='Year',ylab='Defoliation',main='Gfi')
barplot(Gkim$nbv_ratio,names.arg=Gkim$Group.2,ylim=c(0,0.4),
        xlab='Year',ylab='Defoliation',main='Gki')
barplot(Teim$nbv_ratio,names.arg=Teim$Group.2,ylim=c(0,0.4),
        xlab='Year',ylab='Defoliation',main='Tei')
barplot(Wtam$nbv_ratio,names.arg=Wtam$Group.2,ylim=c(0,0.4),
        xlab='Year',ylab='Defoliation',main='Wta')
barplot(Dglm$nbv_ratio,names.arg=Dglm$Group.2,ylim=c(0,0.4),
        xlab='Year',ylab='Defoliation',main='Dgl')

# bar plot of ac_tot_wd (shown in species)
par(mfrow=c(3,2))
barplot(Rbum$ac_tot_wd,names.arg=Rbum$Group.2,
        xlab='Year',ylab='ac_tot_wd',main='Rbu')
barplot(Gfim$ac_tot_wd,names.arg=Gfim$Group.2,
        xlab='Year',ylab='ac_tot_wd',main='Gfi')
barplot(Gkim$ac_tot_wd,names.arg=Gkim$Group.2,
        xlab='Year',ylab='ac_tot_wd',main='Gki')
barplot(Teim$ac_tot_wd,names.arg=Teim$Group.2,
        xlab='Year',ylab='ac_tot_wd',main='Tei')
barplot(Wtam$ac_tot_wd,names.arg=Wtam$Group.2,
        xlab='Year',ylab='ac_tot_wd',main='Wta')
barplot(Dglm$ac_tot_wd,names.arg=Dglm$Group.2,
        xlab='Year',ylab='ac_tot_wd',main='Dgl')

# bar plot of n_tot_wd (shown in species)
par(mfrow=c(3,2))
barplot(Rbum$n_tot_wd,names.arg=Rbum$Group.2,
        xlab='Year',ylab='n_tot_wd',main='Rbu')
barplot(Gfim$n_tot_wd,names.arg=Gfim$Group.2,
        xlab='Year',ylab='n_tot_wd',main='Gfi')
barplot(Gkim$n_tot_wd,names.arg=Gkim$Group.2,
        xlab='Year',ylab='n_tot_wd',main='Gki')
barplot(Teim$n_tot_wd,names.arg=Teim$Group.2,
        xlab='Year',ylab='n_tot_wd',main='Tei')
barplot(Wtam$n_tot_wd,names.arg=Wtam$Group.2,
        xlab='Year',ylab='n_tot_wd',main='Wta')
barplot(Dglm$n_tot_wd,names.arg=Dglm$Group.2,
        xlab='Year',ylab='n_tot_wd',main='Dgl')

# bar plot of alt_m (shown in species)
par(mfrow=c(3,2))
barplot(Rbum$alt_m,names.arg=Rbum$Group.2,
        xlab='Year',ylab='alt_m',main='Rbu')
barplot(Gfim$alt_m,names.arg=Gfim$Group.2,
        xlab='Year',ylab='alt_m',main='Gfi')
barplot(Gkim$alt_m,names.arg=Gkim$Group.2,
        xlab='Year',ylab='alt_m',main='Gki')
barplot(Teim$alt_m,names.arg=Teim$Group.2,
        xlab='Year',ylab='alt_m',main='Tei')
barplot(Wtam$alt_m,names.arg=Wtam$Group.2,
        xlab='Year',ylab='alt_m',main='Wta')
barplot(Dglm$alt_m,names.arg=Dglm$Group.2,
        xlab='Year',ylab='alt_m',main='Dgl')


fd1<-data.matrix(fd.new[,c(2,4:35)])
# convert the class of tree species into factor variable
tree_sp_eu<-as.factor(fd.new$tree_sp_eu)

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

# plot by tree species - Defoliation
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

# set the new variable - level to represent defoliation level
fd.new$nbv_ratio<-format(fd.new$nbv_ratio,scientific=FALSE,digits=3)
fd.new$nbv_ratio<-round(as.numeric(fd.new$nbv_ratio),3)
fd.new$level<-0
for (i in 1:nrow(fd.new)) {
  if (fd.new[i,]$nbv_ratio>0.0 & fd.new[i,]$nbv_ratio<=0.1) {
    fd.new[i,]$level = 'Step1'
  } else if (fd.new[i,]$nbv_ratio>0.11 & fd.new[i,]$nbv_ratio<=0.25) {
    fd.new[i,]$level = 'Step2'
  } else if (fd.new[i,]$nbv_ratio>0.26 & fd.new[i,]$nbv_ratio<=0.60){
    fd.new[i,]$level = 'Step3'
  } else if (fd.new[i,]$nbv_ratio>0.61 & fd.new[i,]$nbv_ratio<=0.99){
    fd.new[i,]$level = 'Step4'
  } else {
    fd.new[i,]$level = 'Step5'
  }
}
fd.new$level<-factor(fd.new$level)
write.table(fd.new,file="fdnew.txt",col.names=TRUE,row.names=TRUE)

# ANOVA
fd.new$tree_sp_eu<-factor(fd.new$tree_sp_eu)
aov<-aov(year~tree_sp_eu,data=fd.new)

# carry out SNK test
library(agricolae)
out<-SNK.test(aov,"tree_sp_eu")
out$group
plot(out) # visualization


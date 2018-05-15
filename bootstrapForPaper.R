## bootstrap the get an idea of the variability in the parameters estimates obtained through ML


## A. Ledberg 2018 05 15


## number of free parameters in the model. Must be 12 or 13
nparams=13

## number of bootstrap resamples
nboot=500

## number of iterations with the optimization, i.e repeated applications of the minimizer
## niter is actually the true number of iterations -1, it would be better to run until convergence but
## that takes too much time with the current implementation. 

niter=2

## load the data by running
source("loadInsarkData.R")

## load initial conditions
xinit=data.frame()
if (nparams==12){
    xinit=read.table("optimal_params12.txt")
}
if (nparams==13){
    xinit=read.table("optimal_params13.txt")
}

xinit<-xinit[,1]

## take log of variance terms to preserve non-negativity in the fitting
indx=c(5,7,8,10)
xinitl<-xinit
for (i in indx){
    xinitl[i]=log(xinit[i])
}




xinit0=xinitl
## start from a more "neutral" poisition in the 13 parameter case to avoid bias
if (nparams==13){
    xinit0[9]=0
}

## make a data-set containing only the three variables we are interested in

rdat<-data[c("hgrp","knst","weight")]

source("fitModelForPaper.R")
bootpars<-list()
bootfit<-list()
for (n in 1:nboot){
    print(paste("sim ", n))
    indx<-sample(dim(rdat)[1],dim(rdat)[1],replace=TRUE)
    tmpdat<-rdat[indx,]
    mfit<-fitModel(tmpdat,posflag=1,normflag=0,xinit=xinit0,npar=nparams,verbose=0)
    for (i in 1:niter){
        val=mfit$val
        mfit<-fitModel(tmpdat,posflag=1,normflag=0,xinit=mfit$par,npar=nparams,verbose=0)
        d=mfit$val-val
    }
    bootpars<-cbind(bootpars,mfit$par)
    bootfit<-cbind(bootfit,mfit$val)
}

## approximate CI:s, assuming 500 bootstraps
cis<-list()
for (i in 1:nparams){
    p<-unlist(bootpars[i,])
    p<-p[order(p)]
    ##cis<-rbind(cis,c(p[2],p[99]))
    ##cis<-rbind(cis,c(p[25],p[476]))
    cis<-rbind(cis,c(p[12],p[489]))
}

cis<-as.data.frame(cis)
names(cis)<-c("lower","upper")
cis$lower<-unlist(cis$lower)
cis$upper<-unlist(cis$upper)
fn=""
if (nparams==12){
    fn="confidence_interval_12parameters_500boots.txt"
}
if (nparams==13){
    fn="confidence_interval_13parameters_500boots.txt"
}
write.table(cis,file=fn,sep=",")

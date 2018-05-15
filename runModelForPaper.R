## code to fit the probability model to the data. This is essentially an interface of fitModelForPaper.R

## 2018 05 14
## Anders Ledberg: anders.ledberg@gmail.com 

source("loadInsarkData.R")

source("fitModelForPaper.R")

####################################################################################XS
## fit the 13 parmameter model
xinit=read.table("optimal_params13.txt")
xinit<-xinit[,1]

## need to use the logarithm of the variance term to ensure that these are positive
indx=c(5,7,8,10)
for (i in indx){
    xinit[i]=log(xinit[i])
}

mfit<-fitModel(data,posflag=1,normflag=0,xinit=xinit,npar=13,verbose=1)

d=1
while (abs(d)>0){
    val=mfit$val
    mfit<-fitModel(data,posflag=1,normflag=0,xinit=mfit$par,npar=13,verbose=1)
    d=mfit$val-val
}

## convert log to real space
params=mfit$par
indx=c(5,7,8,10)
for (i in indx){
    params[i]=exp(params[i])
}




#######################################################################################
## fit the 12 parmameter model
xinit=read.table("optimal_params12.txt")
xinit<-xinit[,1]

## need to use the logarithm of the variance term to ensure that these are positive
indx=c(5,7,8,10)
for (i in indx){
    xinit[i]=log(xinit[i])
}


mfit<-fitModel(data,posflag=1,normflag=0,xinit=xinit,npar=12)

d=1
while (abs(d)>0){
    val=mfit$val
    mfit<-fitModel(data,posflag=1,normflag=0,xinit=mfit$par,npar=12)
    d=mfit$val-val
}

## convert log to real space
params=mfit$par
indx=c(5,7,8,10)

for (i in indx){
    params[i]=exp(params[i])
}


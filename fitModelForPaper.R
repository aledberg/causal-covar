## code to fit the probability model to the data, normalization is optional and default is to not normalize
## Fitting is done by maximizing the likelihood numerically using the Melder-Nead algorithm. It is likely 
## that there are smarter and faster ways to find the optimal parameters, if so please let me know :)

## 2018 05 14
## Anders Ledberg: anders.ledberg@gmail.com 


fitModel<-function(data,normflag=0,posflag=0,xinit=c(),npar=13,verbose=0){
    require(data.table)
    data=data.table(data)
    ## standardize the data for each conscripton year 
    if (normflag==1){
        ## standaridzed variables (mean zero and std dev = 1)
        if ( ("normknst" %in% names(dum)==FALSE) | ("normhgrp" %in% names(dum)==FALSE) ){
            data$normknst=0
            data$normhgrp=0
            
            mdat<-data[,list("mknst"=mean(knst),"mhgrp"=mean(hgrp),"stdknst"=sqrt(var(knst)),"stdhgrp"=sqrt(var(hgrp))),by=con]
            
            mdat<-mdat[order(mdat$con),]
            
            for (i in 1:dim(mdat)[1]){
                indx=data$con==mdat$con[i]
                data$normknst[indx]=(data$knst[indx]-mdat$mknst[i])/mdat$stdknst[i]
                data$normhgrp[indx]=(data$hgrp[indx]-mdat$mhgrp[i])/mdat$stdhgrp[i]
            }
        }
    }

    ## now sort data according to the covariate of interest and create data sets for each level of
    ## this variable
    data<-data[order(data$weight),]
    N<-dim(data)[1]
    varnames<-c("","")
    varindx<-vector(length=2)
    if (normflag==0){
        varnames[1]="knst"
        varnames[2]="hgrp"
    }else
    {
        varnames[1]="normknst"
        varnames[2]="normhgrp"
    }
    varindx[1]=which(names(data)==varnames[1])
    varindx[2]=which(names(data)==varnames[2])
    if (verbose==1){
        for (i in 1:2){
            print(varindx[i])
        }
    }
    datxy<-data[,c(varindx[1],varindx[2]),with=FALSE]
    
    data$X<-data[,varindx[1],with=FALSE]
    data$Y<-data[,varindx[2],with=FALSE]

    ## number of levels
    uniquez=unique(data$weight)
    zlist<-uniquez
    nlev=length(uniquez)

    ## size of each level
    levsize=vector(length=nlev)
    datlist<-list()
    pos=1
    for (i in 1:nlev){
        levsize[i]=sum(data$weight==uniquez[i])
        datlist[[i]]=datxy[pos:(levsize[i]+pos-1),]
        pos=pos+levsize[i]
    }
    
    if (length(xinit)==0){
        xinit<-initialConditions(data,normflag=normflag,posflag=posflag)
    }
    if (verbose==1){
        print(xinit)
    }
    ## minimize the likelihood, conditional upon the number of parmaters to include

    if (npar==13)
        par<-optim(xinit, mylrf13,control=list(maxit=10000),method=c("Nelder-Mead"),zlist=zlist,datlist=datlist,posflag=posflag,verbose=verbose)
    else if(npar==12)
        par<-optim(xinit, mylrf12,control=list(maxit=10000),method=c("Nelder-Mead"),zlist=zlist,datlist=datlist,posflag=posflag,verbose=verbose)
    else
        print("unknown optimization function")
    ##
    
    ##
    ##par<-optim(xinit, mylrf12l,control=list(maxit=10000),method=c("Nelder-Mead"),zlist=zlist,datlist=datlist,posflag=posflag)

    
    return(par)
    
}

## 13 parameter model where there is correlation between
## linear terms and constant terms, the last three parameters
## are in position 11 to 13
mylrf13<-function(x,zlist,datlist,posflag=0,verbose=0){
    require(mvtnorm)
    ## unpack x
    a1=x[1] ## constant term for first variable
    a2=x[2] ## constant term for second variable
    b1=x[3] ## average slope term for first variable
    b2=x[4] ## average slope term for second variable
    ## error variance
    ## force the variance terms to be positive
    if (posflag==1){
        e11=exp(x[5])
        e12=(x[6])
        e22=exp(x[7])
        b11=exp(x[8])
        b12=(x[9])
        b22=exp(x[10])
        ## covariance between linear and constant term eq1
        be1=x[11]
        ## covariance between linear and constant term eq2
        be2=x[12]
        ## covariance between linear in one eq and constant term in other
        ## these are taken to be the same else they cannot be identified
        b1e2=x[13]
    }
    else{
        e11=x[5]
        e12=x[6]
        e22=x[7]
        ## variance of the slope terms
        b11=x[8]
        b12=x[9]
        b22=x[10]
        ## covariance between linear and constant term eq1
        be1=x[11]
        ## covariance between linear and constant term eq2
        be2=x[12]
        ## covariance between linear in one eq and constant term in other
        ## these are taken to be the same else they cannot be identified
        b1e2=x[13]
    }

    ## covariance matrix of error terms
    C=matrix(nrow=2,ncol=2)
    ##print(C)
    C[1,1]=e11
    C[1,2]=e12
    C[2,1]=e12
    C[2,2]=e22

    ## need to check that C is a admissible covariance matrix
    badcovflag=0
    largeNumber=10.0e+10
    if (abs(e12) > sqrt(e11)*sqrt(e22))
        badcovflag=1
    
    ## covariance matrix of slope terms
    S=matrix(nrow=2,ncol=2)
    S[1,1]=b11
    S[1,2]=b12
    S[2,1]=b12
    S[2,2]=b22

    ## check if S is admissible
    if (abs(b12) > sqrt(b11)*sqrt(b22))
        badcovflag=1
    
    ## covariance matrix of slope-error terms
    CS=matrix(nrow=2,ncol=2)
    CS[1,1]=be1
    CS[1,2]=b1e2
    CS[2,1]=b1e2
    CS[2,2]=be2

    ## check if these are admissible
    if (abs(be1) > sqrt(e11)*sqrt(b11))
        badcovflag=1
    if (abs(be2) > sqrt(e22)*sqrt(b22))
        badcovflag=1
    if (abs(b1e2) > sqrt(e22)*sqrt(b11))
        badcovflag=1
    if (abs(b1e2) > sqrt(e11)*sqrt(b22))
        badcovflag=1
    
    ## there is no way around checking also that the
    ## full variance covariance matrix is positive
    ## definite. suck...
    F=matrix(nrow=4,ncol=4,data=0.0)
    F[1,1]=b11
    F[2,2]=e11
    F[3,3]=b22
    F[4,4]=e22
    F[1,2]=be1
    F[1,3]=b12
    F[1,4]=b1e2
    F[2,3]=b1e2
    F[2,4]=e12
    F[3,4]=be2
    SS=F
    for (i in 1:4)
        for (j in i:4)
            SS[j,i]=SS[i,j]
    if (det(SS)<0)
        badcovflag=1
    
    nlev=length(zlist)
    val=0.0
    for (l in 1:nlev){
        ##print(l)
        currz=zlist[l]
        m1=a1+b1*currz
        m2=a2+b2*currz
        ## the covariance is the sum of the covariance matrices under the assumption of independence
        ## but we need to scale the variances with Z
        Sigma=C+S*currz*currz+2*CS*currz
        ##print(Sigma)
        if (badcovflag==1)
            val=val+largeNumber
        else
            val=val+(-sum(log(dmvnorm(x=datlist[[l]],mean=c(m1,m2),sigma=Sigma))))
    }
    ##print(C)
    if (verbose==1){
        print(val)
    }
    return(val)
}


## 12 parameter model where there is no correlation between
## linear terms, but between linear and constant terms
mylrf12<-function(x,zlist,datlist,posflag=0,verbose=0){
    require(mvtnorm)
    ## unpack x
    a1=x[1] ## constant term for first variable
    a2=x[2] ## constant term for second variable
    b1=x[3] ## average slope term for first variable
    b2=x[4] ## average slope term for second variable
    ## error variance
    ## force the variance terms to be positive
    if (posflag==1){
        e11=exp(x[5])
        e12=(x[6])
        e22=exp(x[7])
        b11=exp(x[8])
        ## in this routine the 9th element codes for the
        ## correlation between the linear and constant term accros
        b1e2=(x[9])
        b22=exp(x[10])
        ## covariance between linear and constant term eq1
        be1=x[11]
        ## covariance between linear and constant term eq2
        be2=x[12]
    }
    else{
        e11=x[5]
        e12=x[6]
        e22=x[7]
        ## variance of the slope terms
        b11=x[8]
        b1e2=x[9]
        b22=x[10]
        ## covariance between linear and constant term eq1
        be1=x[11]
        ## covariance between linear and constant term eq2
        be2=x[12]
    }

    ## covariance matrix of error terms
    C=matrix(nrow=2,ncol=2)
    ##print(C)
    C[1,1]=e11
    C[1,2]=e12
    C[2,1]=e12
    C[2,2]=e22

    ## need to check that C is a admissible covariance matrix
    badcovflag=0
    largeNumber=10.0e+10
    if (abs(e12) > sqrt(e11)*sqrt(e22))
        badcovflag=1
    
    
    ## covariance matrix of slope terms
    S=matrix(nrow=2,ncol=2)
    S[1,1]=b11
    S[1,2]=0
    S[2,1]=0
    S[2,2]=b22

    ## no need to check here :)

    ## covariance matrix of slope-error terms
    CS=matrix(nrow=2,ncol=2)
    CS[1,1]=be1
    CS[1,2]=b1e2
    CS[2,1]=b1e2
    CS[2,2]=be2

    ## check if these are admissible
    if (abs(be1) > sqrt(e11)*sqrt(b11))
        badcovflag=1
    if (abs(be2) > sqrt(e22)*sqrt(b22))
        badcovflag=1
    if (abs(b1e2) > sqrt(e22)*sqrt(b11))
        badcovflag=1
    if (abs(b1e2) > sqrt(e11)*sqrt(b22))
        badcovflag=1

    ## there is no way around checking also that the
    ## full variance covariance matrix is positive
    ## definite. suck...
    F=matrix(nrow=4,ncol=4,data=0.0)
    F[1,1]=b11
    F[2,2]=e11
    F[3,3]=b22
    F[4,4]=e22
    F[1,2]=be1
    F[1,3]=0
    F[1,4]=b1e2
    F[2,3]=b1e2
    F[2,4]=e12
    F[3,4]=be2
    SS=F
    for (i in 1:4)
        for (j in i:4)
            SS[j,i]=SS[i,j]
    if (det(SS)<0)
        badcovflag=1

    

    nlev=length(zlist)
    val=0.0
    for (l in 1:nlev){
        ##print(l)
        currz=zlist[l]
        m1=a1+b1*currz
        m2=a2+b2*currz
        ## the covariance is the sum of the covariance matrices under the assumption of independence
        ## but we need to scale the variances with Z
        Sigma=C+S*currz*currz+2*CS*currz
        ##print(Sigma)
        if (badcovflag==1)
            val=val+largeNumber
        else
            val=val+(-sum(log(dmvnorm(x=datlist[[l]],mean=c(m1,m2),sigma=Sigma))))
    }
    ##print(C)
    if (verbose==1){
        print(val)
    }
    return(val)
}

## generate initial conditions for the maximum liklelihood fit
## this might not work all that well...
initialConditions<-function(data,normflag=0,posflag=0){
    ## output variable
    inits<-vector(length=10)    
    ## fit a non-conditional model to get the linear terms
    if (normflag==0){
        mod<-lm(cbind(knst,hgrp)~1+weight,data=data)
        inits[1]<-mod$coefficients[1,1]
        inits[2]<-mod$coefficients[1,2]
        inits[3]<-mod$coefficients[2,1]
        inits[4]<-mod$coefficients[2,2]
        su=summary(mod)
        if (posflag==0){
            inits[5]<-su[1]$"Response knst"$sigma**2/10
            ##inits[6]<- -cov(data$hgrp,data$knst)/10
            inits[6]<- -10
            inits[7]<-su[2]$"Response hgrp"$sigma**2/10
            inits[8]<-inits[5]/70./70.
            inits[10]<-inits[7]/70./70.
            ##inits[9]<-inits[6]/70./70.0*2
            inits[9]<- inits[8]/2
        }else
        {
            inits[5]<-log(su[1]$"Response knst"$sigma**2/10)
            ##inits[6]<- -cov(data$hgrp,data$knst)/10
            inits[6]<- -100
            inits[7]<-log(su[2]$"Response hgrp"$sigma**2/10)
            inits[8]<-inits[5]/65./70.
            inits[10]<-inits[7]/65./70.
            ##inits[9]<-inits[6]/70./70.0*2
            ##inits[9]<- inits[8]/2
            inits[9]<- 0
        }
    }
    else{
        mod<-lm(cbind(normknst,normhgrp)~1+weight,data=data)
        inits[1]<-mod$coefficients[1,1]
        inits[2]<-mod$coefficients[1,2]
        inits[3]<-mod$coefficients[2,1]
        inits[4]<-mod$coefficients[2,2]
        su=summary(mod)
        if (posflag==0){
            inits[5]<-su[1]$"Response normknst"$sigma**2/10
            inits[6]<- cov(data$hgrp,data$knst)/100
            inits[7]<-su[2]$"Response normhgrp"$sigma**2/10
            inits[8]<-inits[5]/70./70.
            inits[10]<-inits[7]/70./70.
            ##inits[9]<-inits[6]/70./70.0*2
            inits[9]<- 0
        }else
        {
            inits[5]<-log(su[1]$"Response normknst"$sigma**2/20)
            inits[6]<- cov(data$normhgrp,data$normknst)/100
            ##inits[6]<- -100
            inits[7]<-log(su[2]$"Response normhgrp"$sigma**2/20)
            inits[8]<-log(0.001)
            inits[10]<-log(0.001)
            ##inits[9]<-inits[6]/70./70.0*2
            ##inits[9]<- inits[8]/2
            inits[9]<- 0
        }
    }       
    return(inits)
}


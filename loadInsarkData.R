## This file is part of the causal covariability project.

## 2018 05 14
## Anders Ledberg: anders.ledberg@gmail.com 


## contents:
##
## This file contains R-code to load the data needed to do the analysis and figure in the paper. The data
## is cleaned and the output of executing this script is a data.frame called "data". This data.frame is used
## by other scripts.
##
## This file also contains code to load the original INSARK.csv data-file and perform some initial "cleaning" of the data
## This file (INSARK.csv) can be downloaded from www.riksarkivet.se. However, the cleaned data on the three relevant
## variabels used in the paper are provided so the first steps (0.1-0.5) in this script can be safely ignored. They are given
## here in order to describe the process of cleaning.



## The steps starting with 0 can be ignored unless there is interest in downloading and working with the
## original dataset (INSARK.csv)
##################################################################
## Step 0.1: load the data file

## dirname="/home/anders/projects/ideas/insark/"
## ##dirname="./"
## fn="INSARK.csv"
## data=read.table(paste(dirname,fn,sep=""),header=TRUE,sep="\t",fill=TRUE,stringsAsFactors=FALSE,fileEncoding="UTF8",skipNul=TRUE,quote="")

## ##################################################################
## ## Step 0.2: some cases appeared at more than one occasion, remove all except the first

## indx <- data$RA_NR>1
## data<-data[!indx,]


## ##################################################################
## ## Step 0.3: detect year of birth and remove erronous cases

## data$year<-as.numeric(substr(data$PENR,1,2))
## indx<-data$year> 50 & !is.na(data$year)
## data<-data[indx,]

## ##################################################################
## ## Step 0.4: find the year of conscription and remove erroneous values plus all data from
## ## 1996 and 1997 as these are much fewer cases than the other years

## data$con=as.numeric(substr(data$INST,1,2))
## indx<-data$con> 69 & data$con < 95
## data<-data[indx,]


## ##################################################################
## ## Step 0.5: create numeric variables for height, weight, knee-extension strength and hand-grip strength
## ##           and remove cases with erroneous values

## data$height<-as.numeric(data$LNGD)
## data$weight<-as.numeric(data$MASS)
## data$knst<-as.integer(data$KNST)
## data$hgrp<-as.integer(data$HGRP)

## indx<-!is.na(data$knst) & !is.na(data$hgrp) & !is.na(data$height) & !is.na(data$weight)
## data<-data[indx,]



## #############################################################################
## ## Step 0.6: extract the variables to be used in the next step and save these to disk (insark_selection.rds)
## ##           this file should be identical to the one that can be downloaded from this page

## selvar=c("weight","height","knst","hgrp","RA_RAEKNARE")
## datasel=data[selvar]

## ## change the names of the last variable, this serves as an identification variable
## selvar[5]<-"id"
## names(datasel)<-selvar

## fn="insark_selection.rds"
## saveRDS(datasel,file=fn)




####################################################################################
####################################################################################
####################################################################################
## Start from here unless downloading INSARK.csv


###################################################################################
## Step 1.0: load the data
fn="insark_selection.rds"
data<-readRDS(fn)


###################################################################################
## Step 2.0: keep only at the central part of the weight distribution in order
##           to make interventions and a linear causal model be more reasonable

ulim<-74
llim<-65
data<-data[data$weight >=llim & data$weight <=ulim,]



###################################################################################
## Step 3.0: restrict variabilty in height 

llim<-178
ulim<-181
data<-data[data$height >=llim & data$height <=ulim,]

###################################################################################
## Step 4.0: remove outliers in the strength variables

indx<-data$knst>=150
data<-data[indx,]

indx<-data$hgrp>=200
data<-data[indx,]

## the data frame "data" is now ready to be used by the other R-scripts.

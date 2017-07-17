# Script from Thomas - sent via email 2017-07-04

# if not already installed
# devtools::install_github("fishfollower/SAM/stockassessment", ref="mack")

rm(list=ls())

library(stockassessment)


setwd("D:/WKWIDE 2017 final run")

cn<-read.ices("cn.dat")
cw<-read.ices("cw.dat")
dw<-read.ices("dw.dat")
lf<-read.ices("lf.dat")
lw<-read.ices("lw.dat")
mo<-read.ices("mo.dat")
nm<-read.ices("nm.dat")
pf<-read.ices("pf.dat")
pm<-read.ices("pm.dat")
sw<-read.ices("sw.dat")
surveys<-read.ices("survey2.dat")

recap<-read.table("tag.dat", header=TRUE)
recap<-recap[recap$Type==1 & recap$RecaptureY<=2006,]
recap<-recap[recap[,1]>=min(as.numeric(rownames(sw))), ]

recap2<-read.table("tag3.dat", header=TRUE)
recap2$r<-round(recap2$r)
recap2<-recap2[recap2$Nscan>0,]
recap<-rbind(recap,recap2)

age<- recap$ReleaseY - recap$Yearclass
recap  <- recap[age>1 & age<13,]


#### Remove the tags recapture in the release year
recap <- recap[recap$ReleaseY!=recap$RecaptureY,]

W<-matrix(NA,nrow=nrow(cn), ncol=ncol(cn))
W[as.numeric(rownames(cn))<2000]<-10
attr(cn,"weight")<-W

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn,
                    prop.mature=mo,
                    stock.mean.weight=sw,
                    catch.mean.weight=cw,
                    dis.mean.weight=dw,
                    land.mean.weight=lw,
                    prop.f=pf,
                    prop.m=pm,
                    natural.mortality=nm,
                    land.frac=lf,
                    recapture=recap)

#### model configuration

conf<-defcon(dat)
conf$keyLogFsta[1,] <- c(0,1,2,3,4,5,6,7,7,7,7,7,7)
conf$keyVarObs[4,]      <- c(-1,-1,-1, 3,4,4,4,4,4,4,4,4,-1)
conf$fbarRange <- c(4,8)
conf$corFlag <- 0
conf$fixVarToWeight<-1
conf$obsCorStruct[] <- c("ID", "ID", "ID", "AR", "ID")
#conf$keyCorObs[4,7:11]<-0
conf$keyCorObs[4,]<-0

par<-defpar(dat,conf)




# model fitting



fit <-sam.fit(dat,conf,par, newtonsteps=0)


save(fit,file="fitAR-2.RData")

### REsidual plots : take a long time to run
library(TMB)
options(mc.cores=3)
dis<-dat$fleetTypes[dat$aux[,"fleet"]]==5
res<-oneStepPredict(fit$obj, observation.name="logobs", data.term.indicator="keep", discrete=FALSE, subset=which(!dis))
res2<-oneStepPredict(fit$obj, observation.name="logobs", data.term.indicator="keep", discrete=TRUE, conditional=which(!dis),
                     subset=which(dis), method ="oneStepGeneric", range=c(0,Inf), parallel=TRUE)
totalres<-rep(NA,nrow(dat$aux))
totalres[!dis]<-res$residual
totalres[dis]<-res2$residual
myres<-data.frame(dat$aux, residual=totalres)
save(myres, file="myresAR-2.RData")

tagres<-myres[myres$fleet==5,]

myres$residual<-ave(myres$residual, myres$year, myres$age, myres$fleet, FUN=function(x)mean(x)*sqrt(length(x)))
class(myres)<-"samres"

pdf("resAR-2.pdf",10,10)
plot(myres)
dev.off()

pdf("tagresAR-2.pdf",10,10)
plotby(tagres$year, tagres$RecaptureY-tagres$year, tagres$residual, by=tagres$age, xlab="Year", ylab="No years out")
dev.off()

pdf("figs.pdf")
ssbplot(fit)
fbarplot(fit)
recplot(fit)
catchplot(fit)
dev.off()



pdf("summary.pdf")
set<-c(fit)
ssbplot(set)
fbarplot(set)
recplot(set)
catchplot(set)
dev.off()
library(TMB)
source('estimator.R')
library(parallel)
library(doMC)
library(plyr)
library(dplyr)
registerDoMC(cores=15)
                                        #library(Rgadget)
debug <- FALSE

if(debug){
  system('rm *.{o,so}')
  compile('gadgetLite.cpp',"-O0 -g",CXX='g++')
  dyn.load(dynlib("gadgetLite"))
} else {
  compile('gadgetLite.cpp')
  dyn.load(dynlib("gadgetLite"))
}

load('runDatrick.RData')
map <- list(recruits=factor(rep(NA,20)),
            SIa = factor(rep(NA,4)),
            #rickmu = factor(NA),
            #ricklambda = factor(NA),
#            aComm = factor(NA),
#            aSurv = factor(NA),
            log_sigma = factor(NA),
            initial=factor(rep(NA,9)),
            recl=factor(NA),
            recsd=factor(NA),
            meanrec=factor(NA),
            lik_sigma = factor(rep(NA,5)),
            linf=factor(NA), bComm=factor(NA), bSurv=factor(NA))

## fixed effects recruits
map.rec <- map
map.rec$recruits <- NULL

## Random effects
mapR <- map
mapR$recruits <- NULL
mapR$log_sigma <- NULL


init.par <- gen$parameters
init.par$log_sigma <- log(0.0000001)


design.dat <-
    expand.grid(wgts=seq(0,1,by=0.1),
                exp.type=c('rnd','rick','fixed'),
                err=c(0,1),
                age=c(0,1)) %>%
    filter(!(wgts > 0 & (exp.type!='rnd')))
design.dat$ID <- 1:nrow(design.dat)

run.func <- function(x){
    print(x$ID)
    if(x$err==1){
        dat <- gen.err$data
    } else {
        dat <- gen$data
    }

    rnd <- NULL
    if(x$exp.type == 'rnd'){
        mapGL <- mapR
        dat$rfunc <- 1
        rnd <- 'recruits'
        dat$compW[6] <- x$wgts
    } else if(x$exp.type == 'rick'){
        mapGL <- map
        dat$rfunc <- 1
    } else {
        mapGL <- map.rec
        dat$rfunc <- 0
    }

    try(mGL(dat,init.par,FALSE,rnd,mapGL),TRUE)
    
}


res <- dlply(design.dat,~ID,run.func,.parallel=TRUE)

x <- 1

if(FALSE){

obj0 <- MakeADFun(gen$data,gen$parameters,
                 map=map,
                 DLL="gadgetLite")
obj0$hessian <- TRUE
opt0 <- nlminb(obj0$par,obj0$fn,obj0$gr)
R0 <- obj0$report(opt0$par)


## remove age data
gen$data$compW[c(3,5)] <- 0
objage <- MakeADFun(gen$data,gen$parameters,
                 map=map,
                 DLL="gadgetLite")
objage$hessian <- TRUE
optage <- nlminb(objage$par,objage$fn,objage$gr)
Rage <- objage$report(optage$par)


gen$data$compW[c(3,5)] <- 1



## now with error


gen.err$data$rfunc <- 1
obj.err <- MakeADFun(gen.err$data,gen.err$parameters,
                  map=map,
                  DLL="gadgetLite")
obj.err$hessian <- TRUE


## now with error and no age
gen.err$data$compW[c(3,5)] <- 0
obj.errage <- MakeADFun(gen.err$data,gen.err$parameters,
                  map=map,
                  DLL="gadgetLite")
obj.errage$hessian <- TRUE
gen.err$data$compW[c(3,5)] <- 1


#opt <- do.call("optim",obj)
opt <- nlminb(obj.errage$par,obj.errage$fn,obj.errage$gr)
R.errage <- obj.errage$report(opt)

opt.err <- nlminb(obj.err$par,obj.err$fn,obj.err$gr)
pl <- obj.err$env$parList(opt.err$par)
pl.age <- obj.errage$env$parList(opt$par)
pl0 <- obj0$env$parList(opt0$par)
#plrec <- objrec$env$parList(optrec$par)
pl0age <- objage$env$parList(optage$par)


## random effects

gen$data$rfunc <- 1
gen$data$compW <- c(rep(1,5),1)

gen.err$data$rfunc <- 1
gen.err$data$compW <- c(rep(1,5),1)
obj2 <- MakeADFun(gen$data,pl0,random="recruits",
                  map=mapR,#list(SIa = factor(rep(NA,4))),
                  DLL="gadgetLite")
tmp <- obj2$par
upper <-  tmp+abs(0.9*tmp)
lower <- tmp-abs(0.9*tmp)
upper['log_sigma'] <- log(0.35)
lower['log_sigma'] <- log(0.05)

opt2 <- nlminb(obj2$par,obj2$fn,obj2$gr,
               upper=upper,
               lower=lower)
pl2 <- obj2$env$parList(opt2$par)


obj3 <- MakeADFun(gen.err$data,pl,random="recruits",
                  map=mapR,#list(SIa = factor(rep(NA,4))),
                  DLL="gadgetLite")

tmp <- obj3$par
upper <-  tmp+abs(0.9*tmp)
lower <- tmp-abs(0.9*tmp)
upper['log_sigma'] <- log(0.35)
lower['log_sigma'] <- log(0.05)

opt3 <- nlminb(obj3$par,obj3$fn,obj3$gr,
               upper=upper,
               lower=lower)
pl3 <- obj3$env$parList(opt3$par)

gen$data$rfunc <-1
gen$data$compW[c(3,5)] <- 0

obj4 <- MakeADFun(gen$data,pl0age,random="recruits",
                  map=mapR,#list(SIa = factor(rep(NA,4))),
                  DLL="gadgetLite")
tmp <- obj4$par
upper <-  tmp+abs(0.9*tmp)
lower <- tmp-abs(0.9*tmp)
upper['log_sigma'] <- log(0.35)
lower['log_sigma'] <- log(0.05)

opt4 <- nlminb(obj4$par,obj4$fn,obj4$gr,
               upper=upper,
               lower=lower)
pl4 <- obj4$env$parList(opt4$par)

gen.err$data$rfunc <-1
gen.err$data$compW[6] <- 1
gen.err$data$compW[c(3,5)] <- 0

obj5 <- MakeADFun(gen.err$data,pl.age,random="recruits",
                  map=mapR,#list(SIa = factor(rep(NA,4))),
                  DLL="gadgetLite")
tmp <- obj5$par
upper <-  tmp+abs(0.9*tmp)
lower <- tmp-abs(0.9*tmp)
upper['log_sigma'] <- log(0.5)
lower['log_sigma'] <- log(0.05)

opt5 <- nlminb(obj5$par,obj5$fn,obj5$gr,
               upper=upper,
               lower=lower)
pl5 <- obj5$env$parList(opt5$par)


## fixed effects
map.rec <- map
map.rec$recruits <- NULL

gen$data$rfunc <- 0
gen.err$data$rfunc <-0
gen.err$data$compW[6] <- 0
gen.err$data$compW[c(3,5)] <- 1
gen$data$compW[6] <- 0
gen$data$compW[c(3,5)] <- 1

obj.rec.err <- MakeADFun(gen.err$data,gen$parameters,
                         map=map.rec,#list(SIa = factor(rep(NA,4))),
                         DLL="gadgetLite")

opt.rec.err <- nlminb(obj.rec.err$par,obj.rec.err$fn,obj.rec.err$gr)
pl.rec.err <- obj.rec.err$env$parList(opt.rec.err$par)


obj.rec <- MakeADFun(gen$data,gen$parameters,
                     map=map.rec,#list(SIa = factor(rep(NA,4))),
                     DLL="gadgetLite")

opt.rec <- nlminb(obj.rec$par,obj.rec$fn,obj.rec$gr)
pl.rec <- obj.rec$env$parList(opt.rec$par)


gen.err$data$compW[c(3,5)] <- 0

gen$data$compW[c(3,5)] <- 0

obj.rec.err.age <- MakeADFun(gen.err$data,gen$parameters,
                         map=map.rec,#list(SIa = factor(rep(NA,4))),
                         DLL="gadgetLite")

opt.rec.err.age <- nlminb(obj.rec.err.age$par,
                      obj.rec.err.age$fn,obj.rec.err.age$gr)
pl.rec.err.age <- obj.rec.err.age$env$parList(opt.rec.err.age$par)


obj.rec.age <- MakeADFun(gen$data,gen$parameters,
                     map=map.rec,#list(SIa = factor(rep(NA,4))),
                     DLL="gadgetLite")

opt.rec.age <- nlminb(obj.rec.age$par,obj.rec.age$fn,obj.rec.age$gr)
pl.rec.age <- obj.rec.age$env$parList(opt.rec.age$par)

save(opt0,opt.err,optage,opt2,opt3,file='opts.RData')
save(pl0,pl,pl.age,pl0age,pl2,pl3,pl4,pl5,pl.rec,pl.rec.err,
     pl.rec.age,pl.rec.err.age,file='pls.RData')
}

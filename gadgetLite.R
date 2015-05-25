library(TMB)
library(Rgadget)
debug <- TRUE

if(debug){
  system('rm *.{o,so}')
  compile('gadgetLite.cpp',"-O0 -g",CXX='g++')
  dyn.load(dynlib("gadgetLite"))
} else {
  compile('gadgetLite.cpp')
  dyn.load(dynlib("gadgetLite"))
}
  
load('runDatricker.RData')
map <- list(#recruits=factor(rep(NA,20)),
            SIa = factor(rep(NA,4)),
            rickmu = factor(NA),
            initial=factor(rep(NA,9)),
            linf=factor(NA), bComm=factor(NA), bSurv=factor(NA))
load('runDat.RData')
obj0 <- MakeADFun(gen$data,gen$parameters,
                 map=map,
                 DLL="gadgetLite")
obj0$hessian <- TRUE
opt0 <- nlminb(obj0$par,obj0$fn,obj0$gr)

gen0 <- gen
## remove info on recruitment
gen$parameters$recruitment <- rep(10,20)

objrec <- MakeADFun(gen$data,gen$parameters,
                 map=map,
                 DLL="gadgetLite")
objrec$hessian <- TRUE
optrec <- nlminb(objrec$par,objrec$fn,objrec$gr)
## remove age data
gen$data$compW[c(3,5)] <- 0
objage <- MakeADFun(gen$data,gen$parameters,
                 map=map,
                 DLL="gadgetLite")
objage$hessian <- TRUE
optage <- nlminb(objage$par,objage$fn,objage$gr)
## now with error
gen.err$data$compW[c(3,5)] <- 1
obj.err <- MakeADFun(gen.err$data,gen.err$parameters,
                  map=map,
                  DLL="gadgetLite")
obj.err$hessian <- TRUE


## now with error
gen.err$data$compW[c(3,5)] <- 0
obj.errage <- MakeADFun(gen.err$data,gen.err$parameters,
                  map=map,
                  DLL="gadgetLite")
obj.errage$hessian <- TRUE



#opt <- do.call("optim",obj)
opt <- nlminb(obj.errage$par,obj.errage$fn,obj.errage$gr)
opt.err <- nlminb(obj.err$par,obj.err$fn,obj.err$gr)
pl <- obj.err$env$parList(opt.err$par)
pl.age <- obj.errage$env$parList(opt$par)
pl0 <- obj0$env$parList(opt0$par)
plrec <- objrec$env$parList(optrec$par)
pl0age <- objage$env$parList(optage$par)
blu <- data.frame(type=rep(c('true','base','baserec',
                      'basenoage','error','noage'),each=20),
                  year=rep(1:20,6),
                  rec=c(exp(gen.err$parameters$recruits),
                      exp(pl0$recruits),
                      exp(plrec$recruits),
                      exp(pl0age$recruits),
                      exp(pl.age$recruits),
                      exp(pl$recruits)))

gen.err$data$compW <- rep(1,6)
obj2 <- MakeADFun(gen.err$data,pl,random="recruits",
                  map=map,#list(SIa = factor(rep(NA,4))),
                  DLL="gadgetLite")
opt2 <- nlminb(obj2$par,obj2$fn,obj2$gr)

opt$hessian ## <-- FD hessian from optim
obj$he() ## <-- Analytical hessian


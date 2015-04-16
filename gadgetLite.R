library(TMB)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
debug <- FALSE
compile('gadgetLite.cpp')
dyn.load(dynlib("gadgetLite"))
if(debug){
  system('rm *.{o,so}')
  compile('gadgetLite.cpp',"-O0 -g",CXX='g++')
  dyn.load(dynlib("gadgetLite"))
}
load('runDat.RData')
map <- list(recruits=factor(rep(NA,20)),
            initial=factor(rep(NA,10)),
            linf=factor(NA), bComm=factor(NA), bSurv=factor(NA))

obj <- MakeADFun(data,parameters)#,map=map)
obj$hessian <- TRUE

tmp <- obj$report()
dimnames(tmp$stkArr) <- list(age=1:10,length=1:90,year=1:20,step=1:4)
dimnames(tmp$commArr) <- list(age=1:10,length=1:90,year=1:20,step=1:4)
stkdat <- as.data.frame.table(tmp$stkArr,responseName='pred') %>%
  mutate(age=as.numeric(age),
         length=as.numeric(length),
         year=as.numeric(year),
         step=as.numeric(step))
commdat <- as.data.frame.table(tmp$commArr,responseName='pred')

stkcomp <- simdat$stocks %>% 
  group_by(age,length,year,step) %>%
  summarise(obs=sum(num)) %>%
  left_join(data.table(stkdat))

dat <- filter(stkcomp) %>% 
  group_by(length,year,step) %>%
  summarise(p=sum(pred),o=sum(obs))

 ggplot(dat,aes(length,o))+ geom_line() + geom_line(aes(length,p),lty=2) + facet_wrap(~year+step,scale='free_y')
#opt <- do.call("optim",obj)
opt <- nlminb(obj$par,obj$fn,obj$gr)

opt$hessian ## <-- FD hessian from optim
obj$he() ## <-- Analytical hessian


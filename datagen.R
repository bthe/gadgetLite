library(Rgadget)

opt <- gadget.options('simple1stock')
opt$stocks$imm$n <- 1e6*exp(rnorm(20))
gm <- gadget.skeleton(time=opt$time,area=opt$area,stock=opt$stocks,opt$fleets)
gm.sim <- compiler::cmpfun(gadget.simulate)
sim <- gm.sim(gm)
tDF <- compiler::cmpfun(Rgadget:::toDataFrame)
simdat <- tDF(sim)


SI <- acast(Rgadget:::survey.index(subset(simdat$stocks,step==2),
                                   split=25*0:4),
            year~SIgroup)
ldistSI <- acast(Rgadget:::ldist(subset(simdat$fleets,fleet=='surv'&step==2)),
                 lgroup~year,value.var='p')

ldistComm <- acast(Rgadget:::ldist(subset(simdat$fleets,fleet=='comm')),
                   lgroup~year~step,value.var='p')

aldistSI <- acast(Rgadget:::aldist(subset(simdat$fleets,fleet=='surv'&step==2)),
                  age~lgroup~year,value.var='p')

aldistComm <- acast(Rgadget:::aldist(subset(simdat$fleets,fleet=='comm')),
                    age~lgroup~year~step,value.var='p')
fleetCatches <- acast(ddply(simdat$fleets,~year+step,summarise,
                            catch=sum(num*10^(-5)*length^3)),
                      year~step)

data <- list(SI=t(SI),
             ldistSI=ldistSI,
             ldistComm=ldistComm,
             aldistSI=aldistSI,
             aldistComm=aldistComm,
             fleetCatches=fleetCatches,
             SIlgroups=1:4*25,
             firstyear=1,
             lastyear=20,
             maxlgr=15,
             minage=1,
             maxage=10,
             minlength=5,
             maxlength=90,
             M=rep(0.2,10),
             initSigma=c(2.2472, 2.8982, 4.0705, 4.9276,
                         5.5404, 5.8072, 6.0233, 8, 9, 9),
             wa=c(10^(-5),3),
             compW=rep(1,5))


parameters <- list(recruits=sim$gm@stocks$imm@renewal.data$number*1e-5,
                   recl=9.897914,
                   recsd=2.2472,
                   initial=10*exp(-0.2*2:10),
                   aComm=-8.2,
                   bComm=0.22,
                   aSurv=-4.5,
                   bSurv=0.3,
                   k=log(0.09),
                   linf=115,
                   beta=log(200),
                   SIa=rep(0,4),
                   meanrec=mean(sim$gm@stocks$imm@renewal.data$number*1e-5),
                   log_sigma = log(100))

save(opt,gm,sim,simdat,parameters,data,file='runDat.RData')

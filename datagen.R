library(Rgadget)

## ricker w. random variations
opt.rick <- gadget.options('ricker')
opt.rick$fleets$comm$Fy <- c(rep(0.2,40),rep(0.1,40))
opt.rick$stocks$mat$spawnparameters$p1 <- rlnorm(20,sdlog=0.3)

## stochastic recruitment
opt <- gadget.options('simple1stock')
opt$stocks$imm$n <- 1e6*rlnorm(20,sdlog=0.5)
opt$fleets$comm$Fy <- opt.rick$fleets$comm$Fy
## run the stuff

gm <- gadget.skeleton(time=opt$time,area=opt$area,stock=opt$stocks,opt$fleets)
gm.rick <- gadget.skeleton(time=opt.rick$time,area=opt.rick$area,
                           stock=opt.rick$stocks,opt.rick$fleets)
if(length(grep('gm.sim',ls()))==0){
    gm.sim <- compiler::cmpfun(gadget.simulate)
    tDF <- compiler::cmpfun(Rgadget:::toDataFrame)
}
sim <- gm.sim(gm)
sim.rick <- gm.sim(gm.rick)

simdat <- tDF(sim)
simdat.rick <- tDF(sim.rick)

obsgen <- function(simdat,sim,opt,rfunc=0,
                   error=FALSE){
    if(error){
        sigma <- c(0.5,0.3,0.3,0.7)
    } else {
        sigma <- 0
    }
    
    SI <- acast(Rgadget:::survey.index(subset(simdat$stocks,step==2),
                                       split=25*0:4, sigma = sigma),
                year~SIgroup)

    if(error){
        sigma <- 0.5
    } else {
        sigma <- 0
    }
       

    tmp <- Rgadget:::ldist(subset(simdat$fleets,fleet=='surv'&step==2),
                           sigma = sigma)
    tmp <- ddply(tmp,~year, mutate,
                 #num=round(num*100/sum(num)),
                 p=num/sum(num))
    ldistSI <- acast(tmp,
                     lgroup~year,
                     value.var='p')


    ldistComm <- acast(Rgadget:::ldist(subset(simdat$fleets,fleet=='comm'),
                                     sigma = sigma),
                       lgroup~year~step,value.var='p')
    
    if(error){
        sigma <- 0.1
    } else {
        sigma <- 0
    }


    tmp <- Rgadget:::aldist(subset(simdat$fleets,fleet=='surv'&step==2),
                                     sigma = sigma)
    tmp <- ddply(tmp,~year, mutate,
                 #num=round(num*100/sum(num)),
                 p=num/sum(num))
    
    aldistSI <- acast(tmp,
                      age~lgroup~year,value.var='p')

    tmp <- Rgadget:::aldist(subset(simdat$fleets,fleet=='comm'),
                                     sigma = sigma)
    tmp <- ddply(tmp,~year, mutate,
                 #num=round(num*100/sum(num)),
                 p=num/sum(num))
    
    aldistComm <- acast(tmp,
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
               compW=c(rep(1,5),0),
               rfunc=0,
               fleetMort=opt$fleets$comm$Fy)
  
  
  parameters <- list(recruits=rep(0,20),#
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
                     meanrec= 1,    #,
                     log_sigma = .1,
                     rickmu=1,
                     ricklambda=3.5,
                     lik_sigma=rep(0,5))
  return(list(data=data,parameters=parameters))
}
gen <- obsgen(simdat,sim,opt)
gen.err <- obsgen(simdat,sim,opt,error=TRUE)
save(opt,gm,sim,simdat,gen,gen.err,file='runDat.RData')

gen <- obsgen(simdat.rick,sim.rick,opt,rfunc=1)
gen.err <- obsgen(simdat.rick,sim.rick,opt.rick,rfunc=1,error=TRUE)
save(opt.rick,gm.rick,sim.rick,simdat.rick,
     gen,gen.err,file='runDatrick.RData')

library(Rgadget)

library(tidyr)
#load('Rs.RData')
load('res.RData')
load('res2.RData')
load('runDatrick.RData')
simdat <- simdat.rick



tmp.func <- function(tmp,var='pred'){
    ## call the function
#    gc()
#    obj <- MakeADFun(gen$data,par,
#                 map=map,
#                 DLL="gadgetLite")
#    tmp <- obj$report()
#    if(class(foo) == 'try-error')
#        return(data.frame())
#    tmp <- foo$report
    ## do a bit of formatting
    dimnames(tmp$stkArr) <- list(age=1:10,length=1:90,year=1:20,step=1:4)
    dimnames(tmp$commArr) <- list(age=1:10,length=1:90,year=1:20,step=1:4)
    dimnames(tmp$predidx) <- dimnames(gen$data$SI)
    dimnames(tmp$commArrLP) <- list(length=1:90,year=1:20,step=1:4)
    dimnames(tmp$commArrALP) <- list(age=1:10,length=1:90,year=1:20,step=1:4)
    dimnames(tmp$survArrLP) <- list(length=1:90,year=1:20,step=1:4)
    dimnames(tmp$survArrALP) <- list(age=1:10,length=1:90,year=1:20,step=1:4)

    ## 
    stkdat <- as.data.frame.table(tmp$stkArr,responseName=var) %>%
        mutate(age=as.numeric(age),
               length=as.numeric(length),
               year=as.numeric(year),
               step=as.numeric(step)) %>%
                   data.table()

    commdat <- as.data.frame.table(tmp$commArr,responseName=var) %>%
        mutate(age=as.numeric(age),
               length=as.numeric(length),
               year=as.numeric(year),
               step=as.numeric(step)) %>%
                   data.table()

    
    sidat <- as.data.frame.table(tmp$predidx,responseName=var) %>%
        data.table()

    ldistComm <- as.data.frame.table(tmp$commArrLP,responseName=var)%>%
        mutate(
               length=as.numeric(length),
               year=as.numeric(year),
               step=as.numeric(step)) %>%
                   data.table()
    aldistComm <- as.data.frame.table(tmp$commArrALP,responseName=var)%>%
        mutate(age=as.numeric(age),
               length=as.numeric(length),
               year=as.numeric(year),
               step=as.numeric(step)) %>%
                   data.table()
    
    ldistSI <- filter(as.data.frame.table(tmp$survArrLP,responseName=var),
                      step==2) %>%
                          mutate(
                                 length=as.numeric(length),
                                 year=as.numeric(year),
                                 step=as.numeric(step)) %>%
                                     data.table()
    
    
    return(list(stkdat=stkdat,#commdat=commdat,
                sidat=sidat,ldistComm=ldistComm,
                aldistComm=aldistComm,ldistSI=ldistSI))
}

names(res) <- 1:nrow(design.dat)
sidat <- ldply(1:nrow(design.dat),function(x){
    if(class(res[[x]]) == 'try-error')
        tmp <- res2[[as.character(x)]]
    else
        tmp <- res[[as.character(x)]]
#    dimnames(x$report$stkArr) <- list(age=1:10,length=1:90,year=1:20,step=1:4)
    tmp <- as.data.frame.table(tmp$report$predidx,responseName='pred') %>%
        data.table()
    tmp$ID <- x
    return(tmp)
}) %>%
    left_join(design.dat)
tmp <- (gen$data$SI %>%
     as.data.frame.table(responseName='obs'))

levels(sidat$Var1) <- levels(tmp$Var1)

ggplot(filter(sidat,err==0,age==1,wgts>0.5|exp.type!='rnd'),aes(as.numeric(Var2),pred,col=exp.type,lty=as.factor(wgts))) + geom_line() + facet_wrap(~Var1,scale='free_y') +  geom_line(data=gen$data$SI %>%
    as.data.frame.table(responseName='obs') %>%
    data.table(), aes(as.numeric(Var2),obs), col='black',size=2,lty=1)

ldistSI <- ldply(1:nrow(design.dat),function(x){
    if(class(res[[x]]) == 'try-error')
        tmp <- res2[[as.character(x)]]
    else
        tmp <- res[[as.character(x)]]
    tmp <- tmp$report$survArrLP
    dimnames(tmp) <- list(length=1:90,year=1:20,step=1:4)
    tmp <- as.data.frame.table(tmp,responseName='pred') %>%
        data.table()
    tmp$ID <- x
    return(tmp)
}) %>%
    left_join(design.dat)




stkdat <- llply(res,function(x){
    if(class(x) == 'try-error')
        return(data.table())
    dimnames(x$report$stkArr) <- list(age=1:10,length=1:90,year=1:20,step=1:4)
    as.data.frame.table(x$report$stkArr,responseName='pred') %>%
        mutate(age=as.numeric(age),
               length=as.numeric(length),
               year=as.numeric(year),
               step=as.numeric(step)) %>%
                   data.table()
})



big.res <- llply(res,tmp.func)


#res0 <- tmp.func(gen$parameters,'res0')


res1 <- tmp.func(R0,'res1')
res2 <- tmp.func(Rage,'res2')
res3 <- tmp.func(R.err,'res3')
res4 <- tmp.func(R.errage,'res4')
res5 <- tmp.func(R3,'res5')
res6 <- tmp.func(R2,'res6')
res7 <- tmp.func(R4,'res7')
res8 <- tmp.func(R5,'res8')
res.rec <- tmp.func(R.rec,'res.rec')
res.rec.err <- tmp.func(R.rec.err,'res.rec.err')
res.rec.age <- tmp.func(R.rec.age,'res.rec.age')
res.rec.err.age <- tmp.func(R.rec.err.age,'res.rec.err.age')





stkcomp <- simdat$stocks %>% 
    group_by(age,length,year,step) %>%
    summarise(obs=sum(num)) %>%
#    left_join(res0$stkdat) %>%
#    left_join(res1$stkdat) %>%
#    left_join(res2$stkdat) %>%
#    left_join(res3$stkdat) %>%
#    left_join(res4$stkdat) %>%
#    left_join(res5$stkdat) %>%
    left_join(res6$stkdat) %>%
#    left_join(res7$stkdat) %>%
#    left_join(res8$stkdat) %>%
    left_join(res.rec$stkdat) %>%
#    left_join(res.rec.err$stkdat) %>%
#    left_join(res.rec.age$stkdat) %>%
#    left_join(res.rec.err.age$stkdat) %>%
    gather(data.source,num,-c(age,length,year,step))

rec <- stkcomp %>%
    filter(age==1, step == 1) %>%
    group_by(year,data.source) %>%
    summarise(rec=sum(num))

dat <- stkcomp %>% 
    group_by(length,year,step,data.source) %>%
    summarise(n=sum(num)) %>%
    filter(step==1) %>%
#    filter(data.source %in% c('res0','obs'))%>%
 ggplot(aes(length,n,col=data.source))+ geom_line() + 
    facet_wrap(~year+step,scale='free_y')

png('sifig.png',width=800,height=800)
print(
gen$data$SI %>%
    as.data.frame.table(responseName='obs') %>%
    data.table() %>%
#    left_join(res0$sidat) %>%
#    left_join(res1$sidat) %>%
#    left_join(res2$sidat) %>%
#    left_join(res3$sidat) %>%
#    left_join(res4$sidat) %>%
#    left_join(res5$sidat) %>%
    left_join(res6$sidat) %>%
#    left_join(res7$sidat) %>%
#    left_join(res8$sidat) %>%
    left_join(res.rec$sidat) %>%
#    left_join(res.rec.err$sidat) %>%
#    left_join(res.rec.age$sidat) %>%
#    left_join(res.rec.err.age$sidat) %>%
    gather(data.source,num,-c(Var1,Var2)) %>%
    rename(lgroup=Var1,year=Var2) %>%
    mutate(year=as.numeric(year),
           Approach=ifelse(data.source=='obs','True value',
               ifelse(data.source=='res6','Random effects','Fixed effects'))) %>%
#    filter(data.source %in% c('obs','res6','res1')) %>%
    ggplot(aes(year,num/1e9,col=Approach)) + geom_line() +
    facet_wrap(~lgroup,scale='free_y') + theme_bw() +
           xlab('Year') + ylab('Index') 
)
dev.off()

dimnames(gen$data$ldistComm) <- list(length=6:90,year=1:20,step=1:4)
dimnames(gen$data$aldistComm) <- list(age=1:10,length=6:90,year=1:20,step=1:4)
dimnames(gen$data$ldistSI) <- list(length=6:90,year=1:20)
dimnames(gen$data$aldistSI) <- list(age=1:10,length=6:90,year=1:20)

as.data.frame.table(gen$data$ldistComm,responseName='obs') %>%
    mutate(length=as.numeric(length),
           year=as.numeric(year),
           step=as.numeric(step)) %>%
    data.table() %>%
#    left_join(res0$ldistComm) %>%
    left_join(res1$ldistComm) %>%
    left_join(res2$ldistComm) %>%
    left_join(res3$ldistComm) %>%
    left_join(res4$ldistComm) %>%
    left_join(res5$ldistComm) %>%
    left_join(res6$ldistComm) %>%
    left_join(res7$ldistComm) %>%
    left_join(res8$ldistComm) %>%
    left_join(res.rec$ldistComm) %>%
#    left_join(res.rec.err$ldistComm) %>%
    gather(data.source,num,-c(length,year,step)) %>%
    filter(step==1) %>%
    mutate(length = length -ifelse(data.source!='obs',5,0)) %>%
    ggplot(aes(length,num,col=data.source))+ geom_line() + 
    facet_wrap(~year+step,scale='free_y')



as.data.frame.table(gen$data$ldistSI,responseName='obs') %>%
    mutate(length=as.numeric(length),
           year=as.numeric(year)) %>%
    data.table() %>%
    left_join(res1$ldistSI) %>%
    left_join(res2$ldistSI) %>%
    left_join(res3$ldistSI) %>%
    left_join(res4$ldistSI) %>%
    left_join(res5$ldistSI) %>%
    left_join(res6$ldistSI) %>%
    left_join(res7$ldistSI) %>%
    left_join(res8$ldistSI) %>%
    left_join(res.rec$ldistSI) %>%
#    left_join(res.rec.err$ldistSI) %>%
    gather(data.source,num,obs:res.rec) %>%
    mutate(length = length -ifelse(data.source!='obs',5,0)) %>%
    ggplot(aes(length,num,col=data.source))+ geom_line() + 
    facet_wrap(~year+step,scale='free_y')

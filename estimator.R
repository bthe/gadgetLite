mGL <- function(dat,par,bounds=FALSE,random=NULL,mapGL){
    obj <- MakeADFun(dat,par,random="recruits",
                     map=mapGL,
                     DLL="gadgetLite")
    if(bounds){
        tmp <- obj$par
        upper <-  tmp+abs(0.9*tmp)
        lower <- tmp-abs(0.9*tmp)
        upper['log_sigma'] <- log(0.5)
        lower['log_sigma'] <- log(0.05)
    } else {
        upper <- Inf
        lower <- -Inf
    }

    opt <- nlminb(obj$par,obj$fn,obj$gr,
                  upper=upper,
                  lower=lower)
    pl <- obj$env$parList(opt$par)
    report <- obj$report()
    return(list(obj=obj,opt=opt,pl=pl,report=report))
}

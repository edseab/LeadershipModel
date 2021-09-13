find_vol_equi <- function(x,func,search.all=T,returns=F,...){
  args <- as.list(unlist(x))
  names(args) <- colnames(x)
  args <- args[which(names(args)%in%names(formals(func)))]
  args <- lapply(args,as.numeric)
  
  extra_Args <- as.list(sys.call())
  extra_Args <- extra_Args[!names(extra_Args)==""]
  
  args[names(args) %in% names(extra_Args)] <- NULL
  args[names(extra_Args)[names(extra_Args) %in% names(as.list(args(func)))]] <- extra_Args[names(extra_Args) %in% names(as.list(args(func)))]
  
  if(search.all){
  vol_eq <- do.call(rootSolve::uniroot.all,args=c(f=func,interval=list(c(0,1)),args))
  }else{
  vol_eq <- tryCatch({do.call(rootSolve::uniroot,args=c(f=func,interval=list(c(0,1)),args))$root},error=function(.)return(c()))
  }
  
  if (!0 %in% vol_eq & do.call(func,args=c(0,args))<0) vol_eq <- c(0,vol_eq)
  if (!1 %in% vol_eq & do.call(func,args=c(1,args))>0) vol_eq <- c(vol_eq,1)
  
  if(returns){
  args <- c(args,list(dataheavy=T))
  res <- sapply(vol_eq,function(x)do.call(func,args=c(x,args))$AvgRet)
  vol_eq <- list(vol_eq=vol_eq,returns=res)
  }
  
  return(vol_eq)
  }

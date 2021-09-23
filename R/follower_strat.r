follower_strat <- function(leadership=c("homogeneous","heterogeneous"),opt.I = T,increment=100,...){

  leadership <- match.arg(leadership)
  args <- as.list(sys.call())
  if(any(c("E") %in% names(args)))warning("Extraction should not be in the dataframe and will be ignored")
  args <- args[!names(args) %in% c("","leadership","increment","E")]
  
  if(opt.I & "inv" %in% names(args)) {
             args <- args[names(args)!="inv"]
			 warning("While opt.I=T, investment should not be in the dataframe and will be ignored"
   }
  db <- do.call(expand.grid,args)
 
   if(leadership=="homogeneous"){
   
for (i in 1:nrow(db)){
values <- db[i,colnames(fulldb) %in% names(formals(vol_ben))]
if(opt.I){ 
E_optim <- function(E){
 return(do.call(ldr_output,args=c(as.list(values),list(E=E,leadership=leadership,progress=F)))$RetNoVol)
}
}else{
fulldb$expected_vol[i] <- find_vol_equi(values,vol_ben,select.max=T)
E_optim <- function(E){do.call(vol_ben,args=c(as.list(values),list(pvol=fulldb$expected_vol[i],dataheavy=F, save.RetNoVol=T)))$RetNoVolLoL
}

# Calculate optimal E_bar
retnovols <- optimize(E_optim,c(0,30),maximum=T)
 options <- sapply(c(0,retnovols$maximum,30),E_optim)
best_payment <-as.numeric(unlist(c(0,retnovols,30)[which.max(options)]))

if(opt.I){ 
results <- do.call(ldr_output,args=c(as.list(values),list(E=best_payment,leadership=leadership,progress=F)))
}else{
results <-do.call(vol_ben,args=c(as.list(values),list(E=best_payment,pvol=fulldb$expected_vol[i],dataheavy=T)))
}

db$expected_vol[i] <- fulldb$expected_vol[i]
db$Leader_payment[i] <- best_payment
if(opt.I) db$Leader_inv[i] <- results$expected_inv
db$Ldr_Return[i] <- results$Ldr_Return
db$Flwr_return[i] <- results$Flwr_return
db$No_Ldr_Return[i] <- results$No_Ldr_Return


progress(i,nrow(db),increment=increment)
}
}
return(db)
}



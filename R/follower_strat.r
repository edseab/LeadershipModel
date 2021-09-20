follower_strat <- function(leadership=c("homogeneous","heterogeneous"),increment=100,...){

  leadership <- match.arg(leadership)
  args <- as.list(sys.call())
  if(any(c("E","inv") %in% names(args)))warning("Extraction and Investment should not be variables in the dataframe and will be ignored")
  args <- args[!names(args) %in% c("","leadership","increment","inv","E")]
 
  db <- do.call(expand.grid,args)
 
   if(leadership=="homogeneous"){
   
for (i in 1:nrow(db)){
  
E_optim <- function(E){
 return(do.call(ldr_output,args=c(as.list(db[i,]),list(E=E,leadership=leadership,progress=F)))$Follower_return)
}
flwr_rets <- optimize(E_optim,c(0,30),maximum=T)
 options <- sapply(c(0,flwr_rets$maximum,30),E_optim)
best_payment <-as.numeric(unlist(c(0,flwr_rets,30)[which.max(options)]))

results <- do.call(ldr_output,args=c(as.list(db[i,]),list(E=best_payment,leadership=leadership,progress=F)))

db$expected_vol[i] <- results$expected_vol
db$Leader_payment[i] <- best_payment
db$Leader_inv[i] <- results$expected_inv
db$Leader_return[i] <- results$Leader_return
db$Follower_return[i] <- results$Follower_return
db$No_Ldr_Return[i] <- results$No_Ldr_Return


progress(i,nrow(db),increment=increment)
}
}
return(db)
}



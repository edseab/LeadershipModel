follower_strat <- function(leadership=c("homogeneous","heterogeneous"),increment=100,...){

  leadership <- match.arg(leadership)
  args <- as.list(sys.call())
  if(any(c("E","inv") %in% names(args)))warning("Extraction and Investment should not be variables in the dataframe and will be ignored")
  args <- args[!names(args) %in% c("","leadership","increment","inv","E")]
 
  fulldb <- do.call(expand.grid,args)
 
   if(leadership=="homogeneous"){
for (i in 1:nrow(fulldb)){
  
E_optim <- function(E) do.call(ldr_output,args=c(as.list(fulldb[i,]),list(E=E,leadership=leadership,progress=F)))$Follower_return

flwr_rets <- sapply(seq(0,30,0.1),E_optim)

best_payment <- seq(0,30,0.01)[which.max(flwr_rets)]

results <- do.call(ldr_output,args=c(as.list(fulldb[i,]),list(E=best_payment,leadership=leadership,progress=F)))

fulldb$Nvols[i] <- results$Nvols
fulldb$Leader_payment[i] <- best_payment
fulldb$Leader_inv[i] <- results$Leader_inv
fulldb$Leader_return[i] <- results$Leader_return
fulldb$Follower_return[i] <- results$Follower_return
fulldb$No_Ldr_Return[i] <- results$No_Ldr_Return

progress(i,nrow(fulldb),increment=increment)
}
}
return(fulldb)
}



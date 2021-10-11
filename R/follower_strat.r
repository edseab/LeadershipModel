follower_strat <- function(leadership=c("homogeneous","heterogeneous"),max.iter=10,opt.I = T,increment=100,...){

  leadership <- match.arg(leadership)
  args <- as.list(sys.call())
  if(any(c("E") %in% names(args)))warning("Extraction should not be in the dataframe and will be ignored")
  args <- args[!names(args) %in% c("","leadership","increment","E","opt.I")]
  
  if(opt.I & "inv" %in% names(args)) {
             args <- args[names(args)!="inv"]
			 warning("While opt.I=T, investment should not be in the dataframe and will be ignored")
   }
  db <- do.call(expand.grid,args)
 
   if(leadership=="homogeneous"){
   
for (i in 1:nrow(db)){
values <- db[i,colnames(db) %in% names(formals(vol_ben))]
if(opt.I){ 
E_optim <- function(E){
 return(do.call(ldr_output,args=c(as.list(values),list(E=E,leadership=leadership,progress=F)))$RetNoVol)
}
}else{
E_optim <- function(E){
equis <- find_vol_equi(c(values,E=E),vol_ben,select.max=T)
out <- do.call(vol_ben,args=c(as.list(values),list(E=E,pvol=equis,dataheavy=T)))
return(out$RetNoVol)
}
}

# Calculate optimal E_bar
#first find E for which follower return is as low as no leader
max_E <- ifelse(opt.I,values$L*values$grpsz*8,values$L*values$inv*values$grpsz)
nonldrs <- optimize(E_optim,c(0,max_E),maximum=T)
count <- 0
while(max_E-nonldrs$maximum<0.01 & count<=max.iter){
max_E <- 0.9*max_E
nonldrs <- optimize(E_optim,c(0,max_E),maximum=T)
count<-count+1
}
options <- sapply(c(0,nonldrs$maximum,max_E),E_optim)
best_payment <-as.numeric(unlist(c(0,nonldrs,30)[which.max(options)]))

if(opt.I){ 
results <- do.call(ldr_output,args=c(as.list(values),list(E=best_payment,leadership=leadership,progress=F)))
db$expected_vol[i] <- results$expected_vol
db$expected_vol_check[i] <- find_vol_equi(c(values,E=best_payment),vol_ben,select.max=T)
}else{
db$expected_vol[i] <- find_vol_equi(c(values,E=best_payment),vol_ben,select.max=T)
results <-do.call(vol_ben,args=c(as.list(values),list(E=best_payment,pvol=db$expected_vol[i],dataheavy=T)))
}

db$Leader_payment[i] <- best_payment
if(opt.I) db$Leader_inv[i] <- results$expected_inv
db$RetNoVol[i] <- results$RetNoVol
db$Ldr_return[i] <- results$Ldr_return
db$Flwr_return[i] <- results$Flwr_return
db$Acephalous_return[i] <- results$Acephalous_return


progress(i,nrow(db),increment=increment)
}
}
return(db)
}

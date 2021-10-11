ldr_output <- function(leadership=c("homogeneous","heterogeneous"),baseline=5,progress=T,increment=100,...){
  leadership <- match.arg(leadership)
  args <- as.list(sys.call())
  if("inv" %in% names(args))warning("Investment should not be a variable in the dataframe and will be ignored")
  args <- args[!names(args) %in% c("","leadership","increment","inv","progress")]
 
  fulldb <- do.call(expand.grid,args)
 
   if(leadership=="homogeneous"){
for (i in 1:nrow(fulldb)){

ldr_reward_fnc <- function(inv){
  find_vol_equi(c(fulldb[i,],inv=inv),vol_ben,select.max=T)
## Another version with relatedness to greoup???
  }
  if(is.null(fulldb$LL))LL<- 0
  inv_equis <- sapply(seq(0,1,0.01),ldr_reward_fnc)
fulldb$expected_inv[i] <- ifelse(sum(inv_equis)==0,0,sum(inv_equis*seq(0,1,0.01))/sum(inv_equis))
fulldb$expected_vol[i] <- sum(inv_equis)/101
values <- fulldb[i,colnames(fulldb) %in% names(formals(vol_ben))]
fulldb$Ldr_return[i] <- with(fulldb[i,],baseline + P*(1-expected_inv) + LL+(L-LL)*expected_inv + E*(Ecoef-1/grpsz) - Lcost - volcost)
fulldb$Flwr_return[i] <- with(fulldb[i,],baseline +P + LL+(L-LL)*expected_inv - E/grpsz - expected_vol*volcost)
fulldb$Acephalous_return[i] <- with(fulldb[i,],baseline + P + LL)
fulldb$RetNoVol[i] <- with(fulldb[i,],(baseline + P + LL)*((1-expected_vol)^(grpsz-1)) + (1-((1-expected_vol)^(grpsz-1)))*Flwr_return)
 if(progress) progress(i,nrow(fulldb),increment=increment)
 }
 return(fulldb)
 }
 }

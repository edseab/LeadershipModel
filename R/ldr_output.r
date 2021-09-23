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
  if(is.na(fulldb$LL))LL<- 0
  inv_equis <- sapply(seq(0,1,0.01),ldr_reward_fnc)
fulldb$expected_inv[i] <- ifelse(sum(inv_equis)==0,0,sum(inv_equis*seq(0,1,0.01))/sum(inv_equis))
fulldb$expected_vol[i] <- sum(inv_equis)/101
values <- fulldb[i,colnames(fulldb) %in% names(formals(vol_ben))]
fulldb$RetNoVol[i] <- do.call(vol_ben,args=c(as.list(values),list(inv=fulldb$expected_inv[i],pvol=fulldb$expected_vol[i],dataheavy=F, save.RetNoVol=T)))
fulldb$Ldr_return[i] <- with(fulldb,baseline + P*(1-inv) + LL+(L-LL)*inv + E*(Ecoef-1/grpsz) - Lcost - volcost
fulldb$Flwr_return[i] <- with(fulldb,baseline +P + LL+(L-LL)*inv - E/grpsz - expected_vol*volcost)
fulldb$No_Ldr_Return[i] <- with(fulldb,baseline +P + LL)
 if(progress) progress(i,nrow(fulldb),increment=increment)
 }
 return(fulldb)
 }
 }

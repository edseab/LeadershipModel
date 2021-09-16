ldr_output <- function(leadership=c("homogeneous","heterogeneous"),progress=T,increment=100,...){
  leadership <- match.arg(leadership)
  args <- as.list(sys.call())
  if("inv" %in% names(args))warning("Investment should not be a variable in the dataframe and will be ignored")
  args <- args[!names(args) %in% c("","leadership","increment","inv")]
 
  fulldb <- do.call(expand.grid,args)
 
   if(leadership=="homogeneous"){
for (i in 1:nrow(fulldb)){

ldr_reward_fnc <- function(inv){
  with(fulldb[i,],((LL + (L-LL)*inv)*grpsz - E)/grpsz  + Baseline + P*(1-inv)-volcost-Lcost+Ecoef*E)

## Another version with relatedness to greoup???
  }
  
highest_ldr_return <- seq(0,1,0.01)[which.max(ldr_reward_fnc(seq(0,1,0.01)))]
highest_ldr_return_value <- ldr_reward_fnc(highest_ldr_return)


elec_probs <- 1/(1+0:(fulldb$grpsz[i]-1)) 
 RetVol <- elec_probs*highest_ldr_return_value + 
        with(fulldb[i,],(1-elec_probs)*(((LL + (L-LL)*highest_ldr_return)*grpsz - E)/grpsz  + Baseline + P - volcost))
 RetNoVol <- with(fulldb[i,],c(LL + Baseline + P, rep(((LL + (L-LL)*highest_ldr_return)*grpsz - E)/grpsz  + Baseline + P,grpsz-1)))
 
 
 max_vols <- max(RetVol-RetNoVol)
 
 
 if(max_vols < 0){
 fulldb$Nvols[i] <- 0
 fulldb$Leader_inv[i] <- NA
 }else{
 fulldb$Nvols[i] <- (1:fulldb$grpsz[i])[which.max(RetVol-RetNoVol)]
 fulldb$Leader_inv[i] <- highest_ldr_return
 }
 
 fulldb$Leader_return[i] <- highest_ldr_return_value
 fulldb$Follower_return[i] <- with(fulldb[i,],((LL + (L-LL)*highest_ldr_return)*grpsz - E)/grpsz  + Baseline + P)
 fulldb$No_Ldr_Return[i] <- with(fulldb[i,],LL + Baseline + P)
 
 if(progress) progress(i,nrow(fulldb),increment=increment)
 }
 return(fulldb)
 }
 }
 
 









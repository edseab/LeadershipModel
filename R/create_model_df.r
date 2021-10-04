create_model_df <- function(leadership=c("homogeneous","heterogeneous"),increment=100,added = NA,...){
  leadership <- match.arg(leadership)
  args <- as.list(sys.call())
  args <- args[!names(args) %in% c("","leadership","increment","added")]
 
  fulldb <- do.call(expand.grid,args)
 
 if(!is.na(added)){
 for(i in seq_along(added)){
 fulldb[,names(added)[i]] <- eval(parse(text=paste0('with(fulldb,',added[i],')')))
 }}
 
  if(leadership=="homogeneous"){
    fulldb$other_equis <- fulldb$equi_volunteering <- NA
	for(i in 1:nrow(fulldb)){
    options <- find_vol_equi(fulldb[i,],vol_ben,returns=T)
    best_option <- which(options$returns-max(options$returns)<10e-15)[1] # this is because sometimes R will say 2 values are different even when they are not, and that the diff between them is -8.81*e-16
   
    fulldb$equi_volunteering[i] <- options$vol_eq[best_option]
   
    res <- with(fulldb[i,],vol_ben(as.numeric(equi_volunteering),grpsz=grpsz,L=L,LL=LL,P=P,inv=inv,Lcost=Lcost,E=E,Ecoef=Ecoef,volcost=volcost,N=N,dataheavy=T))
   
    fulldb$effective_E[i] <- res$LoL_effective_E
   
    if(length(options)>1) fulldb$other_equis[i] <- paste(options$vol_eq[-best_option],collapse=";")
   
    fulldb$Ldr_return[i] <- res$Ldr_return
    fulldb$Flwr_return[i] <- res$Flwr_return
    fulldb$NoLeader_return[i] <- res$Acephalous_return
   
    progress(i, nrow(fulldb), increment=increment)
 }

#volunteering and other outcomes
fulldb$volunteering <- cut(fulldb$equi_volunteering, breaks=c(-1,0,0.333,0.667,1,2), labels=c("none","rare","common","most","all"),right=T)
fulldb$Leadered_grp_return <- fulldb$Ldr_return+fulldb$Flwr_return*(fulldb$grpsz-1)
fulldb$NoLeader_grp_return <- fulldb$NoLeader_return*fulldb$grpsz

fulldb$efficient <- as.numeric(as.numeric(fulldb$Leadered_grp_return)>as.numeric(fulldb$NoLeader_grp_return))

fulldb$LeaderFollowerRatio <- fulldb$Ldr_return/fulldb$Flwr_return
fulldb$LdrToFollowerOutcome <- "Equal"
fulldb$LdrToFollowerOutcome[which(fulldb$LeaderFollowerRatio>1)] <- "Beneficial"
fulldb$LdrToFollowerOutcome[which(fulldb$LeaderFollowerRatio<1)] <- "Costly"


# Class variable
fulldb$class <- NA
fulldb$class[which(fulldb$LdrToFollowerOutcome=="Beneficial" & fulldb$Flwr_return<fulldb$NoLeader_return & fulldb$volunteering!="none")] <- "Leader exploitation"
fulldb$class[which(fulldb$efficient==0 & fulldb$volunteering=="none")] <- "No leadership - costly to all"
fulldb$class[which(fulldb$efficient==0 & fulldb$volunteering=="rare" & fulldb$LdrToFollowerOutcome=="Equal")] <- "Low useless leadership"
fulldb$class[which(fulldb$efficient==1 & fulldb$volunteering!="none" & fulldb$LdrToFollowerOutcome=="Equal")] <- "Equal leadership"
fulldb$class[which(fulldb$efficient==1 & fulldb$volunteering=="none")] <- "No leadership - costly to leader"
fulldb$class[which(fulldb$efficient==1 & fulldb$volunteering!="none"& fulldb$LdrToFollowerOutcome=="Costly")] <- "w(Leader) < w(Follower)"
fulldb$class[which(fulldb$efficient==1 & fulldb$volunteering!="none"& fulldb$LdrToFollowerOutcome=="Beneficial")] <- "w(Leader) > w(Follower)"

}

  if(leadership=="heterogeneous"){
  fulldb2 <- fulldb #inefficient for now but dont want to mix up names
fulldb2$HiL <- fulldb2$L*fulldb2$HiLmulti
fulldb2$PHiL <- fulldb2$P*fulldb2$HiLPmulti

fulldb2$HiLvol <- fulldb2$LoLvol <-NA

  
  Baseline <- 5 # change if needed
  for(i in 1:nrow(fulldb2)){
  LoLvol <- 1e-8
  HiLvol <- 0
  cycle <- 1
  while(HiLvol!=1 & LoLvol!=0 & cycle <20){
    HiLvol <- find_vol_equi(fulldb2[i,],vol_ben,pvol=LoLvol,Lgroup="HiL",select.best=T,search.all=F)
    if(length(HiLvol)>1) HiLvol <- HiLvol[which.max(find_vol_equi(fulldb2[i,],vol_ben,pvol=LoLvol,select.best=T,returns=T,search.all=F)$returns)]
    LoLvol <- find_vol_equi(fulldb2[i,],vol_ben,pvolHiL=HiLvol,select.best=T,search.all=F,Lgroup="LoL")
    if(length(LoLvol)>1) LoLvol <- LoLvol[which.max(find_vol_equi(fulldb2[i,],vol_ben,pvolHiL=HiLvol,select.best=T,returns=T,search.all=F)$returns)]
    cycle <- cycle+1
  }
  
  # 1 last cycle for good luck
  HiLvol <- find_vol_equi(fulldb2[i,],vol_ben,pvol=LoLvol,select.best=T,search.all=F,Lgroup="HiL")
  if(length(HiLvol)>1) HiLvol <- HiLvol[which.max(find_vol_equi(fulldb2[i,],vol_ben,pvol=LoLvol,select.best=T,returns=T,search.all=F)$returns)]
  LoLvol <- find_vol_equi(fulldb2[i,],vol_ben,pvolHiL=HiLvol,select.best=T,search.all=F,Lgroup="LoL")
  if(length(LoLvol)>1) LoLvol <- LoLvol[which.max(find_vol_equi(fulldb2[i,],vol_ben,pvolHiL=HiLvol,select.best=T,returns=T,search.all=F)$returns)]
  
  fulldb2$HiLvol[i] <- HiLvol
  fulldb2$LoLvol[i] <- LoLvol
  
  
  # Now some variables
  
  res <- with(fulldb2[i,],vol_ben(pvol=as.numeric(LoLvol),pvolHiL=as.numeric(HiLvol),grpsz=grpsz,L=L,LL=LL,P=P,HiL=HiL,PHiL=PHiL,HiLprop=HiLprop, inv=inv,Lcost=Lcost,E=E,Ecoef=Ecoef,volcost=volcost,N=N,select.best=T,dataheavy=T))
  
  fulldb2$HiL_effective_E[i] <- res$HiL_effective_E
  fulldb2$LoL_effective_E[i] <- res$LoL_effective_E
  
  fulldb2$Ldr_return[i] <- res$Ldr_return
  fulldb2$Flwr_return[i] <- res$Flwr_return
  fulldb2$NoLdr_returnHiL[i] <- res$NoLdr_returnHiL
  fulldb2$NoLdr_returnLoL[i] <- res$NoLdr_returnHiL

  fulldb2$Rotating_Leader_returnHiL[i] <- res$Rotating_Leader_returnHiL
  fulldb2$Rotating_Leader_returnLoL[i] <- res$Rotating_Leader_returnLoL
  
  progress(i,nrow(fulldb2), increment=10000)
}

fulldb2$equi_type <- NA
fulldb2$equi_type[which(fulldb2$HiLvol==1)] <- "All_Hi"
fulldb2$equi_type[which(fulldb2$LoLvol==0)] <- "No_Lo"
fulldb2$equi_type[which(fulldb2$HiLvol==1 & fulldb2$LoLvol==1)] <- "Everyone"
fulldb2$equi_type[which(fulldb2$HiLvol==0 & fulldb2$LoLvol==0)] <- "No-one"
fulldb2$equi_type[which(fulldb2$HiLvol<1 & fulldb2$LoLvol>0)] <- "multiple_equi"
fulldb2$equi_type[which((fulldb2$LoLvol==1 & fulldb2$HiLvol<1)|(fulldb2$HiLvol==0 & fulldb2$LoLvol>0))] <-"Inverted"

fulldb2$equi_type[is.na(fulldb2$equi_type)] <- "Unaccounted"

fulldb2$Leadered_grp_return <- fulldb2$Ldr_return+fulldb2$Flwr_return*(fulldb2$grpsz-1)
fulldb2$NoLdr_grp_return <- (fulldb2$NoLdr_returnHiL*fulldb2$HiLprop + fulldb2$NoLdr_returnLoL*(1-fulldb2$HiLprop))*fulldb2$grpsz
fulldb2$efficient <- as.numeric(fulldb2$Leadered_grp_return>fulldb2$NoLdr_grp_return)
fulldb2$LdrToFollowerOutcome <- "Equal"
fulldb2$LdrToFollowerOutcome[which(fulldb2$Ldr_return>fulldb2$Flwr_return)] <- "Beneficial"
fulldb2$LdrToFollowerOutcome[which(fulldb2$Ldr_return<fulldb2$Flwr_return)] <- "Costly"

  }
  
ifelse(leadership=="heterogeneous",return(fulldb2),return(fulldb))
}
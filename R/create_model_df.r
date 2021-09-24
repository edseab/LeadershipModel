create_model_df <- function(leadership=c("homogeneous","heterogeneous"),increment=100,added = NA,...){
  leadership <- match.arg(leadership)
  args <- as.list(sys.call())
  args <- args[!names(args) %in% c("","leadership","increment","added","save.RetNoVol")]
 
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
    fulldb$NoLeader_return[i] <- res$NoLdr_return
   
    progress(i, nrow(fulldb), increment=increment)
 }

#volunteering and other outcomes
fulldb$volunteering <- "none"
fulldb$volunteering[fulldb$equi_volunteering>0 & fulldb$equi_volunteering<=0.333] <- "rare"
fulldb$volunteering[fulldb$equi_volunteering>0.333 & fulldb$equi_volunteering<=0.666] <- "common"
fulldb$volunteering[fulldb$equi_volunteering>0.666 & fulldb$equi_volunteering<1] <- "most"
fulldb$volunteering[fulldb$equi_volunteering==1] <- "All"

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
return(fulldb)
}
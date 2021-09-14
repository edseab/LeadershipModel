
# devtools::install_github("edseab/LeadershipModel")
library(LeadershipModel)
library(rootSolve)
library(RColorBrewer)

# Vary returns to leadership
plot(seq(0,1,0.1),modl(seq(0,1,0.1),0,L=2,Lavg=2,E=0,Eavg=0),
      type = "l", ylim=c(0.4,2),
	  ylab="Return ratio relative to average",
	  xlab="Probability of volunteering",
	  main=c("Vavg=0%    P=Pavg=1    E=Eavg=0"))
	  text(0.45,1,labels="L = 2",srt=-2)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0,L=3,Lavg=3,E=0,Eavg=0))
	  text(0.4,1.13,labels="L = 3",srt=10)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0,L=1.2,Lavg=1.2,E=0,Eavg=0))
	  text(0.42,0.89,labels="L = 1",srt=-17)

# Vary average probability of volunteering
plot(seq(0,1,0.1),modl(seq(0,1,0.1),0.1,L=7,Lavg=7,E=0,Eavg=0),
      type = "l", ylim=c(0.4,2),
	  ylab="Return ratio relative to average",
	  xlab="Probability of volunteering",
	  main=c("L=Lavg=7    P=Pavg=1    E=Eavg=0"))
	  text(0.55,1.27,labels="Vavg = 10%",srt=14)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=7,Lavg=7,E=0,Eavg=0))
	  text(0.6,1.11,labels="Vavg = 20%",srt=6)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.5,L=7,Lavg=7,E=0,Eavg=0))
	  text(0.65,1.03,labels="Vavg = 50%",srt=-3)

# Vary average leadership of group
plot(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=7,Lavg=7,E=0,Eavg=0),
      type = "l", ylim=c(0.4,2),
	  ylab="Return ratio relative to average",
	  xlab="Probability of volunteering",
	  main=c("L=7    P=Pavg=1    E=Eavg=0    Vavg=0.2"))
	  text(0.54,1.12,labels="Lavg = 5",srt=6)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=7,Lavg=3,E=0,Eavg=0))
	  text(0.50,1.33,labels="Lavg = 3",srt=17)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=7,Lavg=15,E=0,Eavg=0))
	  text(0.50,0.93,labels="Lavg = 15",srt=-3)

# Vary productivity
plot(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,E=0,Eavg=0,P=1.4,Pavg=1),
      type = "l", ylim=c(0.4,2),
	  ylab="Return ratio relative to average",
	  xlab="Probability of volunteering",
	  main=c("L=5    Pavg=1    E=Eavg=0    Vavg=0.2"))  	  
	  text(0.55,1.17,labels="P = 1.4",srt=2)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,P=0.8,Pavg=1,E=0,Eavg=0))
	  text(0.50,1.03,labels="P = 0.8",srt=3)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,P=3,Pavg=1,E=0,Eavg=0))
	  text(0.50,1.57,labels="P = 3",srt=-4)

# Vary extraction
plot(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,E=0,Eavg=0,P=1,Pavg=1),
      type = "l", ylim=c(0.4,2),
	  ylab="Return ratio relative to average",
	  xlab="Probability of volunteering",
	  main=c("L=LAvg=5    P=Pavg=1    Eavg=0    Vavg=0.2"))  	  
	  text(0.55,1.09,labels="E = 0",srt=2)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,P=1,Pavg=1,E=2,Eavg=0))
	  text(0.50,1.23,labels="E = 2",srt=9)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,P=1,Pavg=1,E=4,Eavg=0))
	  text(0.50,1.37,labels="E = 4",srt=14)

# Vary average extraction
plot(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,E=1,Eavg=0,P=3,Pavg=3),
      type = "l", ylim=c(0.4,2),
	  ylab="Return ratio relative to average",
	  xlab="Probability of volunteering",
	  main=c("L=Lavg=5    P=Pavg=3    E=1    Vavg=0.2"))  	  
	  text(0.50,1.06,labels="Eavg = 0",srt=0)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,P=3,Pavg=3,E=1,Eavg=6))
	  text(0.50,0.97,labels="Eavg = 6",srt=1)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,P=3,Pavg=3,E=1,Eavg=12))
	  text(0.50,0.88,labels="Eavg = 12",srt=4)

## Now benefits to extraction after being elected ##

# No extraction
plot(seq(0,1,0.1),abdl(seq(0,1,0.1),L=5,Lavg=5,E=0,Eavg=0, P=1,Pavg=1),
      lty=1,
      type = "l",ylim=c(-1,3),
	  ylab = "Benefit if leader stays",
	  xlab = "Average probability of volunteering",
	  main = "L=Lavg=5    P=Pavg=1    E=Eavg=0"
)
lines(seq(0,1,0.1),overtl(seq(0,1,0.1),L=5,Lavg=5,E=0,Eavg=0, P=1,Pavg=1),
      lty=2)
legend(0.4,2.5,lty=1:2,legend=c("Benefit to leader","Benefit to group"))
abline(h=0, col='darkred')

# Some extraction
plot(seq(0,1,0.1),abdl(seq(0,1,0.1),L=5,Lavg=5,E=1,Eavg=1, P=1,Pavg=1),
      lty=1,
      type = "l",ylim=c(-1,3),
	  ylab = "Benefit if leader stays",
	  xlab = "Average probability of volunteering (Vavg)",
	  main = "L=Lavg=5    P=Pavg=1    E=Eavg=1"
)
lines(seq(0,1,0.1),overtl(seq(0,1,0.1),L=5,Lavg=5,E=1,Eavg=1, P=1,Pavg=1),
      lty=2)
legend(0.4,2.5,lty=1:2,legend=c("Benefit to leader","Benefit to group"))
abline(h=0, col='darkred')


# More extraction than average
plot(seq(0,1,0.1),abdl(seq(0,1,0.1),L=5,Lavg=5,E=2,Eavg=1, P=1,Pavg=1),
      lty=1,
      type = "l",ylim=c(-1,3),
	  ylab = "Benefit if leader stays",
	  xlab = "Average probability of volunteering (Vavg)",
	  main = "L=Lavg=5    P=Pavg=1    E=2,Eavg=1"
)
lines(seq(0,1,0.1),overtl(seq(0,1,0.1),L=5,Lavg=5,E=2,Eavg=1, P=1,Pavg=1),
      lty=2)
legend(0.4,2.5,lty=1:2,legend=c("Benefit to leader","Benefit to group"))
abline(h=0, col='darkred')

# See how much extraction a leader can get away with
plot(seq(0,5,0.4),abdl(0.5,L=5,Lavg=5,E=seq(0,5,0.4),Eavg=1, P=1,Pavg=1),
      lty=1,
      type = "l",ylim=c(-2,3),
	  ylab = "Benefit if leader stays",
	  xlab = "Leader extraction E",
	  main = "Vavg=0.5   L=Lavg=5    P=Pavg=1     Eavg=1"
)
lines(seq(0,5,0.4),overtl(0.5,L=5,Lavg=5,E=seq(0,5,0.4),Eavg=1, P=1,Pavg=1),
      lty=2)
legend(0.4,2.5,lty=1:2,legend=c("Benefit to leader","Benefit to group"))
abline(h=0, col='darkred')

# Higher production than average
plot(seq(0,5,0.4),abdl(0.5,L=5,Lavg=5,E=seq(0,5,0.4),Eavg=1, P=3,Pavg=1),
      lty=1,
      type = "l",ylim=c(-2,3),
	  ylab = "Benefit if leader stays",
	  xlab = "Leader extraction E",
	  main = "Vavg=0.5   L=Lavg=5    P=3   Pavg=1     Eavg=1"
)
lines(seq(0,5,0.4),overtl(0.5,L=5,Lavg=5,E=seq(0,5,0.4),Eavg=1, P=3,Pavg=1),
      lty=2)
legend(0.4,2.5,lty=1:2,legend=c("Benefit to leader","Benefit to group"))
abline(h=0, col='darkred')

# Higher Leadership than average
plot(seq(0,10,0.4),abdl(0.5,L=6,Lavg=5,E=seq(0,10,0.4),Eavg=1, P=1,Pavg=1),
      lty=1,
      type = "l",ylim=c(-2,8),
	  ylab = "Benefit if leader stays",
	  xlab = "Leader extraction E",
	  main = "Vavg=0.5   L=6   Lavg=5    Pavg=1     Eavg=1"
)
lines(seq(0,10,0.4),overtl(0.5,L=6,Lavg=5,E=seq(0,10,0.4),Eavg=1, P=1,Pavg=1),
      lty=2)
legend(0.4,4.5,lty=1:2,legend=c("Benefit to leader","Benefit to group"))
abline(h=0, col='darkred')

# Higher Leadership and production
plot(seq(0,10,0.4),abdl(0.5,L=6,Lavg=5,E=seq(0,10,0.4),Eavg=1, P=3,Pavg=1),
      lty=1,
      type = "l",ylim=c(-2,8),
	  ylab = "Benefit if leader stays",
	  xlab = "Leader extraction",
	  main = "Vavg=0.5   L=6   Lavg=5    P=3   Pavg=1     Eavg=1"
)
lines(seq(0,10,0.4),overtl(0.5,L=6,Lavg=5,E=seq(0,10,0.4),Eavg=1, P=3,Pavg=1),
      lty=2)
legend(0.4,4.5,lty=1:2,legend=c("Benefit to leader","Benefit to group"))
abline(h=0, col='darkred')

# Higher Leadership and production
plot(seq(0,10,0.4),abdl(0.9,L=6,Lavg=5,E=seq(0,10,0.4),Eavg=1, P=3,Pavg=1),
      lty=1,
      type = "l",ylim=c(-2,8),
	  ylab = "Benefit if leader stays",
	  xlab = "Leader extraction",
	  main = "Vavg=0.9   L=6   Lavg=5    P=3   Pavg=1     Eavg=1"
)
lines(seq(0,10,0.4),overtl(0.9,L=6,Lavg=5,E=seq(0,10,0.4),Eavg=1, P=3,Pavg=1),
      lty=2)
legend(0.4,4.5,lty=1:2,legend=c("Benefit to leader","Benefit to group"))
abline(h=0, col='darkred')

# PARAMETER SPACE TO EXPLORE
Espace <- c(0,2,5,10,20)
Pspace <- c(0,1,3,7,15)
Lspace <- c(1,3,7,15)
invspace  <- LLspace <-Ecoefspace <- c(0.1,0.4,0.9,1)
Lcostspace <- volcostspace <- c(0,0.1,0.5,2)
grpszspace <- c(5,10)
Nspace <- c(50,100,200)

fulldb <- expand.grid(E=Espace,L=Lspace,P=Pspace,
                      inv=invspace,Ecoef=Ecoefspace,LL=LLspace,
                      Lcost=Lcostspace,volcost=volcostspace,
                      grpsz=grpszspace,N=Nspace)

fulldb$other_equis <- fulldb$equi_volunteering <- NA

# When does leadership evolve
# Baseline <- 5 # change if needed
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
 
  progress(i, nrow(fulldb), increment=10000)
}
sum(is.na(fulldb$equi_volunteering)) # 0
write.csv(fulldb,"voluteering_equilibria_homogenous.csv",row.names=F)


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


table(fulldb$LdrToFollowerOutcome,fulldb$volunteering,fulldb$efficient)

# write.csv(fulldb,"voluteering_equilibria_homogenous.csv",row.names=F)

## Some graphs ##

fulldb$class <- NA
fulldb$class[which(fulldb$efficient==0 & fulldb$LdrToFollowerOutcome=="Beneficial" & fulldb$volunteering!="none")] <- "Leader selfishness"
fulldb$class[which(fulldb$efficient==0 & fulldb$volunteering=="none")] <- "NoLdr_inefficient"
fulldb$class[which(fulldb$efficient==0 & fulldb$volunteering=="rare" & fulldb$LdrToFollowerOutcome=="Equal")] <- "Equal leadership" # previously "Low useless leadership"
fulldb$class[which(fulldb$efficient==1 & fulldb$volunteering!="none" & fulldb$LdrToFollowerOutcome=="Equal")] <- "Equal leadership"
fulldb$class[which(fulldb$efficient==1 & fulldb$volunteering=="none")] <- "NoLdr_costly"
fulldb$class[which(fulldb$efficient==1 & fulldb$volunteering!="none"& fulldb$LdrToFollowerOutcome=="Costly")] <- "Costly Leadership"
fulldb$class[which(fulldb$efficient==1 & fulldb$volunteering!="none"& fulldb$LdrToFollowerOutcome=="Beneficial")] <- "Beneficial Leadership"

fulldb$any_vol <- as.numeric(fulldb$equi_volunteering>0)
mod <- glm(any_vol ~ E + L + P + inv + Ecoef + LL + Lcost + volcost + grpsz + N,data=fulldb,family=binomial)

library(plotyl)
library(rgl)

selection <- which(fulldb$E==2 & fulldb$inv==0.4 & fulldb$Ecoef==0.9 & fulldb$LL==0.4 & fulldb$Lcost==0.5 & fulldb$volcost==0.5 & fulldb$grpsz==5 & fulldb$N==100)

## create special db just for this graph

graph_db <- expand.grid(P=seq(0,14,0.1),L=seq(0,14,0.1),E=2,
                      inv=0.4,Ecoef=0.9,LL=0.4,
                      Lcost=0.5,volcost=0.5,
                      grpsz=5,N=100)
graph_db$other_equis <- graph_db$equi_volunteering <- NA

# When does leadership evolve
Baseline <- 5 # change if needed
for(i in 1:nrow(graph_db)){
  options <- find_vol_equi(graph_db[i,],vol_ben,returns=T)

 graph_db$equi_volunteering[i] <- options$vol_eq[which.max(options$returns)]
 
 res <- with(graph_db[i,],vol_ben(as.numeric(equi_volunteering),grpsz=grpsz,L=L,LL=LL,P=P,inv=inv,Lcost=Lcost,E=E,Ecoef=Ecoef,volcost=volcost,N=N,dataheavy=T))
  
 graph_db$effective_E[i] <- res$LoL_effective_E
 
 if(length(options)>1) graph_db$other_equis[i] <- paste(options[-which.max(options$returns)],collapse=";")

  graph_db$Ldr_return[i] <- res$Ldr_return
  graph_db$Flwr_return[i] <- res$Flwr_return
 graph_db$NoLeader_return[i] <- res$NoLdr_return
   
  progress(i, nrow(graph_db), increment=10000)
}
graph_db$volunteering <- "none"
graph_db$volunteering[graph_db$equi_volunteering>0 & graph_db$equi_volunteering<=0.333] <- "rare"
graph_db$volunteering[graph_db$equi_volunteering>0.333 & graph_db$equi_volunteering<=0.666] <- "common"
graph_db$volunteering[graph_db$equi_volunteering>0.666 & graph_db$equi_volunteering<1] <- "most"
graph_db$volunteering[graph_db$equi_volunteering==1] <- "All"

graph_db$Leadered_grp_return <- graph_db$Ldr_return+graph_db$Flwr_return*(graph_db$grpsz-1)
graph_db$NoLeader_grp_return <- graph_db$NoLeader_return*graph_db$grpsz

graph_db$efficient <- as.numeric(as.numeric(graph_db$Leadered_grp_return)>as.numeric(graph_db$NoLeader_grp_return))

graph_db$LeaderFollowerRatio <- graph_db$Ldr_return/graph_db$Flwr_return
graph_db$LdrToFollowerOutcome <- "Equal"
graph_db$LdrToFollowerOutcome[which(graph_db$LeaderFollowerRatio>1)] <- "Beneficial"
graph_db$LdrToFollowerOutcome[which(graph_db$LeaderFollowerRatio<1)] <- "Costly"
graph_db$class <- NA
graph_db$class[which(graph_db$efficient==0 & graph_db$LdrToFollowerOutcome=="Beneficial" & graph_db$volunteering!="none")] <- "Leader selfishness"
graph_db$class[which(graph_db$efficient==0 & graph_db$volunteering=="none")] <- "NoLdr_inefficient"
graph_db$class[which(graph_db$efficient==0 & graph_db$volunteering=="rare" & graph_db$LdrToFollowerOutcome=="Equal")] <- "Equal leadership" # previously "Low useless leadership"
graph_db$class[which(graph_db$efficient==1 & graph_db$volunteering!="none" & graph_db$LdrToFollowerOutcome=="Equal")] <- "Equal leadership"
graph_db$class[which(graph_db$efficient==1 & graph_db$volunteering=="none")] <- "NoLdr_costly"
graph_db$class[which(graph_db$efficient==1 & graph_db$volunteering!="none"& graph_db$LdrToFollowerOutcome=="Costly")] <- "Costly Leadership"
graph_db$class[which(graph_db$efficient==1 & graph_db$volunteering!="none"& graph_db$LdrToFollowerOutcome=="Beneficial")] <- "Beneficial Leadership"


surface <- as.matrix(reshape(graph_db[,c("P","L","equi_volunteering")], idvar = "P", timevar = "L", direction = "wide"))[,-1]
fig <- plot_ly(graph_db, x=~L,y=~P,z=~equi_volunteering, color=~class,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="Leadership"),
			                    yaxis=list(title="Individual Production"),
								zaxis=list(title="Percent volunteers at equilibrium")))

fig <- plot_ly(z=~surface) |> add_surface()

ff <- as.matrix(reshape(graph_db[,c("P","L","class")], idvar = "P", timevar = "L", direction = "wide"))[,-1]
ff<-factor(ff, 
    levels=c(unique(graph_db$class)), 
    labels=c(unique(graph_db$class)))
	

fx<-matrix(as.numeric(ff), nrow=sqrt(nrow(graph_db)))
image(fx,
    breaks=(1:(nlevels(ff)+1))-.5,
	col=brewer.pal(nlevels(ff),"Set1"),
	xlab="Individual productivity",
	ylab="Leader multiplier",
	xaxt='n', yaxt='n')
axis(1, at=seq(0,1,1/14),labels=c(0,'','','',4,'','',7,'','',10,'','','',14))
axis(2, at=seq(0,1,1/14),labels=c(0,'','','',4,'','',7,'','',10,'','','',14))

legend (0.2,0.9,fill=brewer.pal(nlevels(ff),"Set1"),legend=levels(ff),bty='n')


### Now same but with different leadership groups

Espace <- c(0,2,5,10,20)
Pspace <- c(0,3,7,15)
Lspace <- c(1,3,7,15)
invspace  <- LLspace <-Ecoefspace <- c(0.1,0.5,0.9)
Lcostspace <- volcostspace <- c(0,0.2,1)
grpszspace <- c(5,10)
Nspace <- c(100)
HiLpropspace <- c(0.1,0.5)
HiLspace <- c(1.2,2)
HiLPspace <- c(0.7,1,2)

# Create the database
fulldb2 <- expand.grid(E=Espace,L=Lspace,P=Pspace,
                       inv=invspace,Ecoef=Ecoefspace,LL=LLspace,
                       Lcost=Lcostspace,volcost=volcostspace,
                       grpsz=grpszspace,N=100,HiLprop = HiLpropspace,HiLmulti = HiLspace,HiLPmulti = HiLPspace)

fulldb2$HiL <- fulldb2$L*fulldb2$HiLmulti
fulldb2$PHiL <- fulldb2$P*fulldb2$HiLPmulti

fulldb2$HiLvol <- fulldb2$LoLvol <-NA

# Now run the code
for(i in 1:nrow(fulldb2)){
  LoLvol <- 0.00000000001
  HiLvol <- 0
  cycle <- 1
  while(HiLvol!=1 & LoLvol!=0 & cycle <20){
    HiLvol <- find_vol_equi(fulldb2[i,],vol_ben,pvol=LoLvol,search.all=F,Lgroup="HiL")
    if(length(HiLvol)>1) HiLvol <- HiLvol[which.max(find_vol_equi(fulldb2[i,],vol_ben,pvol=LoLvol,returns=T,search.all=F)$returns)]
    LoLvol <- find_vol_equi(fulldb2[i,],vol_ben,pvolHiL=HiLvol,search.all=F,Lgroup="LoL")
    if(length(LoLvol)>1) LoLvol <- LoLvol[which.max(find_vol_equi(fulldb2[i,],vol_ben,pvolHiL=HiLvol,returns=T,search.all=F)$returns)]
    cycle <- cycle+1
  }
  
  # 1 last cycle for good luck
  HiLvol <- find_vol_equi(fulldb2[i,],vol_ben,pvol=LoLvol,search.all=F)
  if(length(HiLvol)>1) HiLvol <- HiLvol[which.max(find_vol_equi(fulldb2[i,],vol_ben,pvol=LoLvol,returns=T,search.all=F)$returns)]
  LoLvol <- find_vol_equi(fulldb2[i,],vol_ben,pvolHiL=HiLvol,search.all=F)
  if(length(LoLvol)>1) LoLvol <- LoLvol[which.max(find_vol_equi(fulldb2[i,],vol_ben,pvolHiL=HiLvol,returns=T,search.all=F)$returns)]
  
  fulldb2$HiLvol[i] <- HiLvol
  fulldb2$LoLvol[i] <- LoLvol
  
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

Baseline <- 5 # change if needed

for (i in 1:nrow(fulldb2)){
  
  res <- with(fulldb2[i,],vol_ben(pvol=as.numeric(LoLvol),pvolHiL=as.numeric(HiLvol),grpsz=grpsz,L=L,LL=LL,P=P,HiL=HiL,PHiL=PHiL,HiLprop=HiLprop, inv=inv,Lcost=Lcost,E=E,Ecoef=Ecoef,volcost=volcost,N=N,dataheavy=T))
  
  fulldb2$HiL_effective_E[i] <- res$HiL_effective_E
  fulldb2$LoL_effective_E[i] <- res$LoL_effective_E
  
  fulldb2$Ldr_return[i] <- res$Ldr_return
  fulldb2$Flwr_return[i] <- res$Flwr_return
  fulldb2$NoLdr_return[i] <- res$NoLdr_return
  
  fulldb2$Rotating_Leader_return[i] <- res$Rotating_Leader_return
  progress(i, nrow(fulldb2), increment=10000)
}
fulldb2$Leadered_grp_return <- fulldb2$Ldr_return+fulldb2$Flwr_return*(fulldb2$grpsz-1)
fulldb2$NoLdr_grp_return <- fulldb2$NoLdr_return*fulldb2$grpsz
fulldb2$efficient <- as.numeric(fulldb2$Leadered_grp_return>fulldb2$NoLdr_grp_return)
table(fulldb2$equi_type,fulldb2$efficient)
fulldb2$LdrToFollowerOutcome <- "Equal"
fulldb2$LdrToFollowerOutcome[which(fulldb2$Ldr_return>fulldb2$Flwr_return)] <- "Beneficial"
fulldb2$LdrToFollowerOutcome[which(fulldb2$Ldr_return<fulldb2$Flwr_return)] <- "Costly"

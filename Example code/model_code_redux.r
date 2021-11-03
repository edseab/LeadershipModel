library(Counterfact)
library(LeadershipModel)
library(rootSolve)
library(RColorBrewer)
library(plotly)
library(DiagrammeR)
library(DiagrammeRsvg)

checkfunc <- function(E,Ecoef,L,P,I,Cv,Cf,n,Ewarn=F,sol.save=F, retvol=F){
if(E-L*I*n>0){ 
E <- L*I*n
if(Ewarn)print(paste0("E was too big, replaced with ",E))
}

sol <- uniroot.all(function(v){


(E*Ecoef-P*I-Cf)/(1+v*(n-1)) - Cv - (E/n-L*I)*(1-v)^(n-1) #(12)

}, interval=c(0,1),tol=1e-12)

if(length(sol)>1){
 if(Ewarn) print(paste0("multiple v_hat:",paste(sol,collapse=';')))
sol <- sol[which.max((P+L*I-E/n)-(1-sol)^(n-1)*(L*I-E/n))]
}
if(length(sol)==0) sol <- as.numeric(E*(Ecoef*n-1)/n - I*(P-L) - Cf - Cv> 1e-5 ) # (13)
ret <- (P+L*I-E/n)-(1-sol)^(n-1)*(L*I-E/n)
if(retvol)ret <- (E*Ecoef-I*P-Cf)/(1+sol*(n-1)) +(P+L*I-E/n-Cv)

ifelse(sol.save,return(c(wbar=ret,vhat=sol)),return(ret))
}


fulldb<-expand.grid(Cp=seq(1.1,14,0.1),L=seq(1,14,0.1),E=4,
                      I=0.5,Ce=1,
                      Cf=0.5,Cv=0.5,
                      grpsz=5)
fulldb$vhat <- do.call(apply,args=c(list(X=fulldb,MARGIN=1,FUN=function(x)return(checkfunc(E=x['E'],P=x['Cp'],L=x['L'],I=x['I'],Ecoef=x['Ce'],Cv=x['Cv'],Cf=x['Cf'],n=x['grpsz'],sol.save=T)['vhat']))))
fulldb$wLeader <- with(fulldb, L*I +(1-I)*Cp+ E*(grpsz*Ce-1)/grpsz -Cf-Cv)
fulldb$wFollower <- with(fulldb, L*I +Cp- E/grpsz)

fulldb$LFReturnRatio <- fulldb$wLeader/fulldb$wFollower
fulldb$LFReturnRatio[abs(fulldb$wLeader-fulldb$wFollower)<1e-3] <- 1
fulldb$LFReturnRatioCats <- as.character(cut(fulldb$LFReturnRatio,breaks=c(-1,0.001,0.8,0.999,1,1.2,3)))
fulldb$LFReturnRatioCats[fulldb$vhat==0] <- "No leadership"
fulldb$efficient <- (fulldb$wLeader + (fulldb$grpsz-1)*(fulldb$wFollower - fulldb$vhat*fulldb$Cv))/fulldb$grpsz*fulldb$Cp>1

colours <- c(brewer.pal(7,"RdYlGn"),'#AED6F1')
plot_ly(fulldb, x=~L,y=~Cp,z=~vhat, color=~as.factor(LFReturnRatioCats),colors=colours,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="Leader multiplier L"),
			                    yaxis=list(title="Opportunity cost Cp"),
								zaxis=list(title="Percent volunteers at equilibrium v_hat"),
								aspectratio=list(x=1,y=1,z=0.5)),
					font=list(size=16)) -> fig
			  
fig


htmlwidgets::saveWidget(partial_bundle(fig),'PxL_E4I5Ce1Cf05Cv05n5.html')


istarcheck <- function(E,Ecoef,L,P,Cv,Cf,n,save.vhat=FALSE){


sol <- function(I,save.vhat=F){
sapply(I,function(.)checkfunc(E=E,Ecoef=Ecoef,L=L,P=P,Cv=Cv,Cf=Cf,n=n,I=.,sol.save=T)['vhat'])
}

Istar <- tryCatch(as.numeric(integrate(function(I)I*sol(I),0,1)$value)/as.numeric(integrate(sol,0,1)$value),error=function(x) return(0))
if(Istar %in% c(NA,NaN)) Istar <- 0
if(save.vhat){
vhat <- as.numeric(integrate(sol,0,1)$value)
return(c(Istar=Istar,vhat=vhat))
}else return(Istar)
}

optimalE <- db <- expand.grid(P = c(0,1,3,7,12,15),
                L = c(1,3,7,12,15),
				Ecoef = c(0,0.1,0.4,0.9,1),
				inv=c(0.1,0.3,0.6,0.9),
                Lcost = c(0,0.5,1,2),
				volcost = c(0,0.5,1,2),
                grpsz = c(5,10),opt.I=F)
optimalE_save <- optimalE

		
optimalE <- db <- expand.grid(Cp=seq(1.1,14,0.1),L=seq(1,14,0.1),
                      I=0.5,Ce=0.6,
                      Cf=0.5,Cv=0.5,
                      grpsz=5)			
for (i in 1:nrow(optimalE)){
my.row <- optimalE[i,]
arguments <- with(my.row,list(L=L,P=Cp,I=I,Ecoef=Ce,Cv=Cv,Cf=Cf,n=n))

max_E <- with(my.row,as.numeric(L)*as.numeric(I)*as.numeric(n))
points <- do.call(sapply,args=c(list(X=seq(0,max_E,0.05),FUN=checkfunc),arguments))
if(which.max(points) %in% c(1,length(points))){Estar <- points[which.max(points)]
}else if(points[which.max(points)]-points[1]<1e-05){ Estar <- 0
}else {
start.val <- max(c(which.max(points)-20,1))
stop.val <-min(c(which.max(points)+20,length(points)))
Estar <- do.call(optimize,args=c(list(checkfunc,c(seq(0,max_E,0.05)[start.val],seq(0,max_E,0.05)[stop.val]),maximum=T,tol=1e-12),arguments))$maximum
}
vol <- do.call(checkfunc,args=c(list(E=Estar,sol.save=T),arguments))['vhat']
optimalE$Estar[i] <- Estar 
optimalE$vhat[i] <- vol
optimalE$wLeader[i] <- with(optimalE[i,], L*I +(1-I)*Cp+ Estar*(n*Ce-1)/n -Cf-Cv)
optimalE$wFollower[i] <- with(optimalE[i,], L*I +Cp- Estar/n)
progress(i,nrow(optimalE), increment=100000)
}
optimalE$LFReturnRatio <- optimalE$wLeader/optimalE$wFollower
optimalE$LFReturnRatio[abs(optimalE$wLeader-optimalE$wFollower)<1e-3] <- 1
optimalE$LFReturnRatioCats <- as.character(cut(optimalE$LFReturnRatio,breaks=c(-1,0.0001,0.6,0.7,0.95,0.999,1,1.05,1.2,1.3,3)))
optimalE$LFReturnRatioCats[optimalE$vhat==0] <- "No leadership"
colours <- c(brewer.pal(10,'RdYlGn')[c(3,5,8,9)],'#AED6F1')
fig <- plot_ly(optimalE, x=~L,y=~Cp,z=~vhat,type="scatter3d",
                              color = ~as.factor(LFReturnRatioCats),colors=colours) |>
			  layout(scene=list(xaxis=list(title="Leader multiplier L"),
			                    yaxis=list(title="Opportunity cost Cp"),
								zaxis=list(title="Percent volunteers at equilibrium v_hat"),
								aspectratio=list(x=1,y=1,z=0.5)),
								font=list(size=16))

fig
htmlwidgets::saveWidget(partial_bundle(fig),'optimE_LxPinv5lc0.5vc0.5ec0.6n5.html')

htmlwidgets::saveWidget(partial_bundle(fi2g),'optimE_LxPinv9lc2vc1ec6n5.html')
colfunc <- c('#AED6F1',brewer.pal(9,'RdYlGn'))
xl <- 1
yb <- 1
xr <- 1.5
yt <- 2
par(mar=c(2,7,4,2))
plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
rect(
     xl,
     head(seq(yb,yt,(yt-yb)/10),-1),
     xr,
     tail(seq(yb,yt,(yt-yb)/10),-1),
     col=colfunc
    )
mtext(c("No candidates",0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4),side=2,at=tail(seq(yb,yt,(yt-yb)/10),-1)-0.05,las=1,cex=1.2)
title('Ratio of payoff leader/follower', cex.main=2)

check <- optimalE[which(optimalE$Cp==3),]
par(mar = c(5, 4, 4, 4) + 0.3)
plot(check$L,c(rep(NA,sum(check$vhat==0)),check$LFReturnRatio[check$vhat>0]),lwd=1.2,axes=F,type='l',lty=2,ylab='',xlab='Leadership multiplier L',main=('Equilibrium volunteering (vhat) and Leader/Follower payoff ratio with Ce=0.6'))
axis(side = 4, at = pretty(range(check$LFReturnRatio[check$vhat>0])))
abline(h=1)
par(new = TRUE)
plot(check$L,check$vhat,type='l',xlab='',ylab='V hat')
mtext("w(Leader)/w(Follower)", side = 4, line = 3)
# lines(check$L,c(rep(NA,sum(check$vhat==0)),check$Estar[check$vhat>0]-4),lty=3)
polyCurve (check$L[check$vhat>0],check$vhat[check$vhat>0],to=check$L[check$LFReturnRatio==1],col="#66BD63",dens=10)
polyCurve (check$L[check$vhat>0],check$vhat[check$vhat>0],from=check$L[check$LFReturnRatio==1],col="#F46D43",dens=10)


## Istar figure
optimalEinv <- db <- expand.grid(Cp=seq(1,14,0.1),L=seq(1,14,0.1),Ce=1,
                      Cf=0.5,Cv=1,
                      grpsz=5)
for(i in 1:nrow(optimalEinv)){

my.row <- optimalEinv[i,]
arguments <- with(my.row,list(L=L,P=Cp,Ecoef=Ce,Cv=Cv,Cf=Cf,n=grpsz))

max_E <- with(my.row,as.numeric(L)*as.numeric(grpsz)*0.8)
points <- sapply(seq(0,max_E,.1),function(.)do.call(checkfunc,c(list(E=.,I=as.numeric(tryCatch(do.call(istarcheck,c(list(E=.),arguments)),error=function(x) return(0)))),arguments)))
if(which.max(points) %in% c(1,length(points))){Estar <- points[which.max(points)]
}else if(points[which.max(points)]-points[1]<1e-05){ Estar <- 0
}else {
start.val <- max(c(which.max(points)-10,1))
stop.val <-min(c(which.max(points)+10,length(points)))
optifunc <- function(E){
Istar <- do.call(istarcheck,c(list(E=E),arguments))
return(do.call(checkfunc,c(list(E=E,I=Istar),arguments)))
}
Estar <- optimize(optifunc,c(seq(0,max_E,0.05)[start.val],seq(0,max_E,0.05)[stop.val]),maximum=T,tol=1e-12)$maximum
}
Istar_full <-do.call(istarcheck,c(list(E=Estar,save.vhat=T),arguments))
Istar <- Istar_full['Istar']
vol <- do.call(checkfunc,args=c(list(E=Estar,I=Istar,sol.save=T),arguments))['vhat']
optimalEinv$Estar[i] <- Estar 
optimalEinv$vhat[i] <- vol
optimalEinv$vhat_check[i] <- Istar_full['vhat']
optimalEinv$wLeader[i] <- with(optimalEinv[i,], L*Istar +(1-Istar)*Cp+ Estar*(grpsz*Ce-1)/grpsz -Cf-Cv)
optimalEinv$wFollower[i] <- with(optimalEinv[i,], L*Istar +Cp- Estar/grpsz)
progress(i,nrow(optimalEinv), increment=100000)
}
 optimalEinv$LFReturnRatio <- optimalEinv$wLeader/optimalEinv$wFollower
optimalEinv$LFReturnRatio[abs(optimalEinv$wLeader-optimalEinv$wFollower)<1e-3] <- 1
optimalEinv$LFReturnRatioCats <- as.character(cut(optimalEinv$LFReturnRatio,breaks=c(-1,0.0001,0.6,0.7,0.8,0.999,1,1.10,1.2,1.3,3)))
optimalEinv$LFReturnRatioCats[optimalEinv$vhat==0] <- "No leadership"
colours <- c(brewer.pal(8,'RdYlGn'),'#AED6F1')
fig <- plot_ly(optimalEinv, x=~L,y=~Cp,z=~vhat,type="scatter3d",
                              color = ~as.factor(LFReturnRatioCats),colors=colours) |>
			  layout(scene=list(xaxis=list(title="Leader multiplier L"),
			                    yaxis=list(title="Opportunity cost Cp"),
								zaxis=list(title="Percent volunteers at equilibrium v_hat"),
								aspectratio=list(x=1,y=1,z=0.5)),
								font=list(size=16))

fig
htmlwidgets::saveWidget(partial_bundle(fig),'optimalEinv_LxPlc0.5vc1ec1n5.html')


graph_db_volc <- expand.grid(Cv=seq(0,2,0.1),grpsz=seq(5,30,1),E=4,
                      I=0.5,Ce=1,
                      Cf=0.5,L=10,Cp=5)
graph_db_volc$vhat <- do.call(apply,args=c(list(X=graph_db_volc,MARGIN=1,FUN=function(x)return(checkfunc(E=x['E'],P=x['Cp'],L=x['L'],I=x['I'],Ecoef=x['Ce'],Cv=x['Cv'],Cf=x['Cf'],n=x['grpsz'],sol.save=T)['vhat']))))

with(graph_db_volc[graph_db_volc$grpsz==5,],plot(Cv,vhat,type='l', ylim=c(0,1),ylab='Equilibrium volunteering vhat',xlab='Cost of volunteering Cv'))
with(graph_db_volc[graph_db_volc$grpsz==10,],lines(Cv,vhat,lty=6))
with(graph_db_volc[graph_db_volc$grpsz==15,],lines(Cv,vhat,lty=5))
with(graph_db_volc[graph_db_volc$grpsz==20,],lines(Cv,vhat,lty=4))
with(graph_db_volc[graph_db_volc$grpsz==25,],lines(Cv,vhat,lty=3))
legend(1.5,0.9,legend=seq(5,25,5),lty=c(1,6:3),title="Group size n")



graph_db_lc <- expand.grid(Cf=seq(0,5,0.2),Emult = seq(0,1,0.05),Cp=10,
                      I=0.5,Ce=1,
                      Cv=0.5,L=18,grpsz=5)
graph_db_lc$E <- graph_db_lc$Cf*graph_db_lc$Emult+graph_db_lc$Cv
graph_db_lc$vhat <- do.call(apply,args=c(list(X=graph_db_lc,MARGIN=1,FUN=function(x)return(checkfunc(E=x['E'],P=x['Cp'],L=x['L'],I=x['I'],Ecoef=x['Ce'],Cv=x['Cv'],Cf=x['Cf'],n=x['grpsz'],sol.save=T)['vhat']))))

with(graph_db_lc[graph_db_lc$Emult==1,],plot(Cf,vhat,type='l', ylim=c(0,.25),xlim=c(0,5),ylab='Equilibrium volunteering vhat',xlab='Fixed cost of leadership Cf'))
with(graph_db_lc[graph_db_lc$Emult==0.8,],lines(Cf,vhat,lty=6))
with(graph_db_lc[graph_db_lc$Emult==0.5,],lines(Cf,vhat,lty=5))
with(graph_db_lc[graph_db_lc$Emult==0.2,],lines(Cf,vhat,lty=4))
with(graph_db_lc[graph_db_lc$Emult==0,],lines(Cf,vhat,lty=3))
legend(0.5,0.1,legend=c('100%','80%','50%','20%','0%'),lty=c(1,6:3),title="Percent of Cf reimbursed")



# Ce figure

fulldb<-expand.grid(Cv=seq(0,2,0.05),L=seq(1,10,0.1),E=4,
                      I=0.5,Ce=1,
                      Cf=0.5,Cp=5,
                      grpsz=5)
fulldb$vhat <- do.call(apply,args=c(list(X=fulldb,MARGIN=1,FUN=function(x)return(checkfunc(E=x['E'],P=x['Cp'],L=x['L'],I=x['I'],Ecoef=x['Ce'],Cv=x['Cv'],Cf=x['Cf'],n=x['grpsz'],sol.save=T)['vhat']))))
fulldb$wLeader <- with(fulldb, L*I +(1-I)*Cp+ E*(grpsz*Ce-1)/grpsz -Cf-Cv)
fulldb$wFollower <- with(fulldb, L*I +Cp- E/grpsz)

fulldb$LFReturnRatio <- fulldb$wLeader/fulldb$wFollower
fulldb$LFReturnRatio[abs(fulldb$wLeader-fulldb$wFollower)<1e-3] <- 1
fulldb$LFReturnRatioCats <- as.character(cut(fulldb$LFReturnRatio,breaks=c(-1,0.001,0.8,0.999,1,1.2,3)))
fulldb$LFReturnRatioCats[fulldb$vhat==0] <- "No leadership"
fulldb$efficient <- (fulldb$wLeader + (fulldb$grpsz-1)*(fulldb$wFollower - fulldb$vhat*fulldb$Cv))/fulldb$grpsz*fulldb$Cp>1

colours <- c(brewer.pal(7,"RdYlGn"),'#AED6F1')
fig <- plot_ly(fulldb, x=~L,y=~Cv,z=~vhat, color=~as.factor(LFReturnRatioCats),colors=colours,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="Leader multiplier L"),
			                    yaxis=list(title="Cost of volunteering Cv"),
								zaxis=list(title="Percent volunteers at equilibrium v_hat"),
								aspectratio=list(x=1,y=1,z=0.5)),
					font=list(size=16))
			  
fig


# effect of E on I*
E<- seq(0,8,0.1)
plot(E,sapply(E,function(x)istarcheck(E=x,Ecoef=1,L=10,P=3,Cv=0.5,Cf=1,n=5)),type='l',lwd=1.2,ylim=c(0.3,0.8),ylab='Expected rate of investment I*',xlab='Payment to leader E')
lines(E,sapply(E,function(x)istarcheck(E=x,Ecoef=1,L=3,P=10,Cv=0.5,Cf=1,n=5)),lwd=1.2,lty=3)
legend(0.6,0.8,lty=c(1,3), legend = c('L=10  Cp=3','L=3  Cp=10'))

invdb <- expand.grid(I=seq(0,1,0.01),E=3,Cp=2,L=6,Ce=1,Cf=3,Cv=0.5,grpsz=5)
invdb$vhat <- do.call(apply,args=c(list(X=invdb,MARGIN=1,FUN=function(x)return(checkfunc(E=x['E'],P=x['Cp'],L=x['L'],I=x['I'],Ecoef=x['Ce'],Cv=x['Cv'],Cf=x['Cf'],n=x['grpsz'],sol.save=T)['vhat']))))

Istar <- sum(invdb$I*invdb$vhat)/sum(invdb$vhat)
VforIstar <- create_model_df(inv=Istar,E=3,P=2,L=6,Ecoef=1,LL=0,Lcost=3,volcost=0.5,grpsz=5,N=100)$equi_volunteering
Vstar <- sum(invdb$equi_volunteering)/nrow(invdb)
Imin <- invdb$inv[which.max(invdb$inv[invdb$equi_volunteering>0])]
plot(invdb$I,invdb$vhat, type='l',lwd=2,
                  ylim=c(0,1),ylab='Equilibrium volunteering vhat',
				  xlab='Investment rate I')

# Low cost prestige leadership


optimalEinv <- db <- expand.grid(Cp=1,L=seq(0,5,0.1),Ce=0.9,
                      Cf=0,Cv=0,
                      grpsz=5)			
for(i in 1:nrow(optimalEinv)){

my.row <- optimalEinv[i,]
arguments <- with(my.row,list(L=L,P=Cp,Ecoef=Ce,Cv=Cv,Cf=Cf,n=grpsz))

max_E <- with(my.row,as.numeric(L)*as.numeric(grpsz)*0.8)
points <- sapply(seq(0,max_E,.1),function(.)do.call(checkfunc,c(list(E=.,I=as.numeric(tryCatch(do.call(istarcheck,c(list(E=.),arguments)),error=function(x) return(0)))),arguments)))
if(which.max(points) %in% c(1,length(points))){Estar <- points[which.max(points)]
}else if(points[which.max(points)]-points[1]<1e-05){ Estar <- 0
}else {
start.val <- max(c(which.max(points)-10,1))
stop.val <-min(c(which.max(points)+10,length(points)))
optifunc <- function(E){
Istar <- do.call(istarcheck,c(list(E=E),arguments))
return(do.call(checkfunc,c(list(E=E,I=Istar),arguments)))
}
Estar <- optimize(optifunc,c(seq(0,max_E,0.05)[start.val],seq(0,max_E,0.05)[stop.val]),maximum=T,tol=1e-12)$maximum
}
Istar_full <-do.call(istarcheck,c(list(E=Estar,save.vhat=T),arguments))
Istar <- Istar_full['Istar']
vol <- do.call(checkfunc,args=c(list(E=Estar,I=Istar,sol.save=T),arguments))['vhat']
optimalEinv$Estar[i] <- Estar 
optimalEinv$vhat[i] <- vol
optimalEinv$vhat_check[i] <- Istar_full['vhat']
optimalEinv$wLeader[i] <- with(optimalEinv[i,], L*Istar +(1-Istar)*Cp+ Estar*(grpsz*Ce-1)/grpsz -Cf-Cv)
optimalEinv$wFollower[i] <- with(optimalEinv[i,], L*Istar +Cp- Estar/grpsz)
progress(i,nrow(optimalEinv), increment=100000)
}
 optimalEinv$LFReturnRatio <- optimalEinv$wLeader/optimalEinv$wFollower
optimalEinv$LFReturnRatio[abs(optimalEinv$wLeader-optimalEinv$wFollower)<1e-3] <- 1
optimalEinv$LFReturnRatioCats <- as.character(cut(optimalEinv$LFReturnRatio,breaks=c(-1,0.0001,0.6,0.7,0.8,0.999,1,1.10,1.2,1.3,3)))
optimalEinv$LFReturnRatioCats[optimalEinv$vhat==0] <- "No leadership"

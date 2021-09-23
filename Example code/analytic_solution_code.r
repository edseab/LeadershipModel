setwd('C:/Users/Ed/Google Drive/Backup/Work 2 UNM/R programs/LeadershipModel/output')
# devtools::install_github("edseab/LeadershipModel")
library(LeadershipModel)
library(rootSolve)
library(RColorBrewer)
library(plotly)
library(DiagrammeR)
library(DiagrammeRsvg)


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

fulldb <- create_model_df(
				Espace = c(0,2,5,10,20),
                Pspace = c(0,1,3,7,15),
                Lspace = c(1,3,7,15),
                invspace  = c(0.1,0.4,0.9,1),
				LLspace = c(0.1,0.4,0.9,1)
				Ecoefspace = c(0.1,0.4,0.9,1),
                Lcostspace = c(0,0.1,0.5,2),
				volcostspace = c(0,0.1,0.5,2),
                grpszspace = c(5,10),
                Nspace = c(50,100,200))

table(fulldb$LdrToFollowerOutcome,fulldb$volunteering,fulldb$efficient)

# write.csv(fulldb,"homogenous_leadership.csv",row.names=F)

## Some graphs ##

fulldb$any_vol <- as.numeric(fulldb$equi_volunteering>0)
mod <- glm(any_vol ~ E + L + P + inv + Ecoef + LL + Lcost + volcost + grpsz + N,data=fulldb,family=binomial)

library(plotly)
library(rgl)

selection <- which(fulldb$E==2 & fulldb$inv==0.4 & fulldb$Ecoef==0.9 & fulldb$Lcost==0.5 & fulldb$volcost==0.5 & fulldb$grpsz==5 & fulldb$N==100)

## create special db just for this graph

graph_db <- create_model_df(P=seq(0,14,0.1),L=seq(0,14,0.1),E=2,
                      inv=0.4,Ecoef=0.9,
                      Lcost=0.5,volcost=0.5,LL=0,
                      grpsz=5,N=100)
colours <- brewer.pal(length(unique(graph_db$class
)),"Set2")
surface <- as.matrix(reshape(graph_db[,c("P","L","equi_volunteering")], idvar = "P", timevar = "L", direction = "wide"))[,-1]
fig <- plot_ly(graph_db, x=~L,y=~P,z=~equi_volunteering, color=~class,colors=colours,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="Leader multiplier L"),
			                    yaxis=list(title="Individual Production P"),
								zaxis=list(title="Percent volunteers at equilibrium v_hat")))

htmlwidgets::saveWidget(partial_bundle(fig),'PxL_E2inv4.html')

ff <- as.matrix(reshape(graph_db[,c("P","L","class")], idvar = "P", timevar = "L", direction = "wide"))[,-1]
ff<-factor(ff,labels=c(unique(graph_db$class)))
	

fx<-matrix(as.numeric(ff), nrow=sqrt(nrow(graph_db)))
image(fx,
    breaks=(1:(nlevels(ff)+1))-.5,
	col=colours,
	xlab="Individual productivity P",
	ylab="Leader multiplier L ",
	xaxt='n', yaxt='n')
axis(1, at=seq(0,1,1/14),labels=c(0,'','','',4,'','',7,'','',10,'','','',14))
axis(2, at=seq(0,1,1/14),labels=c(0,'','','',4,'','',7,'','',10,'','','',14))

# legend (0.2,0.9,fill=colours,legend=levels(ff),bty='n')


## Try again with different slices

selection <- which(fulldb$P==15 & fulldb$L==7 & fulldb$Ecoef==0.9 & fulldb$LL==0.1 & fulldb$Lcost==0.5 & fulldb$volcost==0.5 & fulldb$grpsz==5 & fulldb$N==100)

fig <- plot_ly(fulldb[selection,], x=~E,y=~inv,z=~equi_volunteering, color=~class,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="Extraction"),
			                    yaxis=list(title="Investment (%)"),
								zaxis=list(title="Percent volunteers at equilibrium")))

graph_dbE <- create_model_df(inv=seq(0,1,0.01),E=seq(0,20,0.5),P=3,L=7,Ecoef=0.9,LL=0.1,Lcost=4,volcost=0.5,grpsz=5,N=100)
fig <- plot_ly(graph_dbE, x=~E,y=~inv,z=~equi_volunteering, color=~class,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="Extraction"),
			                    yaxis=list(title="Investment (%)"),
								zaxis=list(title="Percent volunteers at equilibrium")))
htmlwidgets::saveWidget(partial_bundle(fig),'ExInv_P3L7Lc4.html')

graph_db_noE <- create_model_df(P=seq(0,14,0.1),L=seq(0,14,0.1),E=0,
                      inv=0.4,Ecoef=1,
                      Lcost=0.5,volcost=0.5,LL=0,
                      grpsz=5,N=100)
fig <- plot_ly(graph_db_noE, x=~L,y=~P,z=~equi_volunteering, color=~class,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="Leadership"),
			                    yaxis=list(title="Individual Production"),
								zaxis=list(title="Percent volunteers at equilibrium")))

htmlwidgets::saveWidget(partial_bundle(fig),'PxL_E0inv4.html')

graph_db_costs <- create_model_df(Lcost=seq(0,3,0.1),L=seq(0,15,0.5),E=2,
                      inv=0.4,Ecoef=1,
                      volcost=0.5,P=10,LL=0,
                      grpsz=5,N=100)
fig <- plot_ly(graph_db_costs, x=~Lcost,y=~L,z=~equi_volunteering, color=~class,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="Lcost"),
			                    yaxis=list(title="L"),
								zaxis=list(title="Percent volunteers at equilibrium")))

htmlwidgets::saveWidget(partial_bundle(fig),'CLxL_E2P10.html')

graph_db_volc <- create_model_df(volcost=seq(0,2,0.1),grpsz=seq(5,30,1),E=10,
                      inv=0.4,Ecoef=1,
                      Lcost=0.5,L=25,P=10,LL=0,
                      N=1000)
surface <- as.matrix(reshape(graph_db_volc[,c("volcost","grpsz","equi_volunteering")], idvar = "volcost", timevar = "grpsz", direction = "wide"))[,-1]
axes <- list(yaxis=list(title='Cost to candidacy Cv', tickmode = "array",nticks=3,tickvals=c(0,10,20),ticktext=0:2),
             xaxis=list(title='Groupsize n',
			            tickmode = "array",
                        ticktext = seq(5,30,5),
						tickvals = seq(0,25,5),
						nticks=6),
                    zaxis=list(title='Equilibrium volunteering rate v_bar'),
					camera=camera)
fig <- plot_ly(z = ~surface, showscale = TRUE, reversescale=T,colorscale = 'Portland')
fig <- fig %>% 
           add_surface()%>%
		   layout(scene = axes)
fig
htmlwidgets::saveWidget(partial_bundle(fig),'volcxn_L25P10.html')

graph_db_lc <- create_model_df(Lcost=seq(0,5,0.2),Emult = seq(0,1,0.05),P=10,
                      inv=0.4,Ecoef=1,
                      volcost=0.5,L=18,grpsz=5,LL=0,
                      N=1000,added=list(E='Lcost*Emult'))
surface <- as.matrix(reshape(graph_db_lc[,c("Lcost","Emult","equi_volunteering")], idvar = "Lcost", timevar = "Emult", direction = "wide"))[,-1]
axes <- list(yaxis=list(title='Cost of leadership CL', tickmode = "array",nticks=6,tickvals=seq(0,25,5),ticktext=0:5),
             xaxis=list(title='Proportion of CL reimbursed to leader',
			            tickmode = "array",
                        ticktext = seq(0,1,0.2),
						tickvals = seq(0,20,4),
						nticks=6),
                    zaxis=list(title='Equilibrium volunteering rate v_bar'))
fig <- plot_ly(z = ~surface, showscale = TRUE, reversescale=T,colorscale = 'Portland')
fig <- fig %>% 
           add_surface()%>%
		   layout(scene = axes)
fig
htmlwidgets::saveWidget(partial_bundle(fig),'LcxEprop_L18E5.html')

fx<-matrix(as.numeric(ffvolc), nrow=sqrt(nrow(graph_db)))
image(fx,
    breaks=(1:(nlevels(ffvolc)+1))-.5,
	col=colours,
	xlab="Individual productivity P",
	ylab="Leader multiplier L ",
	xaxt='n', yaxt='n')
axis(1, at=seq(0,1,1/14),labels=c(0,'','','',4,'','',7,'','',10,'','','',14))
axis(2, at=seq(0,1,1/14),labels=c(0,'','','',4,'','',7,'','',10,'','','',14))

# Try this investment thing:
fulldb <- follower_strat(
                P = c(0,1,3,7,12,15),
                L = c(1,3,7,12,15),
				Ecoef = c(0,0.1,0.4,0.9,1),
                Lcost = c(0,0.1,0.5,1,2),
				volcost = c(0,0.1,0.5,1,2),
                grpsz = c(5,10),
				N=100,
				increment=100000)
# write.csv(other_invest_database_NoLL,"optimal_payment_investmentNOLL.csv", row.names=F)
fulldb$GrpBenefitToLdr <- with(fulldb,Follower_return*(grpsz-1)+Leader_return-No_Ldr_Return*grpsz)
fulldb$LdrNetBenefit <- with(fulldb,Leader_return-fulldb$Follower_return)

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
  
  res <- with(fulldb2[i,],vol_ben(pvol=as.numeric(LoLvol),pvolHiL=as.numeric(HiLvol),grpsz=grpsz,L=L,LL=LL,P=P,HiL=HiL,PHiL=PHiL,HiLprop=HiLprop, inv=inv,Lcost=Lcost,E=E,Ecoef=Ecoef,volcost=volcost,N=N,select.best=T,dataheavy=T))
  
  fulldb2$HiL_effective_E[i] <- res$HiL_effective_E
  fulldb2$LoL_effective_E[i] <- res$LoL_effective_E
  
  fulldb2$Ldr_return[i] <- res$Ldr_return
  fulldb2$Flwr_return[i] <- res$Flwr_return
  fulldb2$NoLdr_returnHiL[i] <- res$NoLdr_returnHiL
  fulldb2$NoLdr_returnLoL[i] <- res$NoLdr_returnHiL

  fulldb2$Rotating_Leader_returnHiL[i] <- res$Rotating_Leader_returnHiL
  fulldb2$Rotating_Leader_returnLoL[i] <- res$Rotating_Leader_returnLoL
  progress(i, nrow(fulldb2), increment=10000)
}
fulldb2$Leadered_grp_return <- fulldb2$Ldr_return+fulldb2$Flwr_return*(fulldb2$grpsz-1)
fulldb2$NoLdr_grp_return <- (fulldb2$NoLdr_returnHiL*fulldb2$HiLprop + fulldb2$NoLdr_returnLoL*(1-fulldb2$HiLprop))*fulldb2$grpsz
fulldb2$efficient <- as.numeric(fulldb2$Leadered_grp_return>fulldb2$NoLdr_grp_return)
fulldb2$LdrToFollowerOutcome <- "Equal"
fulldb2$LdrToFollowerOutcome[which(fulldb2$Ldr_return>fulldb2$Flwr_return)] <- "Beneficial"
fulldb2$LdrToFollowerOutcome[which(fulldb2$Ldr_return<fulldb2$Flwr_return)] <- "Costly"

table(fulldb2$equi_type,fulldb2$LdrToFollowerOutcome,fulldb2$efficient)

 

	  
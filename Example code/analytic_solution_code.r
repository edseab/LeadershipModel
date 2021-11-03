setwd('C:/Users/Ed/Google Drive/Backup/Work 2 UNM/R programs/LeadershipModel/output')
# devtools::install_github("edseab/LeadershipModel")
library(LeadershipModel)
library(rootSolve)
library(RColorBrewer)
library(plotly)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)


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
optimalE <- follower_strat(
                P = c(0,1,3,7,12,15),
                L = c(1,3,7,12,15),
				Ecoef = c(0,0.1,0.4,0.9,1),
				inv=c(0.1,0.3,0.6,0.9),
                Lcost = c(0,0.5,1,2),
				volcost = c(0,0.5,1,2),
                grpsz = c(5,10),
				N=100,
				increment=100000,opt.I=F)
# write.csv(optimalE,"optimalE.csv", row.names=F)
optimalEinv <- follower_strat(
                P = c(0,1,3,7,12,15),
                L = c(1,3,7,12,15),
				Ecoef = c(0,0.1,0.4,0.9,1),
                Lcost = c(0,0.5,1,2),
				volcost = c(0,0.5,1,2),
                grpsz = c(5,10),
				N=100,
				increment=100000,opt.I=T)
# write.csv(optimalEinv,"optimalEinv.csv", row.names=F)
optimalE$GrpBenefitToLdr <- with(optimalE,Flwr_return*(grpsz-1)+Ldr_return-Acephalous_return*grpsz)
optimalE$LdrNetBenefit <- with(optimalE,Ldr_return-Flwr_return)
optimalE$LFReturnRatio <- with(optimalE,Ldr_return/Flwr_return)
optimalE$LFReturnRatio[abs(optimalE$wLeader-wFollower)<1e-3] <- 1
optimalE$LdrNetBenefit[abs(optimalE$LdrNetBenefit)<1e-3] <- 0

optimalE$volunteering <- cut(optimalE$expected_vol, breaks=c(-1,0,0.333,0.667,0.99999,2), labels=c("none","rare","common","most","all"),right=T)

#check this shit
selection <- which(optimalE$Ecoef==1 & optimalE$inv==0.7 & optimalE$Lcost==2 & optimalE$volcost==1 & optimalE$grpsz==5)

fig <- plot_ly(optimalE[selection,], x=~P,y=~L,z=~expected_vol,color=~LFReturnRatio ,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="P"),
			                    yaxis=list(title="L"),
								zaxis=list(title="Percent volunteers at equilibrium")))


graph_optimalE <- follower_strat(L=seq(1,15,0.01),P = 15,
                      inv=0.7,Ecoef=1,
                      volcost=1,Lcost=2,grpsz=5,LL=0,
                      N=100,opt.I=F)
graph_optimalE$LFReturnRatio <- with(graph_optimalE,Ldr_return/Flwr_return)
graph_optimalE$LFReturnRatioCats <- as.factor(cut(graph_optimalE$LFReturnRatio,breaks=c(0,0.5,0.8,0.999,1,1.2,1.5,5)))
plot(expected_vol~L,data=graph_optimalE,type='l')

fig <- plot_ly(graph_optimalE, x=~L,y=~P,z=~expected_vol,type="scatter3d",
                              color = ~LFReturnRatioCats) |>
			  layout(scene=list(xaxis=list(title="L"),
			                    yaxis=list(title="P"),
								zaxis=list(title="Percent volunteers at equilibrium")))

fig
htmlwidgets::saveWidget(partial_bundle(fig),'optimE_LxPinv6lc7vc2ec1n10.html')


graph_optimalEcoef <- follower_strat(L=15,P = 10,
                      inv=seq(0.1,1,0.01),Ecoef=seq(0.1,1,0.02),
                      volcost=2,Lcost=5,grpsz=5,LL=0,
                      N=10000,opt.I=F)
graph_optimalEcoef$LFReturnRatio <- with(graph_optimalEcoef,Ldr_return/Flwr_return)
graph_optimalEcoef$LFReturnRatioCats <- as.factor(cut(graph_optimalEcoef$LFReturnRatio,breaks=c(0,0.5,0.8,0.999,1,1.2,1.5,5)))
fig <- plot_ly(graph_optimalEcoef, x=~inv,y=~Ecoef,z=~expected_vol,type="scatter3d",
                              color = ~LFReturnRatioCats) |>
			  layout(scene=list(xaxis=list(title="inv"),
			                    yaxis=list(title="Ecoef"),
								zaxis=list(title="Percent volunteers at equilibrium")))

fig
htmlwidgets::saveWidget(partial_bundle(fig),'optimEcoef*inv_L10.html')



optimalE5247 Ecoef=1, inv=0.6, Lcost=0,volcost=1,grpsz=5,N=100


## Now with optimalEinv
optimalEinv$GrpBenefitToLdr <- with(optimalEinv,Flwr_return*(grpsz-1)+Ldr_return-Acephalous_return*grpsz)
optimalEinv$LdrNetBenefit <- with(optimalEinv,Ldr_return-Flwr_return)
optimalEinv$LFReturnRatio <- with(optimalEinv,Ldr_return/Flwr_return)
optimalEinv$LFReturnRatio[abs(optimalEinv$LdrNetBenefit)<10e-3] <- 1
optimalEinv$LdrNetBenefit[abs(optimalEinv$LdrNetBenefit)<10e-3] <- 0

optimalEinv$volunteering <- cut(optimalEinv$expected_vol, breaks=c(-1,0,0.333,0.667,0.99999,2), labels=c("none","rare","common","most","all"),right=T)

selection <- which(optimalEinv$Ecoef==1 &  optimalEinv$Lcost==0 & optimalEinv$volcost==0 & optimalEinv$grpsz==5)

fig <- plot_ly(optimalEinv[selection,], x=~P,y=~L,z=~expected_vol ,type="scatter3d") |>
			  layout(scene=list(xaxis=list(title="P"),
			                    yaxis=list(title="L"),
								zaxis=list(title="Percent volunteers at equilibrium")))

fig









## Show em how it works

#figure 5
invdb <- create_model_df(inv=seq(0,1,0.01),E=3,P=2,L=6,Ecoef=1,LL=0,Lcost=3,volcost=0.5,grpsz=5,N=100)
Istar <- sum(invdb$inv*invdb$equi_volunteering)/sum(invdb$equi_volunteering)
VforIstar <- create_model_df(inv=Istar,E=3,P=2,L=6,Ecoef=1,LL=0,Lcost=3,volcost=0.5,grpsz=5,N=100)$equi_volunteering
Vstar <- sum(invdb$equi_volunteering)/nrow(invdb)
Imin <- invdb$inv[which.max(invdb$inv[invdb$equi_volunteering>0])]
plot(invdb$inv,invdb$equi_volunteering, type='l',lwd=2,
                  ylim=c(0,1),ylab='Equilibrium volunteering vhat',
				  xlab='Investment rate I')

polyCurve(invdb$inv,invdb$equi_volunteering, col= 'purple')
segments(Istar,0,Istar,VforIstar,from=Imin,lwd=3)
lines(invdb$inv,invdb$equi_volunteering,lwd=2)
text(Istar+0.01,0.19,"I*",cex=1.5)
text(0.9,0.10,"v*",cex=1.5)

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
difflead <- create_model_df(leadership="hetero",E=Espace,L=Lspace,P=Pspace,
                       inv=invspace,Ecoef=Ecoefspace,LL=LLspace,
                       Lcost=Lcostspace,volcost=volcostspace,
                       grpsz=grpszspace,N=100,HiLprop = HiLpropspace,HiLmulti = HiLspace,HiLPmulti = HiLPspace)

# Now run the code

table(difflead$equi_type,difflead$LdrToFollowerOutcome,difflead$efficient)

 

	  
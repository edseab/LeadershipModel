
# devtools::install_github("edseab/LeadershipModel")
library(LeadershipModel)

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
plot(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,E=1,Eavg=0,P=1,Pavg=1),
      type = "l", ylim=c(0.4,2),
	  ylab="Return ratio relative to average",
	  xlab="Probability of volunteering",
	  main=c("L=Lavg=5    P=Pavg=1    E=2    Vavg=0.2"))  	  
	  text(0.55,1.15,labels="Eavg = 0",srt=4)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,P=1,Pavg=1,E=1,Eavg=2))
	  text(0.50,1.25,labels="Eavg = 1",srt=20)
lines(seq(0,1,0.1),modl(seq(0,1,0.1),0.2,L=5,Lavg=5,P=1,Pavg=1,E=1,Eavg=4))
	  text(0.42,1.37,labels="Eavg = 2",srt=50)


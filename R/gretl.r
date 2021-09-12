gretl <-  function(px,pavg,N=100,grpsz=5, LL=0.8, L=3, Lavg = 3, P=1, Pavg= 1,inv = 0.6,volcost=0.1, Lcost=0.2, E=0.5, Eavg=0.5, Ecoef = 0.9, Baseline=5){

# Probability that ego is in a different group from mutant
p_diff_group <- prod(((N-2):(N-grpsz))/((N-1):(N+1-grpsz)))
# Prob of mutant elected given ego also volunteering
p_elected_vol2 <- sum(dbinom(0:(grpsz-2),(grpsz-2),pavg)*(1/(2:grpsz)))
# Prob of mutant elected given ego not volunteering
p_elected_vol3 <-  sum(dbinom(0:(grpsz-2),(grpsz-2),pavg)*(1/(1:(grpsz-1))))
# probability of ego having volunteered, given non elected and given mutant volunteered
E <- min((LL + (L-LL)*inv)*grpsz,E)
Eavg <- min((LL + (Lavg-LL)*inv)*grpsz,E)

L_return_mut <- ((LL + (L-LL)*inv)*grpsz - E)/grpsz  + Baseline# Leadership of mutant
L_return_avg <- ((LL + (Lavg-LL)*inv)*grpsz - Eavg)/grpsz  + Baseline# Expected group Leadership

GEx <- (p_diff_group*(retl(px=pavg,pavg=pavg,grpsz=grpsz,LL=LL,L=Lavg,Lavg=Lavg,P=Pavg,Pavg=Pavg,inv=inv,volcost=volcost, E=Eavg, Eavg=Eavg,Ecoef=Ecoef)) #Expected return if ego in diff group from mutant
     + (1-p_diff_group)*( # If in same group:
		 (1-px)*retl(px=pavg,pavg=pavg,grpsz=grpsz, banned=1,LL=LL,L=Lavg,Lavg=Lavg,P=Pavg,Pavg=Pavg,inv=inv,volcost=volcost, E=Eavg, Eavg=Eavg,Ecoef=Ecoef) + # return if mutant doesnt volunteer
		 px*pavg*p_elected_vol2*(L_return_mut + Pavg - volcost) + # return if mutant is elected with ego volunteering
		 px*(1-pavg)*p_elected_vol3*(L_return_mut + Pavg) + # return if mutant is elected with ego not volunteering
		 px*pavg*(1-p_elected_vol2)*(1/3)*(L_return_avg + Pavg - volcost - Lcost - inv*Pavg + Ecoef*Eavg) + # return if ego is elected
		 px*pavg*(1-p_elected_vol2)*(2/3)*(L_return_avg + Pavg - volcost) +# return if ego and mutant both volunteer but aren't elected
		 px*(1-pavg)*(1-p_elected_vol3)*(L_return_avg) # return if mut volunteers and not ego and mut loses
	 ))
return(GEx)
}

retl <- function(px,pavg,grpsz=5, L=3,LL=0.8, Lavg = 3, P=1, Pavg=1, banned=0,inv = 0.6,volcost=0.1, Lcost=0.2, E=0.5, Eavg=0.5, Ecoef = 0.9){


p_no_vol <- (1-pavg)^(grpsz-banned-1) # prob that no one else volunteers

p_elected_vol <- sum(dbinom(0:(grpsz-banned-1),(grpsz-banned-1),pavg)*(1/(1:(grpsz-banned)))) #probability of being elected, given volunteering

L_return_ego <- ((LL + (L-LL)*inv)*grpsz - E)/grpsz # Leadership of ego
L_return_avg <- ((LL + (Lavg-LL)*inv)*grpsz - Eavg)/grpsz# Expected group Leadership

Ex <- (1-px)*p_no_vol*(LL+P) + # Return if no leader
      (1-px)*(1-p_no_vol)*(L_return_avg + P) + # Return if ego doesnt volunteer but someone else does
	  px*p_elected_vol*(L_return_ego + P - volcost - Lcost - inv*P + Ecoef*E) +   # Return if ego is leader
	  px*(1-p_elected_vol)*(L_return_avg + P - volcost) # Return if ego volunteers but isn't elected
	
return(Ex)
}
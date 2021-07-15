abdl <- function(pavg=0,grpsz=5, L=3,LL=0.8, Lavg = 3, P=1, Pavg=1, inv = 0.6,volcost=0.1, Lcost=0.2, E=0, Eavg=0, Ecoef = 0.9){

L_return_ego <- ((LL + (L-LL)*inv)*grpsz - E)/grpsz # Leadership of ego
L_return_avg <- ((LL + (Lavg-LL)*inv)*grpsz - Eavg)/grpsz# Expected group Leadership

ldr_stay <- L_return_ego  + P - Lcost - inv*P + Ecoef*E
ldr_gone <- (1-pavg)^(grpsz-1)*(LL+P) + (1-(1-pavg)^(grpsz-1))*(L_return_avg + P)
ldr_adv <- ldr_stay-ldr_gone
return(ldr_adv)
}

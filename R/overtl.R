overtl <- function(pavg=0,grpsz=5, L=3,LL=0.8, Lavg = 3, P=1, Pavg=1, inv = 0.6,volcost=0.1, Lcost=0.2, E=0, Eavg=0, Ecoef = 0.9){

L_return_ego <- ((LL + (L-LL)*inv)*grpsz - E)/grpsz # Leadership of ego
L_return_avg <- ((LL + (Lavg-LL)*inv)*grpsz - Eavg)/grpsz# Expected group Leadership

grp_stay <- L_return_ego + Pavg
grp_gone <- retl(px=pavg,pavg=pavg,grpsz=grpsz, banned=1,LL=LL,L=Lavg,Lavg=Lavg,P=Pavg,Pavg=Pavg,inv=inv,volcost=volcost, E=Eavg, Eavg=Eavg,Ecoef=Ecoef)
grp_adv <- grp_stay-grp_gone
return(grp_adv)
}

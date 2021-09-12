overtl <- function(pvol,pvolHiL=0,grpsz=5, L=3,LL=0.8, Lavg = 3, P=1, Pavg=1, inv = 0.6,volcost=0.1, Lcost=0.2, E=0, Eavg=0, Ecoef = 0.9,HiLprop = 0,HiL=4,PHiL= 0.5,Baseline=5){

  
  Lavg <- L*(1-HiLprop)+HiL*HiLprop
  Pavg <- P*(1-HiLprop)+PHiL*HiLprop
  volavg <- pvol*(1-HiLprop)+pvolHiL*HiLprop
  
  
  EHiL <- min((LL + (HiL-LL)*inv)*grpsz,E)
  E <- min((LL + (L-LL)*inv)*grpsz,E)
  Eavg <- min((LL + (Lavg-LL)*inv)*grpsz,E)
  
  HiL_return <- ((LL + (HiL-LL)*inv)*grpsz - EHiL)/grpsz + Baseline# Leadership of ego
  L_return <- ((LL + (L-LL)*inv)*grpsz - E)/grpsz + Baseline# Leadership of ego
  L_return_avg <-((LL + (Lavg-LL)*inv)*grpsz - Eavg)/grpsz + Baseline
  
  

grp_adv_HiL_stay <- HiL_grp_stay-grp_gone
grp_adv_LoL_stay <- LoL_grp_stay-grp_gone

return(grp_adv)
}

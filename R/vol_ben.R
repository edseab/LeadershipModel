vol_ben <- function(pvol,pvolHiL=0, Lgroup=c("LoL","HiL","Both"),grpsz=5, N=100, L = 3, LL=0.8, P=1,inv = 0.6,volcost=0.1, Lcost=0.2, E=0.5, Ecoef = 0.9, HiLprop = 0,HiL=4,PHiL= 0.5, Baseline=5, dataheavy=F){
  

  if (HiLprop>=1) stop ("HiLprop cannot be 1 or higher")
  Lgroup <- match.arg(Lgroup)
  p_same_group <- (grpsz-1)/(N-1)
  HiLprops <- HiLprop
  HiLbinom <- 1
  
  if(HiLprop>0){
    HiLprops <- (0:(grpsz-1))/(grpsz-1)
    HiLbinom <- dbinom(0:(grpsz-1),(grpsz-1),HiLprop)
    NHiL <- HiLprops*(grpsz-1)
    NLoL <- (1-HiLprops)*(grpsz-1)
  } 
  
  volavg <- pvol*(1-HiLprops)+pvolHiL*HiLprops
  volHiL <- HiLprops*pvolHiL/(HiLprops*pvolHiL+(1-HiLprops)*pvol)
  volHiL[is.na(volHiL)] <- 0 # for the cases where no one volunteers, the percent of HiL among volunteers is unecessary and drops out later, so we switch to 0 so the NA doesnt mess things up
  
  Lavg <- L*(1-HiLprops)+HiL*HiLprops
  L_vol_avg <- L*(1-volHiL)+HiL*volHiL
  Pavg <- P*(1-HiLprops)+PHiL*HiLprops
  P_vol_avg <- P*(1-volHiL)+PHiL*volHiL
  
  EHiL <- min((LL + (HiL-LL)*inv)*grpsz,E)
  Eactual <- min((LL + (L-LL)*inv)*grpsz,E)
  Eavg <- min((LL + (Lavg-LL)*inv)*grpsz,E)
  E_vol_avg <-Eactual*(1-volHiL)+EHiL*volHiL
  
  
  HiL_return <- ((LL + (HiL-LL)*inv)*grpsz - EHiL)/grpsz + Baseline # Leadership of ego
  L_return <- ((LL + (L-LL)*inv)*grpsz - Eactual)/grpsz  + Baseline# Leadership of ego
  L_return_avg <-((LL + (L_vol_avg-LL)*inv)*grpsz - E_vol_avg)/grpsz + Baseline
  
  p_no_vol <- (1-pvolHiL)^(grpsz*HiLprops)*(1-pvol)^(grpsz*(1-HiLprops))# prob that no one else volunteers
  
  if(HiLprop>0){
  p_elected_vol <- sapply(1:(length(HiLprops)),
                function(x) 1/(1 + sum(dbinom(0:(NLoL[x]),NLoL[x],pvol)*(0:NLoL[x])) + sum(dbinom(0:(NHiL[x]),NHiL[x],pvolHiL)*(0:NHiL[x]))))#probability of being elected, given volunteering
  }else {
    p_elected_vol <- sapply(pvol,function(x)sum(dbinom(0:(grpsz-1),(grpsz-1),x)*(1/(1:(grpsz)))))
  }
  
  
  other_group_expected_return <- HiLprop*pvolHiL*(p_elected_vol*(HiL_return + PHiL*(1-inv)-volcost-Lcost+Ecoef*EHiL) + (1-p_elected_vol)*(L_return_avg-volcost+PHiL)) +
    HiLprop*(1-pvolHiL)*(p_no_vol*(LL+PHiL + Baseline)+(1-p_no_vol)*(L_return_avg+PHiL)) +
    
    (1-HiLprop)*pvol*(p_elected_vol*(L_return + P*(1-inv)-volcost-Lcost+Ecoef*Eactual) + (1-p_elected_vol)*(L_return_avg-volcost+P)) +
    (1-HiLprop)*(1-pvol)*(p_no_vol*(LL+P + Baseline)+(1-p_no_vol)*(L_return_avg+P))
  
  if(HiLprop>0)other_group_expected_return <- sum(HiLbinom*(other_group_expected_return))

  if(dataheavy){
    HiLVol <- HiLprop*pvolHiL/(HiLprop*pvolHiL+(1-HiLprop)*pvol)
    pvolHiL_save<- pvolHiL
    pvol_save <- pvol
    p_no_vol_save <- p_no_vol
    if(is.na(HiLVol)){
      HiLvol <- 1 
      if(HiLprop==0){ pvol <- 0.01
      }else pvolHiL <- 0.01
      HiLVol <- HiLprop*pvolHiL/(HiLprop*pvolHiL+(1-HiLprop)*pvol)
      p_no_vol <- 0.99
      }
    Ldr_return <- HiLVol*(HiL_return + PHiL*(1-inv)-volcost-Lcost+Ecoef*EHiL) + (1-HiLVol)*(L_return + P*(1-inv)-volcost-Lcost+Ecoef*Eactual)
    Flwr_return <- sum(HiLbinom*(HiLprop*pvolHiL*(1-p_elected_vol)*(L_return_avg-volcost+PHiL) + 
          HiLprop*(1-pvolHiL)*(1-p_no_vol)*(L_return_avg+PHiL) +
      (1-HiLprop)*pvol*(1-p_elected_vol)*(L_return_avg + P - volcost) + 
      (1-HiLprop)*(1-pvol)*(1-p_no_vol)*(L_return_avg+P)
        ))/sum(HiLbinom*(HiLprop*pvolHiL*(1-p_elected_vol)+HiLprop*(1-pvolHiL)*(1-p_no_vol) + (1-HiLprop)*pvol*(1-p_elected_vol)+(1-HiLprop)*(1-pvol)*(1-p_no_vol)))

    total_P_avg <- (P*(1-HiLprop)+PHiL*HiLprop)
    total_L_avg <- (L*(1-HiLprop)+HiL*HiLprop)
    total_E_avg <- (Eactual*(1-HiLprop)+EHiL*HiLprop)
    
    NoLdr_return <- total_P_avg + LL + Baseline
  
    Rotating_Leader_return <- ((LL+inv*(total_L_avg-LL))*grpsz - total_E_avg*(Ecoef)+grpsz*Baseline + total_P_avg*(grpsz-1+inv) - Lcost)/grpsz
    pvolHiL <- pvolHiL_save
    pvol <- pvol_save
    p_no_vol <- p_no_vol_save
    }

  if (Lgroup %in% c("LoL","Both")){
  # returns to volunteering
  RetVol <- p_elected_vol*(L_return + P*(1-inv)-volcost-Lcost+Ecoef*Eactual)/(
                (1-p_same_group)*other_group_expected_return+
				p_same_group*(volavg*(L_return_avg + Pavg - volcost) + (1-volavg)*(L_return_avg + Pavg)))  + 
      
  (1-p_elected_vol)*  (L_return_avg-volcost+P)/(
    (1-p_same_group)*other_group_expected_return + 
      p_same_group*((L_return_avg + P_vol_avg - volcost - Lcost - inv*P_vol_avg + Ecoef*E_vol_avg)/(grpsz-1) + 
                                          ((volavg*(L_return_avg + Pavg - volcost) + (1-volavg)*(L_return_avg + Pavg))*(grpsz-2)/(grpsz-1))))

  
  if(HiLprop>0)RetVol <- sum(HiLbinom*(RetVol))
  
  RetNoVol <- p_no_vol*(LL+P + Baseline)/((1-p_same_group)*other_group_expected_return+p_same_group*(LL+Pavg + Baseline))  + 
    (1-p_no_vol)*(L_return_avg + P)/((1-p_same_group)*other_group_expected_return + p_same_group*
                                       ((L_return_avg + P_vol_avg - volcost - Lcost - inv*P_vol_avg + Ecoef*E_vol_avg)/(grpsz-1) + 
                                          ((volavg*(L_return_avg + Pavg - volcost) + (1-volavg)*(L_return_avg + Pavg))*(grpsz-2)/(grpsz-1)))
       )
    if(HiLprop>0)RetNoVol <- sum(HiLbinom*(RetNoVol))
    
  }
  
  if (Lgroup %in% c("HiL","Both")){
  # returns to volunteering
  RetVolHiL <- p_elected_vol*(HiL_return + PHiL*(1-inv)-volcost-Lcost+Ecoef*EHiL)/((1-p_same_group)*other_group_expected_return +
                                           p_same_group*(volavg*(L_return_avg + Pavg - volcost) + (1-volavg)*(L_return_avg + Pavg)))  + 
      
      (1-p_elected_vol)*  (L_return_avg-volcost+PHiL)/(
        (1-p_same_group)*other_group_expected_return + 
          p_same_group*((L_return_avg + P_vol_avg - volcost - Lcost - inv*P_vol_avg + Ecoef*E_vol_avg)/(grpsz-1) + 
		  (volavg*(L_return_avg + Pavg - volcost) + (1-volavg)*(L_return_avg + Pavg))*(grpsz-2)/(grpsz-1)))

  if(HiLprop>0)RetVolHiL <- sum(HiLbinom*(RetVolHiL))
  
  
  RetNoVolHiL <- p_no_vol*(LL+PHiL + Baseline)/((1-p_same_group)*other_group_expected_return+p_same_group*(LL+Pavg + Baseline))  + 
      (1-p_no_vol)*(L_return_avg + PHiL)/((1-p_same_group)*other_group_expected_return + p_same_group*
                                         ((L_return_avg + P_vol_avg - volcost - Lcost - inv*P_vol_avg + Ecoef*E_vol_avg)/(grpsz-1) + 
                                            ((volavg*(L_return_avg + Pavg - volcost) + (1-volavg)*(L_return_avg + Pavg))*(grpsz-2)/(grpsz-1)))
      )
    if(HiLprop>0)RetNoVolHiL <- sum(HiLbinom*(RetNoVolHiL))
    
  } 
  
      if(Lgroup=="LoL") output <- c(VolBen=(RetVol-RetNoVol))
      if(Lgroup=="HiL") output <-c(VolBenHiL= (RetVolHiL-RetNoVolHiL))
      if(Lgroup=="Both")output <- c(VolBen=(RetVol-RetNoVol),VolBenHiL=c(RetVolHiL-RetNoVolHiL))
  if (!dataheavy) {
    return(output)
}else 
  output_list <- c(as.list(output),list(AvgRet = other_group_expected_return,LoL_effective_E=Eactual,HiL_effective_E=EHiL,Ldr_return = Ldr_return,Flwr_return=Flwr_return,NoLdr_return=NoLdr_return,Rotating_Leader_return=Rotating_Leader_return))
  return(output_list)
}

vol_ben_HiL <- function(pvolHiL,pvol=0, grpsz=5, N=100, L = 3, LL=0.8, P=1,inv = 0.6,volcost=0.1, Lcost=0.2, E=0.5, Ecoef = 0.9, HiLprop = 0.5,HiL=4,PHiL= 0.5,Baseline=5){
  
  if (HiLprop %in% c(0,1)) stop ("HiLprop cannot be 0 or 1")
  
  p_same_group <- (grpsz-1)/(N-1)
  
  Lavg <- L*(1-HiLprop)+HiL*HiLprop
  Pavg <- P*(1-HiLprop)+PHiL*HiLprop
  volavg <- pvol*(1-HiLprop)+pvolHiL*HiLprop
  
  
  EHiL <- min((LL + (HiL-LL)*inv)*grpsz,E)
  E <- min((LL + (L-LL)*inv)*grpsz,E)
  Eavg <- min((LL + (Lavg-LL)*inv)*grpsz,E)
  
  HiL_return <- ((LL + (HiL-LL)*inv)*grpsz - EHiL)/grpsz + Baseline# Leadership of ego
  L_return <- ((LL + (L-LL)*inv)*grpsz - E)/grpsz + Baseline# Leadership of ego
  L_return_avg <-((LL + (Lavg-LL)*inv)*grpsz - Eavg)/grpsz + Baseline
  
  p_no_vol <- (1-volavg)^(grpsz-1) # prob that no one else volunteers
  
  p_elected_vol <- sapply(volavg,function(x)sum(dbinom(0:(grpsz-1),(grpsz-1),x)*(1/(1:(grpsz))))) #probability of being elected, given volunteering
  
  other_group_expected_return <- retl(px=volavg,pavg=volavg,grpsz=grpsz,LL=LL,L=Lavg,Lavg=Lavg,P=Pavg,Pavg=Pavg,inv=inv,volcost=volcost, Lcost=Lcost,E=Eavg, Eavg=Eavg,Ecoef=Ecoef)
  
  
  # Return to volunteering
    RetVolHiL <- p_elected_vol*(HiL_return + PHiL - volcost - Lcost - inv*PHiL + Ecoef*EHiL)/(
      (1-p_same_group)*other_group_expected_return + 
        p_same_group*(volavg*(HiL_return + Pavg - volcost) + (1-volavg)*(HiL_return + Pavg)))   +  # Relative return if elected
      
      (1-p_elected_vol)*(L_return_avg + PHiL - volcost)/(
        (1-p_same_group)*other_group_expected_return + 
          p_same_group*((L_return_avg + Pavg - volcost - Lcost - inv*Pavg + Ecoef*Eavg)/(grpsz-1) + 
                          ((volavg*(L_return_avg + Pavg - volcost) + (1-volavg)*(L_return_avg + Pavg))*(grpsz-2)/(grpsz-1))))  # Relative Return if not elected
    

    # Return to not volunteering
    
      RetNoVolHiL <- p_no_vol*(LL+PHiL + Baseline)/((1-p_same_group)*other_group_expected_return + p_same_group*(LL+Pavg) + Baseline) + #Return if no-one volunteers in the group
        (1-p_no_vol)*(L_return_avg + PHiL)/((1-p_same_group)*other_group_expected_return + 
                                              p_same_group*((L_return_avg + Pavg - volcost - Lcost - inv*Pavg + Ecoef*Eavg)/(grpsz-1) + 
                                                              ((volavg*(L_return_avg + Pavg - volcost) + (1-volavg)*(L_return_avg + Pavg))*(grpsz-2)/(grpsz-1)))) # Return if at least one other person volunteers in the group
    
  return(RetVolHiL-RetNoVolHiL)
      
      }
  
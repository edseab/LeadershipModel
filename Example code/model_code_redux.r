

checkfunc <- function(E,Ecoef,L,P,I,Cv,Cl,n,Ewarn=F,sol.save=F){
if(E-L*I*n>0){ 
E <- L*I*n
if(Ewarn)print(paste0("E was too big, replaced with ",E))
}

sol <- uniroot.all(function(v){


(E*Ecoef-P*I-Cl-Cv*(1+v*(n-1)))/(1+v*(n-1)) +(L*I-E/n)*(1-v)^(n-1)

}, interval=c(0,1))

if(length(sol)>1){
 if(Ewarn) print(paste0("multiple v_hat:",paste(sol,collapse=';')))
sol <- sol[which.max(sapply(sol,function(v)v*(L*I-E/n-v*Cv)))]
}
if(length(sol)==0) sol <- as.numeric(E*(Ecoef*n-1)/n + I*(L-P) - Cl - Cv> 0 ) # Equation 15
ret <- (1-(1-sol)^(n-1))*(L*I-E/n+sol*Cv)+P
ifelse(sol.save,return(c(wbar=ret,vhat=sol)),return(ret))
}

istarcheck <- function(E,Ecoef,L,P,Cv,Cl,n){


sol <- function(I,save.vhat=F){
I*checkfunc(E=E,Ecoef=Ecoef,L=L,P=P,Cv=Cv,Cl=Cl,n=n,I=I,sol.save=T)$vhat
}

Istar <- integrate(sol,0,1)
if(save.vhat){
vhat <- integrate(function(I)sol(I)/I,0,1)
return(c(Istar=Istar,vhat=vhat))
}else return(Istar)
}


optimalE$Estar_check <-apply(optimalE, 1, 
function(x){
Emax <- as.numeric(x['L'])*as.numeric(x['inv'])*as.numeric(x['grpsz'])

opt <- do.call(checkfunc,args=c(as.list(x[names(x) %in% names(formals(checkfunc))]), 
                                list(maximum=T,interval=c(0,Emax))))
opt <- optimize(checkfunc,maximum=T,interval=c(0,Emax),
                                               Ecoef=as.numeric(x['Ecoef']),
											   P=as.numeric(x['P']),
											   L=as.numeric(x['L']),
											   I=as.numeric(x['inv']),
											   Cv=as.numeric(x['volcost']),
											   Cl=as.numeric(x['Lcost']),
											   n=as.numeric(x['grpsz']))
Estar <- c(0,opt$maximum,Emax)[which.max(sapply(c(0,opt$maximum,Emax),checkfunc,
                                               Ecoef=as.numeric(x['Ecoef']),
											   P=as.numeric(x['P']),
											   L=as.numeric(x['L']),
											   I=as.numeric(x['inv']),
											   Cv=as.numeric(x['volcost']),
											   Cl=as.numeric(x['Lcost']),
											   n=as.numeric(x['grpsz'])))]
					
vol <- checkfunc(E=Estar,Ecoef=as.numeric(x['Ecoef']),
											   P=as.numeric(x['P']),
											   L=as.numeric(x['L']),
											   I=as.numeric(x['inv']),
											   Cv=as.numeric(x['volcost']),
											   Cl=as.numeric(x['Lcost']),
											   n=as.numeric(x['grpsz']), sol.save=T)['vhat']
return(unlist(Estar))
# return(c(Estar=Estar,vol))
}
)




optimalE$Istar_check <- apply(optimalEinv,1,
function(x){
Emax <- as.numeric(x['L'])*as.numeric(x['grpsz'])
Istar <- istarcheck
Istar <- optimize(function(x)checkfunc(E=x,I=istarcheck(E=x), istarcheck,maximum=T










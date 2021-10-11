

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

arguments <- list(Ecoef=as.numeric(x['Ecoef']),
											   P=as.numeric(x['P']),
											   L=as.numeric(x['L']),
											   I=as.numeric(x['inv']),
											   Cv=as.numeric(x['volcost']),
											   Cl=as.numeric(x['Lcost']),
											   n=as.numeric(x['grpsz']))
optimalE$Estar_check <-apply(optimalE, 1, 
function(x){
Emax <- as.numeric(x['L'])*as.numeric(x['inv'])*as.numeric(x['grpsz'])
opt <- do.call(optimize,args=c(list(f=checkfunc,maximum=T,interval=c(0,Emax)),
                            arguments)) 
 
Estar <- c(0,opt$maximum,Emax)[which.max(sapply(c(0,opt$maximum,Emax),function(x)do.call(checkfunc,args=c(list(E=x),arguments))))]
					
vol <- do.call(checkfunc,args=c(list(E=Estar,sol.save=T),arguments))['vhat']
return(unlist(Estar))
# return(c(Estar=Estar,vol))
}
)
optimalE$vhat_check <-apply(optimalE, 1, 
function(x){
Emax <- as.numeric(x['L'])*as.numeric(x['inv'])*as.numeric(x['grpsz'])
opt <- do.call(optimize,args=c(list(f=checkfunc,maximum=T,interval=c(0,Emax)),
                            arguments)) 
 
Estar <- c(0,opt$maximum,Emax)[which.max(sapply(c(0,opt$maximum,Emax),function(x)do.call(checkfunc,args=c(list(E=x),arguments))))]
					
vol <- do.call(checkfunc,args=c(list(E=Estar,sol.save=T),arguments))['vhat']
return(unlist(vol))
}
)
optimalE$finalcheck_G <- apply(optimalE, 1, 
function(x){
(x['Ecoef']/(1+x['vhat_check']*(x['grpsz']-1)) -1/x['grpsz'])*((1-x['vhat_check'])^(x['grpsz']-2))*((1+x['vhat_check']*(x['grpsz']-1))^2)/((1-x['vhat_check'])^(x['grpsz']-1)-1)
})
optimalE$finalcheck_ratio <- apply(optimalE, 1, 
function(x){
(x['Estar_check']*x['Ecoef']-x['P']*x['I']-x['Lcost'])/(x['Estar_check']-x['L']*x['I']*x['n'])
})

arguments$I <- NULL
for(i in 1:nrow(optimalEinv)){
Emax <- as.numeric(optimalEinv$L[i])*as.numeric(optimalEinv$grpsz[i])

Estar_check <- Emax
count <- 0
while(max_E-Estar_check$maximum<0.01 & count<=10){
max_E <- 0.9*max_E
Estar_check <- optimize(function(.){
istar <- do.call(istarcheck,args=c(list(E=.),arguments))
grp_ret <- do.call(checkfunc,args=c(list(E=.,I=istar),arguments))
return(grp_ret)
},interval=c(0,max_E),maximum=T)
count<-count+1
}

optimalEinv$Estar_check[i] <- Estar_check
optimalEinv$Istar_check[i] <- do.call(istarcheck,args=c(list(E=Estar_check),arguments))
optimalEinv$vhat[i] <- do.call(checkfunc,args=c(list(E=Estar_check,I=istar,sol.save=T),arguments))$vhat
optimalEinv$vhat_check[i] <- do.call(istarcheck,args=c(list(E=Estar_check,vhat=T),arguments))$vhat
progress(i, nrow(optimalEinv),increment=100000)
}









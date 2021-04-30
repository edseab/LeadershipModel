unique_permuts <- function(vector, progress=F, mirrors=T, surprise_compliment=F){
Var1 <- c()
Var2 <- c()

if(mirrors){
for(i in 1:(length(vector))){
Var1 <- c(Var1, rep(vector[i], (length(vector)-i+1)))
Var2 <- c(Var2, vector[(i):length(vector)])
if(progress)progress(i, length(vector))
}}

if(!mirrors){
for(i in 1:(length(vector)-1)){
Var1 <- c(Var1, rep(vector[i], (length(vector)-i)))
Var2 <- c(Var2, vector[(i+1):length(vector)])
if(progress)progress(i, length(vector))
}}
if(surprise_compliment) print (sample(c("You bring joy to everyone around you", "You are wonderful person","You are a pleasure to talk to","You are a very thoughtful human being"),1)) 
return(data.frame(Var1=Var1,Var2=Var2))
}
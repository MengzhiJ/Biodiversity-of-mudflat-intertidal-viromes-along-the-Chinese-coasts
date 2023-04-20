#This script was used to calculate RCbray 

#Reference: #Ning D, Deng Y, Tiedje J M, et al. A general framework for quantitatively assessing ecological stochasticity[J]. Proceedings of the National Academy of Sciences, 2019, 116(34): 16892-16898.
#Stegen J C, Lin X, Fredrickson J K, et al. Quantifying community assembly processes and identifying features that impose them[J]. The ISME journal, 2013, 7(11): 2069-2079.

#load packages
library(stats)
library(NST)
library(picante)
library(ape)
library(parallel)
library(vegan)

#calculate obs similarity
tax<-read.delim('I:/Tidal/R_data/Fig.5/mOTUs.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
tax<-t(tax) #site as row, OTU as column
Dij<-vegan::vegdist(tax,method = "bray")

#calculate null Disimilarity

#multi-threads
nworker=10
c1<-parallel::makeCluster(nworker,type="PSOCK")

#export global environment
clusterExport(c1,"tax",envir = environment())

#define null fun(random:k(k=1000)) 
#null model can adjust according to NST help.Also, you can use shuffle in picante packages.
fun.tax <-function(i){
  null.taxo<-NST::taxo.null(tax,sp.freq = "prop",samp.rich = "fix",abundance = "region") #site as row, OTU as column
  taxo<-vegan::vegdist(null.taxo,method = "bray")
  list<- c(taxo)
  return(list)
}

#random 1000 (k=1:1000)
list<- parLapply(c1, 1:1000, fun.tax)

#save null (OPTION)
write.csv(Gij,file ="I:/Tidal/R_data/Fig.5/list.csv", row.names = T,  quote =FALSE) 
write.csv(Dij,file ="I:/Tidal/R_data/Fig.5/Dij.csv", row.names = T,  quote =FALSE) 

#define (Dmax=1)
Gij<-as.matrix(as.data.frame(list))
Eij<-1-Gij
Cij<-1-Dij
Eij.mean=apply(Eij, 1, mean,na.rm=TRUE)
Gij.mean=apply(Gij, 1, mean,na.rm=TRUE)
GD=Gij.mean/cbind(Gij,Dij) # G/Gk and G/D
EC=Eij.mean/cbind(Eij,Cij) # E/Ek and E/C
EC[is.nan(EC)]=1
ECGD=EC
ECGD[which(EC>1)]=GD[which(EC>1)]

#calculate RC
obs=matrix(Dij,nrow=nrow(Gij),ncol=ncol(Gij))
a1=rowSums(Gij>obs,na.rm = TRUE)/ncol(Gij)
a2=rowSums(Gij==obs,na.rm = TRUE)/ncol(Gij)
rc=(0.5-(a1+(a2/2)))*2

#save RC
write.csv(rc,file ="I:/Tidal/R_data/Fig.5/RCbray.csv", row.names = T,  quote =FALSE) 



#This script was obtained from published journal: Ning D, Deng Y, Tiedje J M, et al. A general framework for quantitatively assessing ecological stochasticity[J]. Proceedings of the National Academy of Sciences, 2019, 116(34): 16892-16898.

wd="" # change to folder path in your computer, where the input files are saved
save.wd="" # folder path, where to save the output files
com.file="" # file path of the communty matrix, e.g. OTU table. Each row represents a taxon, and each column represents a sample. Thus, row names are taxon IDs, and column names are sample IDs.
group.file="" # treatment information table. Each row represents a sample.

rand.time=1000 # randomization time for null model analysis. usually set 1000 for real datasets. here use 20 for test.
prefix="Test"

library(vegan)
library(ape)
library(iCAMP)
library(parallel)
library(bigmemory)
library(NST) # need to be NST >=3.0.3
library(permute)
library(DirichletReg)

setwd(wd)
comm=t(read.delim(com.file, row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE))
group=read.delim(group.file, row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

samp.ck=NST::match.name(rn.list=list(comm=comm,group=group))
comm=samp.ck$comm
comm=comm[,colSums(comm)>0,drop=FALSE]
group=samp.ck$group

groupi=group[,1,drop=FALSE] # if you have multiple ways to group samples, select one way each time.
prefixi=paste0(prefix,".Treat") # re-define the prefix in output filenames, specific to the grouping type.
meta.groupi=NULL # if treatment and control are from different metacommunities, you may set meta.groupi=groupi

dist.method="bray" # "jaccard" and "bray" are preferred.

setwd(save.wd)
tnst=tNST(comm=comm, group=groupi, meta.group=meta.groupi, meta.com=NULL,
          dist.method=dist.method, abundance.weighted=TRUE, rand=rand.time,
          output.rand=TRUE, nworker=16, LB=FALSE, null.model="PF",dirichlet=F,
          between.group=FALSE, SES=TRUE, RC=TRUE)

save(tnst,file = paste0(prefixi,".tNST.rda")) # save tNST output in R data format
write.table(tnst$index.grp,file = paste0(prefixi,".tNST.summary.csv"), quote = FALSE,sep = ",")
write.table(tnst$index.pair.grp,file = paste0(prefixi,".tNST.pairwise.csv"),quote = FALSE,sep = ",")
write.table(tnst$index.pair,file = paste0(prefixi,".tNST.pair.csv"),quote = FALSE,sep = ",")

tnstbt=nst.boot(nst.result=tnst, group=groupi, rand=rand.time, trace=TRUE,
                two.tail=T, out.detail=TRUE, between.group=FALSE, nworker=8)
save(tnstbt,file = paste0(prefixi,".tNST.boot.rda"))
write.table(tnstbt$summary,file = paste0(prefixi,".tNST.boot.summary.csv"), quote = FALSE,sep = ",")
write.table(tnstbt$compare,file = paste0(prefixi,".tNST.boot.compare.csv"), quote = FALSE,sep = ",")

tnstpaov=nst.panova(nst.result=tnst, group = groupi, rand = rand.time, trace = TRUE)
write.table(tnstpaov,file = paste0(prefixi,".tNST.PERMANOVA.csv"), quote = FALSE,sep = ",")

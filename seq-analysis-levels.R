#06-19-1 by mharandi
library(TraMineR)
library(cluster)
trajectory_data <- cast_pos_L1_sustained
trajectory_data.labels <- c( "classify", "communal", "exploring" ,"learning" ,"personal", "socialize", "view discussions")
trajectory_data.scodes <- c("CL", "CM", "EX" , "LR", "PR", "SC", "VD")
trajectory_data.seq <- seqdef(trajectory_data, 5:14, states=trajectory_data.scodes, labels=trajectory_data.labels, missing=NA, void="%", nr="*")

##Index plot of first 10 sequences of activities
seqiplot(trajectory_data.seq, withlegend=F)
seqlegend(trajectory_data.seq) 

##Frequency plot of 10 most frequent sequence of activities
seqfplot(trajectory_data.seq, pbarw=T, withlegend=F) 
seqlegend(trajectory_data.seq) 


#Compute the optimal matching distances using substitution costs based on transition rates observed in 
#the data and a 1 indel cost. The resulting distance matrix is stored in the dist.om1 object.
submat   <- seqsubm(trajectory_data.seq, method= "TRATE")
dist.om1 <- seqdist(trajectory_data.seq, method="OM", indel=1, sm=submat) 
clusterward1 <- agnes(dist.om1, diss=TRUE, method="ward")
cl1.3 <- cutree(clusterward1, k=3)
cl1.3fac <- factor(cl1.3, labels = c("Type 1", "Type 2", "Type 3")) 

# typology of the trajectories of volunteers
seqdplot(trajectory_data.seq, group=cl1.3fac)
seqlegend(trajectory_data.seq)

#The 10 most frequent sequences of each cluster of volunteers
seqfplot(trajectory_data.seq, group=cl1.3fac, pbarw=T)
seqlegend(trajectory_data.seq)

#Create event sequences from the state sequences.
trajectory_data.seqe <- seqecreate(trajectory_data.seq)
fsubseq <- seqefsub(trajectory_data.seqe, pmin.support = 0.05)

#The 10 most frequent event subsequences of volunteers with initial contributions:
#---event sequences are created from the state sequences
plot(fsubseq[1:10], col="cyan")

#Determine the most discriminating transitions between clusters and plot the frequencies 
#by cluster of the 5 first ones.
discr <- seqecmpgroup(fsubseq, group=cl1.3fac)

#The most discriminating transitions between clusters and plot the frequencies 
#by cluster of the first five ones. for volunteers with initial contributions
plot(discr[1:3])
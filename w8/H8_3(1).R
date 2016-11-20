#
# Dublin Weather Transition
#

P <- matrix(c(0.2,0.8,0.3,0.7),2,2,byrow=TRUE)

# Two-step transition
P%*%P

# Three-step transition
P%*%P%*%P

# Many-step transition
M <- P
for (i in 1:100)
{
	M <- M%*%P
}
M

# Stationary distribution (using eigenvector)
v <- eigen(t(P))$vector[,1]
pi_st <- v/sum(v)

#
# MSNBC Example
#

# Load necessary packages
library(ClickClust)
library(clickstream)

# Load clickstream data
data(msnbc323)

# Fit Markov chain model to clickstream data
fit <- fitMarkovChain(msnbc323)

# Extract the transition matrix
P<-t(fit@transitions[[1]])
P<-P[as.character(1:17),as.character(1:17)]
P<-as.matrix(P)
round(P,2)

# Plot transition matrix
library(seriation)
pimage(P)

# Find stationary distribution by starting in single state
# In this example (webpage type 1)

v<-rep(0,17)
v[1]<-1
v

for (i in 1:1000)
{
	v <- v%*%P
}
v

# Find stationary distribution using eigenvector
pi_st <- Re(eigen(t(P))$vector[,1])
pi_st <- pi_st/sum(pi_st)

#
# DNA Example
# The frequencies of successive pairs of nucleotides 
# in human preproglucagon gene
# 

DNA <- matrix(c(185,74,86,171,101,41,6,115,69,45,34,78,161,103,100,202),4,4,byrow=TRUE)
rownames(DNA) <- colnames(DNA) <- c("A","C","G","T")
DNA

# Transition matrix
P <- DNA/apply(DNA,1,sum)
P
pimage(P)

# Find stationary distribution using eigenvector
pi_st <- Re(eigen(t(P))$vector[,1])
pi_st <- pi_st/sum(pi_st)
names(pi_st)<-c("A","C","G","T")
pi_st

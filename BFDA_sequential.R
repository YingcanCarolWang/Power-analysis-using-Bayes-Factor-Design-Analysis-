#Bayes factor design analysis
library(BFDA)

# do a sequential design analysis with weak prior on effect size
s1 <- BFDA.sim(expected.ES=0.3,
               prior=list("Cauchy",list(prior.location=0, prior.scale=sqrt(2)/2)),
               n.min=20, stepsize=5, n.max=300, type="t.paired", design="sequential",
               alternative="greater", boundary = c(1/6, 6), B=10000, cores=4, verbose=FALSE)

saveRDS(s1,"U:/RDSmodels/Exp3/s1.rds")
s1 <- readRDS("U:/RDSmodels/Exp3/s1.rds")


s0 <- BFDA.sim(expected.ES=0,
               prior=list("Cauchy",list(prior.location=0, prior.scale=sqrt(2)/2)),
               n.min=20, stepsize=5, n.max=300, type="t.paired", design="sequential",
               alternative="greater", boundary = c(1/6, 6), B=10000, cores=4, verbose=FALSE)

saveRDS(s0,"U:/RDSmodels/Exp3/s0.rds")
s0 <- readRDS("U:/RDSmodels/Exp3/s0.rds")


# if no n.min and n.max is provided in the `BFDA.analyze` function,
# the values from the simulation are taken
BFDA.analyze(s1, design="sequential", n.min=20, n.max=300, boundary=6)

# outcome percentage
# 1 Studies terminating at n.max (n=300)       0.2%
# 2    Studies terminating at a boundary      99.8%
# 3       --> Terminating at H1 boundary      91.1%
# 4       --> Terminating at H0 boundary       8.7%
# 
# Of 0.2% of studies terminating at n.max (n=300):
#   0.1% showed evidence for H1 (BF > 3)
# 0.1% were inconclusive (3 > BF > 1/3)
# 0% showed evidence for H0 (BF < 1/3)
# 
# Average sample number (ASN) at stopping point (both boundary hits and n.max): n = 67
# 
# Sample number quantiles (50/80/90/95%) at stopping point:
#   50% 80% 90% 95% 
#   50 100 135 165 


BFDA.analyze(s0, design="sequential", n.min=20, n.max=300, boundary=6)

# outcome percentage
# 1 Studies terminating at n.max (n=300)       2.5%
# 2    Studies terminating at a boundary      97.5%
# 3       --> Terminating at H1 boundary       3.8%
# 4       --> Terminating at H0 boundary      93.7%
# 
# Of 2.5% of studies terminating at n.max (n=300):
#   0.1% showed evidence for H1 (BF > 3)
# 1.6% were inconclusive (3 > BF > 1/3)
# 0.9% showed evidence for H0 (BF < 1/3)
# 
# Average sample number (ASN) at stopping point (both boundary hits and n.max): n = 58
# 
# Sample number quantiles (50/80/90/95%) at stopping point:
#   50% 80% 90% 95% 
#   30  75 126 200 


plot(s1, n.min=20, n.max=110, boundary=c(1/6,6))

plot(s0, n.min=20, n.max=75, boundary=c(1/6,6))



#with a informed prior on effect size
s1_informed <- BFDA.sim(expected.ES=0.3,
                        prior = list("normal",list(prior.mean = 0.3, prior.variance = 0.01)),
                        n.min=20, stepsize=5, n.max=300, type="t.paired", design="sequential",
                        alternative="greater", boundary = c(1/6, 6), B=10000, cores=4, verbose=FALSE)

saveRDS(s1_informed,"U:/RDSmodels/Exp3/s1_informed.rds")
s1_informed <- readRDS("U:/RDSmodels/Exp3/s1_informed.rds")


s0_informed <- BFDA.sim(expected.ES=0,
                        prior = list("normal",list(prior.mean = 0.3, prior.variance = 0.01)),
                        n.min=20, stepsize=5, n.max=300, type="t.paired", design="sequential",
                        alternative="greater", boundary = c(1/6, 6), B=10000, cores=4, verbose=FALSE)

saveRDS(s0_informed,"U:/RDSmodels/Exp3/s0_informed.rds")
s0_informed <- readRDS("U:/RDSmodels/Exp3/s0_informed.rds")


BFDA.analyze(s1_informed, design="sequential", n.min=20, n.max=300, boundary=6)

# outcome percentage
# 1 Studies terminating at n.max (n=300)         0%
# 2    Studies terminating at a boundary       100%
# 3       --> Terminating at H1 boundary      94.4%
# 4       --> Terminating at H0 boundary       5.5%
# 
# Of 0% of studies terminating at n.max (n=300):
#   0% showed evidence for H1 (BF > 3)
# 0% were inconclusive (3 > BF > 1/3)
# 0% showed evidence for H0 (BF < 1/3)
# 
# Average sample number (ASN) at stopping point (both boundary hits and n.max): n = 49
# 
# Sample number quantiles (50/80/90/95%) at stopping point:
#   50% 80% 90% 95% 
#   35  70  95 120 

BFDA.analyze(s0_informed, design="sequential", n.min=20, n.max=300, boundary=6)
# outcome percentage
# 1 Studies terminating at n.max (n=300)       0.6%
# 2    Studies terminating at a boundary      99.4%
# 3       --> Terminating at H1 boundary       9.6%
# 4       --> Terminating at H0 boundary      89.7%
# 
# Of 0.6% of studies terminating at n.max (n=300):
#   0% showed evidence for H1 (BF > 3)
# 0.5% were inconclusive (3 > BF > 1/3)
# 0.2% showed evidence for H0 (BF < 1/3)
# 
# Average sample number (ASN) at stopping point (both boundary hits and n.max): n = 56
# 
# Sample number quantiles (50/80/90/95%) at stopping point:
#   50% 80% 90% 95% 
#   40  80 110 150 

plot(s1_informed, n.min=20, n.max=70, boundary=c(1/6,6))

plot(s0_informed, n.min=20, n.max=80, boundary=c(1/6,6))








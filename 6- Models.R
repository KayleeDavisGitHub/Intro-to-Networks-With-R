##
##    Simple Network Models
##

# Load Packages
library(igraph)
library(igraphdata)

g <- sample_gnp(n=20, p=0.2)
summary(g)
## IGRAPH 08b3417 U--- 20 36 -- Erdos renyi (gnp) graph
## + attr: name (g/c), type (g/c), loops (g/l), p (g/n)


# Plot two random networks in the same figure
# mfrow is the number of plots, mar is the margin around each plot in the order B,L,T,R
par(mfrow=c(1,2), mar=c(0,1,2,1))
plot(sample_gnp(n=20, p=0.2), main="First random network")
plot(sample_gnp(n=20, p=0.2), main="Second random network")

# Small random network
graph.density(g)
## [1] 0.1894737
# Larger random network
gL <- sample_gnp(n=2000, p=0.2)
graph.density(gL)
## [1] 0.1997669

gm <- sample_gnm(n=2000, m=(2000*1999*0.2) / 2)
summary(gm)
## IGRAPH 963eab8 U--- 2000 399800 -- Erdos renyi (gnm) graph
## + attr: name (g/c), type (g/c), loops (g/l), m (g/n)
graph.density(gm)
## [1] 0.2

average.path.length(gL)
## [1] 1.800233
transitivity(gL, type='global')
## [1] 0.1999701

sum(transitivity(gL, type='local'))/vcount(gL)
## [1] 0.1999738



## The Watts and Strogatz small world model

sw0 <- sample_smallworld(dim=1, size=20, nei=2, p=0.0)
sw002 <- sample_smallworld(dim=1, size=20, nei=2, p=0.02)
sw01 <- sample_smallworld(dim=1, size=20, nei=2, p=0.1)
sw1 <- sample_smallworld(dim=1, size=20, nei=2, p=1)

# Plot four networks in the same figure
# mfrow is the number of plots, mar is the marging around each plot in the order B,L,T,R
par(mfrow=c(2,2), mar=c(2,0,1,0))
plot(sw0, vertex.label=NA, main="p = 0")
plot(sw002, vertex.label=NA, main="p = 0.02")
plot(sw01, vertex.label=NA, main="p = 0.1")
plot(sw1, vertex.label=NA, main="p = 1")


# We are going to have 10 replications for each of the p-values below.
# This will allow us to average over random fluctuations and observe the trends better.
p_vec <- rep(c(0.01, 0.02, 0.04, 0.07, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.75, 1.0), each=10)

# First, we will create small world networks for each of the p-values
gs <- lapply(p_vec, function(p)
     sample_smallworld(dim=1, size=1000, nei=2, p=p))

# Then, we will estimate the average path length and the transitivity
g_apl <- sapply(gs, average.path.length)
g_tran <- sapply(gs, transitivity)

# Now, we will plot them with a log scale for the x-axis
# and fit a spline to show the general trend
par(mfrow=c(1,2), mar=c(4,4,1,2))

plot(p_vec, g_apl, col='grey', log='x', xlab='p', ylab='Average path length')
smoothing_apl <- smooth.spline(p_vec, g_apl)
lines(smoothing_apl)

plot(p_vec, g_tran, col='grey', log='x', xlab='p', ylab='Transitivity')
smoothing_tran <- smooth.spline(p_vec, g_tran)
lines(smoothing_tran)


## Comparing empirical networks to network models

data("enron")
summary(enron)
## IGRAPH 64ec693 D--- 184 125409 -- Enron email network
## + attr: LDC_names (g/c), LDC_desc (g/c), name (g/c), Citation
## | (g/c), Email (v/c), Name (v/c), Note (v/c), Time (e/c),
## | Reciptype (e/c), Topic (e/n), LDC_topic (e/n)
graph.density(enron)
## [1] 3.72443
# Convert to undirected network
enron_s <- as.undirected(enron, mode="collapse")
summary(enron_s)
## IGRAPH 8cc2680 U--- 184 2809 -- Enron email network
## + attr: LDC_names (g/c), LDC_desc (g/c), name (g/c), Citation
## | (g/c), Email (v/c), Name (v/c), Note (v/c)
graph.density(enron_s)
## [1] 0.1668449
g_ran <- lapply(rep(1, 100), function(x)
     sample_gnp(n=184, p=0.1668449))
g_ran_apl <- sapply(g_ran, average.path.length)
g_ran_acc <- sapply(g_ran, transitivity)

res_table <- data.frame(c('Erdos-Renyi', 'Enron'),
                        c(mean(g_ran_apl), average.path.length(enron_s)),
                        c(mean(g_ran_acc), transitivity(enron_s)))
colnames(res_table) <- c('Name', 'Average path length', 'Transitivity')
res_table
##          Name Average path length Transitivity
## 1 Erdos-Renyi            1.838150    0.1669192
## 2       Enron            2.085787    0.3725138


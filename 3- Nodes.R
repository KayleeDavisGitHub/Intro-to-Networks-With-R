##
##    Properties of Nodes, Structural Equivalence, Roles, Positions
##            (this ramps up the difficulty quite a bit)

set.seed(123)

# List all packages:
packages <- c("NetCluster", "blockmodeling", "gplots", "igraph")

# Load all packages in one function:
lapply(packages, library, character.only = TRUE)
#  lapply(object, function, ...)  # For "list" apply... nifty tool!

# swi <- read.csv("swn.csv", header = TRUE, row.names = 1)
# header = TRUE uses the first row as the column names;
#  row.names = 1 uses the first column as the row names
#    (if we put row.names = 2, it would use the second column, etc.)

## I couldn't find the .csv... so I just took the data from the source
# https://networkdata.ics.uci.edu/netdata/html/davis.html
#  and hit "download" - open that r.data with Rstudio and it'll put it in your global env.

plot(davis) # just checking
swi <- as.matrix(davis) # Make incidence matrix - and put as igraph network
swn <- graph.incidence(swi) # This specifically makes a bipartite igraph from a matrix.
swn # UN-B ; 32 89
plot(swn)

swpr <- bipartite.projection(swn) # This makes the graph two one-mode networks
# What is this? - instead of just 1:1 node relationships there are two overlapping networks
#  and the realtionships we care about are between people of the other network.
#   so now our edges are weighted, and igraph strangely calls this a nonbipartite graph? Why?
#     Well now we don't really have one anymore! Instead we transfered the bipartide relationship
#     To a weighted relationship using that projection function. - Weights are number of shared
#     Event interactions. For more, check out this cool illistration:
#   https://en.wikipedia.org/wiki/Bipartite_network_projection#/media/File:Bipartite_network_projection.png

plot(swpr$proj2, # Note two proj's we get from that projection, via X or Y projection weights
     edge.width = E(swpr$proj2)$weight)
cor(swi) # Correlation of our matrix object to see structural position relationship. -1 : 1 (weights)
# Note that Flora and Oliva are structurally similar here and elsewhere

#  Going beyond Pearson correlation to see structural simularity:
# dissimularity measure (-1 to cor).
as.dist(1-cor(swi), upper=TRUE) # Upper = T just prints both sides of diag. Doesn't effect analysis
# We want to move away from just comparing two individuals, but instead find entire clusters
#  of attendants that exibit similar behavior. And then be able to analyize that more interesting
#  result. But to do this, we have to find how nodes are disimilar, so this data is more
#  valuable to find these clusters! Note we have a wider range now because with the -1
#  we could have a 1 - a negative -1, or 1+1 (1- -1).

swdend <- hclust(as.dist(1-cor(swi))) # Standard clustering tool
plot(swdend) # So how do we read this Dendrogram?
# Height is our value, and as we parse that value down, it's grouped into values.
#  Similar values become grouped under other values.
#  So, read from the top down, initial seperation was around the 1.0 mark, developing two
#  clusters, which have unique values apart from one another. as more and more nodes find
#  simularity. For more:
#  https://www.displayr.com/what-is-dendrogram/#:~:text=How%20to%20read%20a%20dendrogram,objects%20are%20A%20and%20B.


# Here is a heatmap with the same viz on each axis
heatmap.2(as.matrix(as.dist(1-cor(swi))),
          trace="none",
          revC=TRUE)

cutree(swdend, k=2) # here we are telling R where we should cut our clusters at.
#  THEORY and your interpretation of the data drives this decision, a dendrogram will not
#  tell you where to cut your clusters.

# So we identified two groups. Let's graph each of these to show our new network:
plot(swpr$proj2, edge.width=E(swpr$proj2)$weight, vertex.color=cutree(swdend, k=2))


## Let's do the same analysis for a one-mode network.
# Let's just ignore this package and data coding here for now... Copy and Pasted
library(ergm)
data("florentine")
require(intergraph)
require(igraph) # I'm adding igraph back in here to override other packages that might have similarly named functions

# Igraph objects, thanks to intergraph package (as seen before)
flo_m <- asIgraph(flomarriage)
flo_b <- asIgraph(flobusiness)

# This sets a default name, and fixes an issue with unnamed nodes:
#   (Professor discovered this error by first not doing this code:)
V(flo_m)$name <- V(flo_m)$vertex.names
V(flo_b)$name <- V(flo_b)$vertex.names

# Stacking our adjacency matricies on top of one another by row bind (rbind())
#  Why? To consider multiple relationship types at the same time.
flo_both <- rbind(as.matrix(get.adjacency(flo_m,names=TRUE)),
                  as.matrix(get.adjacency(flo_b,names=TRUE)))

flo_dist <- as.dist(1-cor(flo_both))
## Warning in cor(flo_both): the standard deviation is zero
flo_dist

flo_dist[is.na(flo_dist)==TRUE]<-2 # Fixes NA error by reassigning to number 2

## With NetCluster
flo_dend <- hclust(flo_dist)
plot(flo_dend)


## With gplots
heatmap.2(as.matrix(flo_dist),trace="none",revC=TRUE)
plot(flo_m,vertex.color=cutree(flo_dend, k=4))
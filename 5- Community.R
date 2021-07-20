##
##   Assortativity and Community Structure
##

# We'll use Zachary's karate club data again:
library(igraph)
library(igraphdata)
library(ape)
## this package will help us easily plot the dendrograms for community detection
#    algorithms that use hierarchical clustering
library(Matrix)
## this package gives us the image() function for quick visualizing adjacency matrices



data("karate")
summary(karate) #karate$Citation   is attached to this data remember


frlay <- layout_with_fr(karate) # preset coordinates generated for us...
## this will ensure that each time we plot the network, the location of the nodes stays constant
plot(karate, edge.width = E(karate)$weight, layout = frlay)



## Components
#   Networks are made up of components, and isolated individuals.
#  You can identify the components of a graph with the decompose.graph() function.
#  This gives a potentially long list of each component.
#  So, table(sapply(decompose.graph(yournetwork),vcount)) will give you a quick summary
#       of the size and number of components.

is_connected(karate)  # There is only one component within this network.

## Cores (k)
#   We could peel away layers of a network, by those who are weekly laching on (degree of 1)
#      all the way inwards (k-cores).
#   We could consider these layers individually, or all at once by seeing the greatest k-core for each node

coreness(karate)

colors <- rainbow(max(4))

plot(karate,
     vertex.color = colors[coreness(karate)],
     edge.width = E(karate)$weight,
     layout = frlay)
legend("topright",
       legend=c(1:4),
       col = colors,
       pch = 16,
       title = "Coreness")

## Cliques
#   These are the maximal connected subgraphs.
#   Note: we can relax the definition of this by including non-connected nodes (but at short distance "n")
#         igraph does not have this functionality (n=1).
# Visual definition of Clique: https://en.wikipedia.org/wiki/Clique_(graph_theory)#/media/File:VR_complex.svg

head(cliques(karate, min=3)) # here we excluded cliques below a certain size.
table(sapply(cliques(karate),length))
# This summarizes the full enumeration of cliques, but showing us the number of cliques of each size
largest_cliques(karate)


#### Community detection
#    There are so many ways to chop up a network into related nodes.
#    Generally speaking, we want subsets of nodes that are well connected among them selves, but few
#       connections from outsiders.
#    There are mountains of ways to parse this out mathematically, but with R things can be easier.

### Hierarchical clustering (see Session 4 for some of this).

## Edge Betweenness
#   Calculate betweenness (we did this in centrality session) of each edge, remove the edge
#      (so divide the network along that edge) with the highest betweeness,
#      recalculate betweenness, identify the new highest edge... repeat.
#   Note:  edges with weights get interpreted as additional distance (this is not the case in our network!)
#          have to adjust this calculation to get the weights on the right emphasis.

eb <- cluster_edge_betweenness(karate, weights = 1/E(karate)$weight)
membership(eb)
sizes(eb)
# library(stats) -- used for some alternative plotting options in our dendrograms.
# ?dendPlot()
dendPlot(eb, mode = "phylo")
plot(eb, karate, edge.width = E(karate)$weight, layout = frlay)
# by putting the community object (here, "eb") in front of the graph object,
#    igraph knows to plot both. igraph then colors the nodes by their community membership,
#    draws bubbles around each community, and colors edges within communities black and between
#    communities red.


## Fast Greedy (Modularity)
#    Similar to "modularity" when we measured assorativity before, this can be used to partition communities.
#      chopping groups up by their assortatitivy scores. Done within, then between all nodes. meaning
#      there are more edges within groups and fewer between them -- then we find communities.
fg <- cluster_fast_greedy(karate, weights = E(karate)$weight)
sizes(fg)
dendPlot(fg, mode="phylo")
plot(fg, karate, edge.width = E(karate)$weight, layout = frlay)

## Walktrap
#   imagine doing many, many, short random walks across a network. You'll likely end up staying in a community
#   on each random walk since between-communities ties should be rare to find.
#   So we can use these random walks to find communities (on average) and use that measure
#   as yet another hierarchical clustering process!
#     [consider simulation, machine learning, and ways to take these random walks out of your hands]

wt <- cluster_walktrap(karate, weights = E(karate)$weight) ## uses edge weight by default
dendPlot(wt)
plot(wt, karate, edge.width = E(karate)$weight, layout = frlay)


### Other approaches:

## Louvain (aka Multilevel)
#    this one is also trying to quickly optimize modularity. Each node here is assigned to a different
#    community, then a node is moved to join one of its neighbors with wich it increases the modularity score.
#    This process of moving nodes around keeps going until modularity cannot be improved anymore.
#    [see Yang et al 2016] - this performs really well.
#  This doesn't classify as a heirarchical community deteticion because it is not strictly agglomerative.
#    (bottom up) compared to "divisive" top-down methodologies.
ml <- cluster_louvain(karate, weights = E(karate)$weight)
plot(ml, karate, edge.width = E(karate)$weight, layout = frlay)

## InfoMap
#    Improvement on walktrap - draws on information theory
im <- cluster_infomap(karate, e.weights=E(karate)$weight)
plot(im, karate, edge.width = E(karate)$weight, layout = frlay)

## Spinglass
#     Drawing from physics, and magnetism - a spin glass type of magnet.
#     Each node is similar to an atom, and we want to simulate the annealing process where we minimize energy
#     and this happens when nodes are grouped in communities with the same spin - than different spin groups.
sg <- cluster_spinglass(karate, weights=E(karate)$weight)
plot(sg, karate, edge.width = E(karate)$weight, layout = frlay)


### Which one do I use?!
#     often the "big picture" is the same, but often we'll find small changes between methods.
#     When publishing, it can be better to publish many different methods in a table or easily reportable
#     Format. (See Seminar notes for some more information on each of these methods)


### Graph Partitions and "Ground Truth"
#     So we can mathematically parse out groups - but how does this align with our theory, and other
#     data? How does this match with our homophily?

# First step: let's calculate assortativity
#      (normalized ---  -1 (disassortive) :  1(assortive) [0 = no evidence])

modularity(karate, V(karate)$Faction)
assortativity(karate, V(karate)$Faction)
#  So this assortativity measure is very high... and matches the karate club theory and our knowledge of it.
#    And we've already eye-balled all the graphs, and things seem to line up to these factions.

# In igraph we can compare() different membership vectors using nmi "normalized mutual information"
#    1 = perfect match!  0 = mutually uninformative of one another.

# Compare "Truth" (faction)  with method:
compare(V(karate)$Faction, eb, method = "nmi")
## [1] 0.8371695
compare(V(karate)$Faction, fg, method = "nmi")
## [1] 0.8255182
compare(V(karate)$Faction, wt, method = "nmi")
## [1] 0.6956222
compare(V(karate)$Faction, ml, method = "nmi")
## [1] 0.6872629
compare(V(karate)$Faction, im, method = "nmi")
## [1] 0.8255182
compare(V(karate)$Faction, sg, method = "nmi")
## [1] 0.6543404

# Table of membership:
table(V(karate)$Faction, membership(eb))

table(V(karate)$Faction, membership(fg))

table(V(karate)$Faction, membership(im))

#  So if our theory is strong in that these factions matter:
#     we see that edge betweenness does "best" with only one person assigned wrong faction.
#     with fast_greedy and infomap coming close behind in faction membership detection.

## we can also compare different partitionings based on different community detection algorithms.
compare(eb, sg, method = "nmi")
## [1] 0.5622443
compare(wt, sg, method = "nmi")
## [1] 0.8953659
compare(fg, im, method = "nmi")
## we can see that these two community detection algorithms produce exactly the same partitioning of the graph
## [1] 1
table(membership(fg),membership(im))

## Stochastic Block Models.
#    Just create a model to calculate the probablity of ties within each group (controlling for variables).
#    (Have to swap packages out of igraph and data out of igraph)
require(intergraph)
detach(package:igraph)
require(sna)
knet <- asNetwork(karate)
## this is the function from intergraph that takes a igraph graph object and
#      makes it into a statnet graph object

# ?blockmodel()
blockmodel(knet, ec = knet %v% "Faction")

# equiviliancy classes between the "blocks" - probability of ties in blocks

##            Block 1    Block 2
## Block 1 0.27500000 0.03472222
## Block 2 0.03472222 0.22875817
#   We can see that a much higher probability of a tie are within each faction than between them.\

kbm <- blockmodel(knet, ec = knet %v% "Faction")$block.model ## saving these results


# (swap packages back)
detach(package:sna)
detach(package:intergraph)
require(igraph)

#  BlockModels are mostly used for generative models of network formation.
#    Now we'll use those block model results:
?sample_sbm()
g_kar <- sample_sbm(34, pref.matrix = kbm, block.sizes = c(16,18), directed = FALSE)
assortativity(g_kar, c(rep(1,16), rep(2,18)))
## This compares pretty closely to the assortativity of the real karate network that we calculated above.
## [1] 0.7456716
par(mfrow=c(1,2))
plot(g_kar,vertex.color = c(rep(1,16), rep(2,18)), main = "Fabricated")
plot(karate, main = "Real")

image(g_kar[]) ## quick way to visualize the adjacency matrix.
#   Remember that graph[] calls the adjacency matrix


## to really make this comparison proper, we need to sort the adjacency matrix
#     (which we can get with just karate[]) by the Faction membership.
#     order(V(karate)$Faction) does the first bit by ordering the faction
#     membership vector into the two groups. Then we need to use that to order
#     the rows and columns in the matrix by that ordering, hence the further
#     order command.
image(karate[][order(as.vector(order(V(karate)$Faction))),])


# Reset the graphing parameter (thank me later)
par(mfrow=c(1,1))
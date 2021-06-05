##
## Intro to Network Types and Concepts
##

# Packages used:
library(igraph)
library(igraphdata)

# There will be some randomness here, so we'll set a seed to keep things consistent:
set.seed(8675309)

# Manually entering Network Data/ viewing networks -----------------------------

# Entering Edge-List data manually (works fine for now).
# ?graph()
g1 <- graph(edges=c(1,2,
                    1,3,
                    2,4,
                    3,1,
                    3,5,
                    5,1,
                    4,3))
# Let's view this:
g1
# So, Directional network, 5 nodes, 7 edges.
# Or more briefly:
summary(g1)
# Or using the default plot() function:
plot(g1)


# Another Edge-List dataset with names instead of numbers:
g2 <- graph(edges=c("Anand","Beth",
                    "Anand","Chris",
                    "Beth","Dieter",
                    "Chris","Sun",
                    "Chris","Ali",
                    "Beth","Ali",
                    "Ali","Sun",
                    "Sun","Beth",
                    "Fatima","Chris",
                    "Fatima","Sun"),
            directed=FALSE)
# Now we have no directionality on nodes.

plot(g2)
summary(g2) # Undirected, now named, 7 nodes, 10 edges.

# We can use functions to visualize our data another way (beyond edge-list)
# Adjacency-Matrix:
g1[]
## 5 x 5 sparse Matrix of class "dgCMatrix"

get.adjacency(g2)
## 7 x 7 sparse Matrix of class "dgCMatrix"


# Similarly, we can visualize the Edges:
E(g1)
# or verticies:
V(g2)

# We can also pull up variables from this object: like $name
V(g2)$name

# So let's plug in more data on top of that:
V(g2)$gender <- c("M","F","M","M","F","M","F") # Here, we're assigning the gender for each vertex.

# Or we'll assign some arbitrary weights to the edges (notice the E(g2) now.)
E(g2)$count <- rep(1:2,5)
g2 <- set.vertex.attribute(g2, "age", value = c(20,26,19,34,22,30,21))
# Because we want R to save this addition to the network, when using the
#  set.vertex.attribute() function, we have to assign this again to the graph name (g2).

# With this new data added, let's summarize the graph:
summary(g2) # We see the new variables added, and how they are relevantly connected.

## Larger data -----------------------------------------------------------------

# A classical long-standing training dataset for students of Network Analysis has been
#  Zachary's Karate Club, some researchers even today test methods on this simple dataset.

data("karate")
summary(karate)

# What does this summary output mean?
#     - U means undirected (versus D for directed)
# - N means that the graph is named
# - W means weighted graph
# - 34 is the number of nodes
# - 78 is the number of edges
# - name (g/c), Citation (g/c), Author (g/c) are all graph attributes,
#      telling us about the source of the data.
# - name (v/c) and label (v/c) mean that name and label are vertex attributes that are characters
# - color (v/n) is a vertex attribute that’s numeric. (Here, the numbers call colors –
#      that’s what’s being used in the plot)
# - Faction (v/n) is a vertex attribute that’s numeric, and basically is the same content as color:
#      both distinguish the two factions that emerged within the club.
# - weight (e/n) means weight is an edge attribute and it’s numeric.

plot(karate) # Remember this is an undirected network - interpretations change when directed!
#  Here we have a bunch of students surrounding two faction leaders - the blue faction lead
#    by John "A" - and the orange faction lead by Mr. Hi ("H")

## With the data set up and discribed, let's run through the basic functions that can get you network stats.
vcount(karate) # Verticie (node) count

ecount(karate) # Edge count

?degree() # a summation of all edges linked to nodes.
# or, (undirected) - 2E/N;  (directed) E/N
mean(degree(karate)) # Average network degree
# [1] 4.588235      # "on average" we have 4.5 connections in the karate club

?graph.density() # This is (usually) just: (Actual Connections/Potential Connections)
graph.density(karate) # Density
# [1] 0.1390374    # This network is about 14% dense/100%

# Diameter gives you the longest distance and path:
?diameter()
diameter(karate) # It's diameter
# [1] 13    # The farthest nodes are 13 people apart.
get_diameter(karate) # Here we see the long-pathway
farthest_vertices(karate) # Here we see the people that are the farthest apart.


# Some graphical options:
plot(karate,
     edge.width = E(karate)$weight,
     vertex.size = degree(karate)*1.5,
     edge.arrow.size = 0.4,
     vertex.label.cex = 0.7)
# So in this graph we have weighted edges based on our data, the nodes are bigger
#   depending on their degree "importance"


## Bipartide Networks: ---------------------------------------------------------
# This is where you have the relationship between nodes be that they are connected
#   to another party. (I worked with Jim, and so did you, so we're connected)

# Seminar notes on this:
mem <- matrix(data = c(1,1,1,1,0,
                      0,1,1,0,0,
                      0,0,0,1,1),
              nrow = 3,
              byrow = TRUE)
# This is assigning the names first to the 3 rows, and then to the 5 columns
dimnames(mem) <- list(c("Group1","Group2","Group3"),
                      c("Y1","Y2","Y3","Y4","Y5"))
mem
##        Y1 Y2 Y3 Y4 Y5
## Group1  1  1  1  1  0
## Group2  0  1  1  0  0
## Group3  0  0  0  1  1

bg <- graph.incidence(mem)
bg  # We see the "B" there now, for Bipartide
## IGRAPH 6d6e710 UN-B 8 8 --
## + attr: type (v/l), name (v/c)
## + edges from 6d6e710 (vertex names):
## [1] Group1--Y1 Group1--Y2 Group1--Y3 Group1--Y4 Group2--Y2 Group2--Y3
## [7] Group3--Y4 Group3--Y5
plot(bg)

pr <- bipartite.projection(bg)
pr
## $proj1
## IGRAPH 749b7be UNW- 3 2 --
## + attr: name (v/c), weight (e/n)
## + edges from 749b7be (vertex names):
## [1] Group1--Group2 Group1--Group3
##
## $proj2
## IGRAPH fbde6d7 UNW- 5 7 --
## + attr: name (v/c), weight (e/n)
## + edges from fbde6d7 (vertex names):
## [1] Y1--Y2 Y1--Y3 Y1--Y4 Y2--Y3 Y2--Y4 Y3--Y4 Y4--Y5
plot(pr$proj1, edge.width = E(pr$proj1)$weight)
plot(pr$proj2, edge.width = E(pr$proj2)$weight)


get.adjacency(pr$proj1, sparse = FALSE, attr = "weight")
##        Group1 Group2 Group3
## Group1      0      2      1
## Group2      2      0      0
## Group3      1      0      0
get.adjacency(pr$proj2, sparse = FALSE, attr = "weight")
##    Y1 Y2 Y3 Y4 Y5
## Y1  0  1  1  1  0
## Y2  1  0  2  1  0
## Y3  1  2  0  1  0
## Y4  1  1  1  0  1
## Y5  0  0  0  1  0


aff1 <- mem %*% t(mem)
diag(aff1) <- 0
# the diagonal doesn't really have important information for these,
#   as it's about self-affiliation. So, with this command, we change those values to 0.
aff1
##        Group1 Group2 Group3
## Group1      0      2      1
## Group2      2      0      0
## Group3      1      0      0
aff2 <- t(mem) %*% mem
diag(aff2) <- 0
aff2
##    Y1 Y2 Y3 Y4 Y5
## Y1  0  1  1  1  0
## Y2  1  0  2  1  0
## Y3  1  2  0  1  0
## Y4  1  1  1  0  1
## Y5  0  0  0  1  0

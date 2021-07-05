##
##   Session 5:   Diads and Triads
##

library(ergm)
data("faux.dixon.high")
detach(package:ergm)
detach(package:network)
require(intergraph)
require(igraph)
dixon <- asIgraph(faux.dixon.high)


# Network Basics:
summary(dixon)

# Plotting with color by grade. [How might we clean this code?]
plot(dixon,
     vertex.label=NA,
     edge.arrow.size=0.25,
     vertex.size=3,
     edge.width=0.5,
     vertex.color=V(dixon)$grade)
legend("topright",
       legend=7:12,
       pch=19,
       col=categorical_pal(8)[c(7,8,1,2,3,4)])


## Dyads:
#    Reciprocity:  both nodes point to one another.
#    Here we can get mutual, asymmetrical, and null counts:

dyad.census(dixon) # Note the $ indicator for each object.

reciprocity(dixon) # We can calculate this via dyad census (above) or through adjacency matrix.

# Dyad Census:
2*dyad_census(dixon)$mut/ecount(dixon)
# two times the mutual count to control for the directionality of edges (which we use as denominator


# Using the adjacency matrix
dam <- as.matrix(get.adjacency(dixon))

sum(dam %*% t(dam)) # [I've added matrix multiplier here for a teaching moment if needed]
## Element-wise multiplation of the adjacency matrix, giving us the full number of mutual edges
#    (equivalent to 2*dyad_census(dixon)$mut.)

sum(dam) ## The count of the total number of edges in the network (equivlaent to ecount(dixon).)

sum(dam*t(dam))/sum(dam)

# Note all three are the same:
reciprocity(dixon) # Simple function
2*dyad_census(dixon)$mut/ecount(dixon) # Dyad Census utilization
sum(dam*t(dam))/sum(dam) # Using adjacency matrix
# 37% chance that if Student A names Student B as their friend; Student B will have also named A.
#    More generally, we can say that 37% of edges are reciprocated.


# Or we can use an alternative reciprocity calculation:
reciprocity(dixon, mode="ratio")
dyad.census(dixon)$mut/(dyad.census(dixon)$mut + dyad.census(dixon)$asym)
# mutual connections / mutal + asym

# Homophily  - relationships among people given a variable
assortativity.nominal(dixon,V(dixon)$grade)
assortativity.nominal(dixon,V(dixon)$sex)
assortativity.nominal(dixon,factor(V(dixon)$race)) # Had to factorize race here
# .nominal is giving us group-wise assortativity, without that it would treat as continuous.
# postive number means more assortment (sorting) into like groups with the variable than non-variable.



## Triads
transitivity(dixon)
# 18% chance of two neighbors of a vertex are themselves connected.
#    put another way, 18% of all triads are closed triangles (triads)

# Social networks often have a high level of transitivity, compare this to a completely random graph:
#   of same size:
rand <- sample_gnm(vcount(dixon),ecount(dixon),directed=TRUE)
summary(rand)
transitivity(rand)



# Triad Census:
?triad.census() # This help page defines most of the below information:

census_labels <- c('003','012','102','021D','021U','021C','111D','111U',
                  '030T','030C','201','120D','120U','120C','210','300')
## this is the order in which the triads appear in the triad census output.
triad.census(dixon)

df <- data.frame(census_labels, triad.census(dixon))
## binding the two labels with the output will let us actually interpret this!
#
# So most common are unconnected triads, then triads with only one asymmetric edge...

triad.census(rand)




# Manual exploration of triad census and transitivitiy:
##  Notes:
y <- sum(triad.census(dixon)[4:16, ])
x <- sum(triad.census(dixon)[1:3, ])



y <-  2280642 + 172313 + 49433
x <- sum(1400,1113,2576,1396,1611,163,22,386,73,92,98,135,43)
x/y


?transitivity()
transitivity(dixon, isolates = "NaN")

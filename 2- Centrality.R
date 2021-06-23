##
## Session 3: Centrality - "Importance" and "Interconnectivity" indications
##

set.seed(1)
# So this data comes as a test dataset from the ergm package - loaded into igraph via intergraph.
#   ergm package data -->  intergraph -->  igraph (familiar package by now)

library(ergm)  # By the way, don't use require()... use library()..
#              #  for more on this:    https://yihui.org/en/2014/07/library-vs-require/
library(intergraph)
## This package lets you port network objects between igraph and the statnet
## suite of packages (of which ergm is a part).

data("faux.dixon.high")
## The statnet and igraph packages don't always play well together,
## so we need to "detach" the statnet packages
detach(package:ergm)
detach(package:network)
library(igraph)
dixon <- asIgraph(faux.dixon.high)

## Explore the Network (get used to this about every time you load data)
summary(dixon)
graph.density(dixon)  #  1.9% of ties that could exist do exist
plot(dixon)   # 9 isolate people with no reported friends.
plot(dixon, vertex.label=NA, edge.arrow.size=0.25, vertex.size=5, edge.width=.5)

# table() is a nice function that shows you each value for that variable
#   and the counts within each value
table(V(dixon)$grade)
table(V(dixon)$race) # B = Black, H = Hispanic, O = Other, W = White
table(V(dixon)$sex)

## Degree Centrality:
#   -  This is simply the number of connections one has
#     --  For directed networks:
#         -  in degrees (directed connections inwards)
#         -  out degrees (directed outwards)

# Igraph can print a whole vector of this data for us if we want:
degree_vector <- degree(dixon)
head(degree_vector)  # so this is the number of degrees based on the node number.
summary(degree_vector) # here we see the descriptive statistics for our degree measurement.

#  So recall this is a highschool dataset, what does this data mean then?

in_degree_vector <- degree(dixon, mode="in") # added argument we're now looking at in-degrees
summary(in_degree_vector)

# can use base R hist() fuction to quickly see distribution visually:
hist(in_degree_vector, breaks = 20)

out_degree_vector <- degree(dixon, mode="out") # now we're looking at outward degree centrality
summary(out_degree_vector)

hist(out_degree_vector, breaks = 20)
#  So what did this form of centrality really tell us? Long right skew, but about same in/out


## Eigenvector Centrality and PageRank Centrality:
#    - These measures not only capture the N connections - but the importance of those connections.
#  Just Remember:
#    - Use Eigenvector function --> undirected networks
#    - PageRank --> directed networks
#      (really just report a table with every centrality measure and let your readers decide)

eig <- evcent(dixon)$vector  # evcent() function does a lot, so just isolate $vector at end of it.
summary(eig)
hist(eig, breaks=20) # very similar graph to before.

pr <- page.rank(dixon)$vector
summary(pr)
hist(pr, breaks=20)


## Closeness Centrality
#   This measure is different in that is measures how far/close each node is to each other.
#   Could use the same mode = "in" and mode = "out" for this measure to better specify.

close_vector <- closeness(dixon) # look at the warning here - why do we think R yelled at us?
# How are we to measure relationship of nodes with isolated nodes?
#  Have to be careful about this, maybe take large component, maybe report both...

summary(close_vector)
hist(close_vector, breaks=20)
# Notice that our results are pretty questionable... and it's because of those isolates

# Instead of fixing the root problem (probably by taking the large component)
#    Let's just filter our graph by those over a set small value:
hist(close_vector[close_vector>0.00005], breaks=20)
#  This does good-enough to tell us that most students in the network are roughly as equal as
#     everyone else (x-axis is so small in comparison. + normal distribution)

## If we don't specify in/out degree - it defaults to "out" degree.
close_vector_in <- closeness(dixon, mode = "in")
close_vector_out <- closeness(dixon, mode = "out")
hist(close_vector, breaks=20)
hist(close_vector_in, breaks=20)
hist(close_vector_out, breaks=20)



## Betweenness Centrality:
#   -  This is the extent to which each node is connected to each other node in the network.
#      ( this is usually important in presumed information flow theories )
betweenness_vector <- betweenness(dixon)
summary(betweenness_vector)
hist(betweenness_vector, breaks=20)
#  So very skewed, similar to other methods. Let's actually compare all these methods directly:


## Comparing Centrality Measures:
cents <- data.frame(degree_vector, in_degree_vector, out_degree_vector,
                    betweenness_vector, close_vector, eig, pr)
plot(cents)  # So what the heck does this tell us?
#  well we can see graphically and using the correlation (cor()) function: that almost all graphs
#    are trending positive. That is, there is no shockingly different graph that raises our eyebrow
cor(cents)

# One observation though is that the intersection of PageRank (pr) and our in-degree measure
#   is very correlated, this -may- sugguest that students with many friends are also friends with
#    influential students (as defined as themselves having many connections.)
#    Of course here is where you as as the researcher need to prove that case via theory and data.


## Let's Visualize Centrality:

dlayout <- layout.fruchterman.reingold(dixon)
library(scales)  # for the rescale() function -- helps scale all centrality measures

## Plotting in-degree, with vertices showing grade
plot(dixon,
     vertex.color = V(dixon)$grade,
     vertex.size = rescale(in_degree_vector, c(2,9)),
     edge.arrow.size = 0.25,
     edge.width = 0.5,
     vertex.label = NA,
     layout = dlayout)
## A legend to help make sense of the colouring;
## take my word for it that these are the right colours for each grade!
legend("topright",
       legend = 7:12,
       pch = 19,
       col = categorical_pal(8)[c(7,8,1,2,3,4)])
#  So what does this tell us?
#    1. popular kids in every grade (clusering via color)
#    2.  9th grade seem to be very tightly packed, 11th grade very spread out.


##  PageRank graph:  # "cool kid" indication:
plot(dixon, vertex.color=V(dixon)$grade,
     vertex.size=rescale(pr, c(2,9)), edge.arrow.size=0.25,
     edge.width=0.5, vertex.label=NA, layout=dlayout)
legend("topright", legend=7:12, pch=19, col=categorical_pal(8)[c(7,8,1,2,3,4)])


## Betweenness Centrality:
plot(dixon, vertex.color=V(dixon)$grade, vertex.size=rescale(betweenness_vector, c(2,9)),
     edge.arrow.size=0.25, edge.width=0.5, vertex.label=NA, layout=dlayout)
legend("topright", legend=7:12, pch=19, col=categorical_pal(8)[c(7,8,1,2,3,4)])
#  This seems to focus on the 9th and 10th graders - bridge young and older students


## Closeness Centrality:
plot(dixon, vertex.color=V(dixon)$grade, vertex.size=rescale(close_vector, c(1,8)),
     edge.arrow.size=0.25, edge.width=0.5, vertex.label=NA, layout=dlayout)
legend("topright", legend=7:12, pch=19, col=categorical_pal(8)[c(7,8,1,2,3,4)])
#  and this measure is very indistingishable, no real outgoing edges.


# could have looked at not only grade - but race, gender, favorite school subject, sports...
#                                  Anything the researcher wants!

# Centrality is a very important metric, and often the cornerstone of most social science.
#  Think about what actions school admins could make with this data, or any other data!

Grade <- as.factor(V(dixon)$grade)

library(ggplot2) #I'm just using qplot here to force our graph a bit more
qplot(in_degree_vector, out_degree_vector, color=Grade)+
        facet_grid(~ as.factor(V(dixon)$race)) +
        geom_smooth(method = "lm")+
        theme_bw()

# Super quick graph, I know, but basically it reads that no matter the race
# we find that all grade groups have healthy in/outward degree centrality.
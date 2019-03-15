
# The package (www.igraph.org) is maintained by Gabor Csardi and Tamas Nepusz.
# We thanks for notes preparation Mateo Mangani, Sharon Kuhman, Michal Burdukiewicz and Katya Ognyanova

install.packages("igraph","dplyr", "ggplot2", "surveillance") 
library(ggplot2)
library(dplyr)
library(surveillance)


#  ------->> Intruduction --------  

v1 <- c(1, 5, 11, 33)       # Numeric vector, length 4
v2 <- c("hello","world")    # Character vector, length 2 (a vector of strings)

# Mathematical operations:
sum(v1)      # The sum of all elements
mean(v1)     # The average of all elements
sd(v1)       # The standard deviation


v1[3]             # third element of v1
v1[2:4]           # elements 2, 3, 4 of v1
v1[6:10] <- 6:10

m <- matrix(data=1, nrow=5, ncol=4)  # same matrix as above, 5x4, full of 1s
m <- matrix(1,5,4) 			             # same matrix as above
dim(m)                               # What are the dimensions of m?


plot(x=1:10, y=rep(5,10), pch=19, cex=5, col="dark red")



# ================  Networks in igraph ================

rm(list = ls()) # Remove all the objects we created so far.

library(igraph) # Load the igraph package


#  ------->> Create networks --------

g4 <- graph( edges=c(1,2, 2,3, 3,1), n=3, directed=F ) # an undirected graph with 3 edges
# The numbers are interpreted as vertex IDs, so the edges are 1-->2, 2-->3, 3-->1
plot(g4) # A simple plot of the network - we'll talk more about plots later


#  ------->> Edge, vertex, and network attributes --------

# Access vertices and edges:
E(g4) # The edges of the object
V(g4) # The vertices of the object


# You can examine the network matrix directly:
g4[]
g4[1,] 

# Add attributes to the network, vertices, or edges:
V(g4)$name # automatically generated when we created the network.
V(g4)$gender <- c("male", "male", "male", "male", "female", "female", "male")
E(g4)$type <- "email" # Edge attribute, assign "email" to all edges
E(g4)$weight <- 10    # Edge weight, setting all existing edges to 10

# Examine attributes
edge_attr(g4)
vertex_attr(g4)
graph_attr(g4)






# Full graph
fg <- make_full_graph(40)
plot(fg, vertex.size=10, vertex.label=NA)

# Star graph 
st <- make_star(40)
plot(st, vertex.size=10, vertex.label=NA) 

# Tree graph
tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=10, vertex.label=NA) 

# Ring graph
rn <- make_ring(40)
plot(rn, vertex.size=10, vertex.label=NA)

# Erdos-Renyi random graph 
# ('n' is number of nodes, 'm' is the number of edges)
er <- sample_gnm(n=100, m=40) 
plot(er, vertex.size=6, vertex.label=NA)  

# Watts-Strogatz small-world graph
# Creates a lattice with 'dim' dimensions of 'size' nodes each, and rewires edges 
# randomly with probability 'p'. You can allow 'loops' and 'multiple' edges.
# The neighborhood in which edges are connected is 'nei'.
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)
plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

# Barabasi-Albert preferential attachment model for scale-free graphs
# 'n' is number of nodes, 'power' is the power of attachment (1 is linear)
# 'm' is the number of edges added on each time step 
ba <-  sample_pa(n=100, power=1, m=1,  directed=F)
plot(ba, vertex.size=6, vertex.label=NA)

#igraph can also give you some notable historical graphs. For instance:
zach <- graph("Zachary") # the Zachary carate club
plot(zach, vertex.size=10, vertex.label=NA)



# ================ 3. Reading network data from files ================



# DATASET : matrix 
#p1[i,j]=skale*(hum*(V(net2)$lud[i]*V(net2)$lud[j])/(1+macierz[i,j])+pork*(V(net2)$swi[i]*V(net2)$swi[j])/(1+macierz[i,j])+fst*V(net2)$las[i]*V(net2)$las[j]/(1+macierz[i,j])^2)

p1

net=graph_from_adjacency_matrix(p1,  weighted=TRUE, mode="directed")

siec=graph_from_adjacency_matrix(mac,  weighted=TRUE, mode="directed")

V(siec)$names=lista$county

for (i in 1:length(a[,1])) {
  V(net)[a[i,2]]$chorzy=1
}
plot.igraph(net, vertex.size=0.01, vertex.label=V(siec)$names)



net.bg <- sample_pa(80, 1.2) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

# You can set the layout 

l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)

# igraph has a number of built-in layouts, including:

# Randomly placed vertices
l <- layout_randomly(net.bg)
plot(net.bg, layout=l)

# Circle layout
l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)

# 3D sphere layout
l <- layout_on_sphere(net.bg)
plot(net.bg, layout=l)

# The Fruchterman-Reingold force-directed algorithm 
# Nice but slow, most often used in graphs smaller than ~1000 vertices. 
l <- layout_with_fr(net.bg)
plot(net.bg, layout=l)

# You will also notice that the layout is not deterministic - different runs 
# will result in slightly different configurations. Saving the layout in l
# allows us to get the exact same result multiple times.
par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=l)
plot(net.bg, layout=l)
dev.off()

# ================ 6. Network and node descriptives ================


# Density
# The proportion of present edges from all possible ties.
edge_density(net, loops=F)
ecount(net)/(vcount(net)*(vcount(net)-1)) #for a directed network

# Reciprocity
# The proportion of reciprocated ties (for a directed network).
reciprocity(net)
dyad_census(net) # Mutual, asymmetric, and null node pairs
2*dyad_census(net)$mut/ecount(net) # Calculating reciprocity

# Transitivity
# global - ratio of triangles (direction disregarded) to connected triples
# local - ratio of triangles to connected triples each vertex is part of
transitivity(net, type="global")  # net is treated as an undirected network
transitivity(as.undirected(net, mode="collapse")) # same as above
transitivity(net, type="local")
triad_census(net) # for directed networks



# Diameter (longest geodesic distance)
# Note that edge weights are used by default, unless set to NA.
diameter(net, directed=F, weights=NA)
diameter(net, directed=F)
diam <- get_diameter(net, directed=T)
diam

# Note: vertex sequences asked to behave as a vector produce numeric index of nodes
class(diam)
as.vector(diam)


# Node degrees
# 'degree' has a mode of 'in' for in-degree, 'out' for out-degree,
# and 'all' or 'total' for total degree. 
deg <- degree(net, mode="all")
plot(net, vertex.size=deg*3)
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")

# Degree distribution
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")


# Centrality & centralization

# Centrality functions (vertex level) and centralization functions (graph level).
# The centralization functions return "res" - vertex centrality, "centralization", 
# and "theoretical_max" - maximum centralization score for a graph of that size.
# The centrality functions can run on a subset of nodes (set with the "vids" parameter)

# Degree (number of ties)
degree(net, mode="in")
centr_degree(net, mode="in", normalized=T)

# Closeness (centrality based on distance to others in the graph)
# Inverse of the node's average geodesic distance to others in the network
closeness(net, mode="all", weights=NA) 
centr_clo(net, mode="all", normalized=T) 

# Eigenvector (centrality proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph adjacency matrix
eigen_centrality(net, directed=T, weights=NA)
centr_eigen(net, directed=T, normalized=T) 

# Betweenness (centrality based on a broker position connecting others)
# (Number of geodesics that pass through the node or the edge)
betweenness(net, directed=T, weights=NA)
edge_betweenness(net, directed=T, weights=NA)
centr_betw(net, directed=T, normalized=T)



# Average path length 
# The mean of the shortest distance between each pair of nodes in the network 
# (in both directions for directed graphs). 
mean_distance(net, directed=F)
mean_distance(net, directed=T)

# We can also find the length of all shortest paths in the graph:
distances(net) # with edge weights
distances(net, weights=NA) # ignore weights

# We can extract the distances to a node or set of nodes we are interested in.
# Here we will get the distance of every media from the New York Times.
dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], to=V(net), weights=NA)

# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, 
     vertex.label.color="white")

# We can also find the shortest path between specific nodes.
# Say here between MSNBC and the New York Post:
news.path <- shortest_paths(net, 
                            from = V(net)[name=="bialostocki"], 
                            to  = V(net)[name=="bialski"],
                            output = "both") # both path nodes and edges



# ================ The ASF example ================
siec=graph_from_adjacency_matrix(mac,  weighted=TRUE, mode="directed")
V(siec)$names=lista$county
V(siec)$x=lista$x
V(siec)$y=lista$y



lay=matrix(data=c(V(siec)$x,V(siec)$y), nrow=18, ncol=2)

plot.igraph(siec, vertex.size=9, vertex.label=V(siec)$names, vertex.label.cex=0.4, layout=lay )
plot.igraph(siec, vertex.size=9, vertex.label=V(siec)$names, vertex.label.cex=0.7, edge.arrow.size=0.3, layout=lay )


p1[i,j]=skale*(hum*(V(net2)$lud[i]*V(net2)$lud[j])/(1+macierz[i,j])+pork*(V(net2)$swi[i]*V(net2)$swi[j])/(1+macierz[i,j])+fst*V(net2)$las[i]*V(net2)$las[j]/(1+macierz[i,j])^2)


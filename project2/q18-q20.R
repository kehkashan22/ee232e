library('igraph')
library('entropy')
library('infotheo')

#Q18
myfiles = list.files(path="gplus", pattern="*.circles")
count = 0
for (file in myfiles){
  file = paste("gplus", file, sep="/")
  if (length(readLines(file)) > 2){
    count = count + 1
  }
}
print(count)

#Q19 - indegree and outdegree plot for three nodes
node1 = "109327480479767108490"
node2 = "115625564993990145546"
node3 = "101373961279443806744"

options(scipen=999)
file1 = paste("gplus/", node1, ".edges", sep = "")
gplus_graph1 = read_graph(file1, format="ncol", directed=TRUE)
gplus_graph1 = gplus_graph1 + vertex(node1)
for (i in seq(1, vcount(gplus_graph1)-1,1)){
  gplus_graph1 = gplus_graph1 + edge(vcount(gplus_graph1), i)
}
in_degree_1 = degree(gplus_graph1, mode = "in")
plot(in_degree_1, main = "In-Degree Distribution of Network 1", ylab = "In Degree DIstribution")
OutDegree1 = degree(gplus_graph1, mode = "out")
plot(OutDegree1, main = "Out-Degree Distribution of Network 1", ylab = "Out Degree DIstribution")

options(scipen=999)
file2 = paste("gplus/", node2, ".edges", sep = "")
gplus_graph2 = read_graph(file2, format="ncol", directed=TRUE)
gplus_graph2 = gplus_graph2 + vertex(node2)
for (i in seq(1, vcount(gplus_graph2)-1,1)){
  gplus_graph2 = gplus_graph2 + edge(vcount(gplus_graph2), i)
}
InDegree2 = degree(gplus_graph2, mode = "in")
plot(InDegree2, main = "In-Degree Distribution of Network 2", ylab = "In Degree DIstribution")
OutDegree2 = degree(gplus_graph2, mode = "out")
plot(OutDegree2, main = "Out-Degree Distribution of Network 2", ylab = "Out Degree DIstribution")

options(scipen=999)
file3 = paste("gplus/", node3, ".edges", sep = "")
gplus_graph3 = read_graph(file3, format="ncol", directed=TRUE)
gplus_graph3 = gplus_graph3 + vertex(node3)
for (i in seq(1, vcount(gplus_graph3)-1,1)){
  gplus_graph3 = gplus_graph3 + edge(vcount(gplus_graph3), i)
}
InDegree3 = degree(gplus_graph3, mode = "in")
plot(InDegree3, main = "In-Degree Distribution of Network 3" , ylab = "In Degree DIstribution")
OutDegree3 = degree(gplus_graph3, mode = "out")
plot(OutDegree3, main = "Out-Degree Distribution of Network 3", ylab = "Out Degree DIstribution")


#Q20. Compare Modularity Scords & Plot Communities
wtc1 = cluster_walktrap(gplus_graph1)
modul1 = modularity(wtc1)
colors1 = rainbow(max(membership(wtc1)))
plot(wtc1, gplus_graph1, main = "Community Structure of Network 1", vertex.colors = colors1[membership(wtc1)], layout = layout.fruchterman.reingold,
     vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.1)


wtc2 = cluster_walktrap(gplus_graph2)
modul2 = modularity(wtc2)
colors2 = rainbow(max(membership(wtc2)))
plot(wtc2, gplus_graph2, main = "Community Structure of Network 2", vertex.colors = colors1[membership(wtc2)], layout = layout.fruchterman.reingold,
     vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.1)

wtc3 = cluster_walktrap(gplus_graph3)
modul3 = modularity(wtc3)
colors3 = rainbow(max(membership(wtc3)))
plot(wtc3, gplus_graph3, main = "Community Structure of Network 3", vertex.colors = colors1[membership(wtc3)], layout = layout.fruchterman.reingold,
     vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.1)


library(igraph)
#graph_1 <- graph( edges=c('A','S','A','T','A','Z','S','F','S','R','S','O','T','L','Z','O','F','B','R','P','R','C','L','M','B','G','B','U','P','B','C','P','M','D','U','H','D','C','H','E','H','V','V','I','I','N'), directed=F)
graph_1 <- graph( edges=c('A','B','A','C','B','D','C','D','C','F','C','L','D','E','F','J','F','K','L','M','E','G','J','K','J','I','K','M','G','H','I','H'), directed=F)
#graph_1<-graph(edges = C('A','B','A','C','B','D','C','D','C','F','C','L','D','E','F','J','F','K','L','M','E','G','J','K','J','I','K','M','G','H','I','H'),directed = F)
#E(graph_1)$weight <- c(36,61,31,32,31,80,52,122,112,102,43,36,45,32,20,40)
E(graph_1)$weight <- 1
E(graph_1)$color <- 'black'
V(graph_1)$color <- 'orange'
found=FALSE
initial <- 'A'
goal <- 'M'

open<-c(initial)
closed<-c()
cost<-0
while(TRUE){
  if(length(open)==0){
    break
  }
  n<-open[1]
  if(n==goal){
    found=TRUE
    V(graph_1)[open[1]]$color <- 'yellow'
    closed<-c(closed,n)
    break
  }
  V(graph_1)[open[1]]$color <- 'yellow'
  closed<-c(closed,open[1])
  
  open<-open[-1]
  adj<-adjacent_vertices(graph_1,n)
  adjlist<-c()
  for(i in 1:length(adj[[1]])){
    if(adj[[1]][i]$name %in% open || adj[[1]][i]$name %in% closed )
      next
    adjlist<-c(adjlist,adj[[1]][i]$name)
  }
  open<-c(adjlist,open)#for dfs
  #open<-c(open,adjlist)#for bfs
}
if(found)
  print(closed)

V(graph_1)[initial]$color <- 'red'
V(graph_1)[goal]$color <- 'red'


for(i in 2:length(closed)-1){
  ei <- get.edge.ids(graph_1, c(closed[i],closed[i+1]))
  cost<-cost+E(graph_1)[ei]$weight
  E(graph_1)[ei]$color<-'orange'
  print(E(graph_1)[ei])
  print(E(graph_1)[ei]$weight)
  print(E(graph_1)[ei]$color)
}
plot(graph_1)
print(cost)

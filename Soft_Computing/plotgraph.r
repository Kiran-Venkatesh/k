library(igraph)
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")

el <- matrix( c("foo", "bar", "bar", "foobar"), nc = 2, byrow = TRUE)
graph_from_edgelist(el)

graph_from_edgelist(cbind(1:10, c(2:10, 1)))



## Two rings
bfs(make_ring(10) %du% make_ring(10), root=1, "out",
    order=TRUE, rank=TRUE, father=TRUE, pred=TRUE,
    succ=TRUE, dist=TRUE)

## How to use a callback
f <- function(graph, data, extra) {
  print(data)
  FALSE
}
tmp <- bfs(make_ring(10) %du% make_ring(10), root=1, "out",
           callback=f)

## How to use a callback to stop the search
## We stop after visiting all vertices in the initial component
f <- function(graph, data, extra) {
  data['succ'] == -1
}
bfs(make_ring(10) %du% make_ring(10), root=1, callback=f)





## A graph with two separate trees
dfs(make_tree(10) %du% make_tree(10), root=1, "out",
    TRUE, TRUE, TRUE, TRUE)

## How to use a callback
f.in <- function(graph, data, extra) {
  cat("in:", paste(collapse=", ", data), "\n")
  FALSE
}
f.out <- function(graph, data, extra) {
  cat("out:", paste(collapse=", ", data), "\n")
  FALSE
}
tmp <- dfs(make_tree(10), root=1, "out",
           in.callback=f.in, out.callback=f.out)

## Terminate after the first component, using a callback
f.out <- function(graph, data, extra) {
  data['vid'] == 1
}
tmp <- dfs(make_tree(10) %du% make_tree(10), root=1,
           out.callback=f.out)



# plot communities in a graph
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6,11))
com <- cluster_spinglass(g, spins=5)
V(g)$color <- com$membership+1
g <- set_graph_attr(g, "layout", layout_with_kk(g))
plot(g, vertex.label.dist=1.5)


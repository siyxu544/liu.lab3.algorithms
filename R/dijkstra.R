#' Dijkstra Shortest Path
#'
#' @description
#' Dijkstra’s algorithm finds the shortest paths
#' from a single source vertex to all other vertices
#' in a weighted graph with non-negative edge weights.
#' @param graph A data.frame with column v1,v2,w
#' @param init_node A numeric scalar start node that must exist in the graph.
#' @return Numeric vector of distances from init_node to all nodes.
#' @examples
#' wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'            v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'            w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}
#' @export


dijkstra <- function(graph, init_node) {
  stopifnot(is.data.frame(graph),
            all(c("v1","v2","w") %in% names(graph)),
            is.numeric(init_node),
            length(init_node) == 1L)
  v1 <- graph$v1; v2 <- graph$v2; w <- graph$w
  stopifnot(is.numeric(v1), is.numeric(v2), is.numeric(w), all(!is.na(w)), all(w >= 0))

  vv <- c(v1, v2) # get all nodes in the graph
  nodes <- sort(unique(vv[!is.na(vv)])) # remove duplicates and sort

  if (!(init_node %in% nodes)) {
    stop("'init_node' must exist in the graph") # check that the start node exists in the graph
  }


  dist <- rep(Inf, length(nodes)); # create a distance vector initialized with Inf
  names(dist) <- as.character(nodes) # set node IDs (as character) as names of dist
  dist[as.character(init_node)] <- 0 # set the distance of the start node to 0

  # logical(n) creates a logical vector of length n, default values are FALSE
  visited <- rep(FALSE, length(nodes))
  names(visited) <- as.character(nodes)

  while (TRUE) {
    cand <- which(!visited) # indices of unvisited nodes
    if (!length(cand))
      break

    # find the unvisited node with the smallest distance
    # m is a character string
    m <- names(dist)[cand][which.min(dist[cand])]
    if (is.infinite(dist[m]))
      break # remaining nodes are unreachable

    visited[m] <- TRUE

    # m is character, convert back to numeric to compare with v1
    from_m <- which(v1 == as.numeric(m))

    for (i in from_m) {
      # v2 is numeric, convert to character to match the type of names(dist)
      v <- as.character(v2[i])
      path <- dist[m] + w[i]
      if (path < dist[v])
        dist[v] <- path
    }
  }
  return(unname(dist)) # return the distance vector without names
}



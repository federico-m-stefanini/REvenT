#' Convert an eve_tree object into an igraph object
#'
#' The igraph package is  needed for the trasformation taking place.
#'
#' @param primoPTree object of type eve_tree.
#' @return an igraph  object
#'
#'
#' @export
evtree_2ig <- function(primoPTree){
  #
  #
  gp <- genProbLs(primoPTree)
  curG <- igraph::make_empty_graph(n=1+nrow(gp))
  igraph::V(curG)$labelLO[1] <- "CSI"
  igraph::V(curG)$labelLO[2:(1+nrow(gp))] <- gp$path
  # root
  estrattoreCSI <- which(gp$par == 1)
  for(auxCh in estrattoreCSI){
    chi_child <- which(igraph::V(curG)$labelLO == gp[auxCh,1])
    curG <- igraph::add_edges(curG,
                              edges=c(1,chi_child),
                      attr=list(event=gp[auxCh,2]))
  }
  for(aux1 in 1:(max(gp$par)-1)){# aux1 <- 1
    estrattore1 <- gp$par == aux1
    estrattore2 <-  gp$par == (aux1+1)
    estrattoreAll <- estrattore1 | estrattore2
    for(auxPa in which(estrattore1)){ #auxPa<- 1
      for(auxCh in which(estrattore2)){# auxCh <- 4
        dimestri <- base::nchar(gp[auxPa,1])
        if(substr(gp[auxCh,1],1,dimestri) == gp[auxPa,1]){
          # add an edge
          chi_par <- which(igraph::V(curG)$labelLO == gp[auxPa,1])
          chi_child <- which(igraph::V(curG)$labelLO == gp[auxCh,1])
          curG <- igraph::add_edges(curG,
                            edges=c(chi_par,chi_child),
                            attr=list(event=gp[auxCh,2]))
        }
      }
    }
  }

 return(curG)
}


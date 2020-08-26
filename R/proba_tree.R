#' Probabilistic tree
#'
#' This  function  creates a matrix of conditional probabilities
#' given a dataframe of events.
#'
#' @param myETree  an object of type "eve_tree" as create by
#' calling eve_tree()
#' @return an object of type "proba_tree"
#'
#'
#' @export
proba_tree <- function(myETree){
  stopifnot(class(myETree) == "data.frame")#
  #  error("Tipo di oggetto errato!!!")

  verifica <- attr(myETree,"Tipologia") == "eve_tree"
  if(!verifica){
    stop("Wrong class fro myETree !!!")
  }
  # crea matrice proba
  dimensioni <- dim(myETree)
  matPro <- matrix(NA,nrow = dimensioni[1],
                   ncol= dimensioni[2])
  matPro[,1] <- 1

  result <- list(eve_tree  = myETree,
                 proba_tree = matPro)

  attr(result,"Tipologia") <- "proba_tree"
  result
}

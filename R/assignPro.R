#' Assignement of conditional probability values
#'
#' This is an important function to work with probability-event trees.
#'
#' @param protree object of type proba_tree in which conditional probability values will be written.
#' @param evento  a string representing an event of interest and further conditioning events.
#'                For example 'Pa1:malato;Pa2:normal'
#' @param valPro conditional probability value.
#' @return object of type proba_tree with assigned probability values
#'
#'
#' @export
assignPro <- function(protree,evento,valPro){
  test2<- strsplit(evento,";",fixed=T)[[1]]
  test3<- strsplit(test2,":",fixed=T)
  test4<- lapply(test3,function(vx){
    res<- as.numeric(substr(vx[1],3,1000))
    list(res,vx[2])})
  dimeRow <- nrow(protree$eve_tree)
  res <- matrix(NA,ncol=length(test4),nrow=dimeRow)
  for(aux in 1:length(test4)){
     res[,aux] <- protree$eve_tree[,1+test4[[aux]][[1]]] == test4[[aux]][[2]]
  }
  selettore <- apply(res,1,all)
  # in che colonna devo salvarlo??
  ultimoC <- length(test4)
  colonnaCondiPro <- test4[[ultimoC]][[1]]+1
  protree$proba_tree[selettore,colonnaCondiPro] <- valPro
  #resL <- list(res,
  #           protree$eve_tree[selettore,],
  #            protree$proba_tree)
  colnames(protree$proba_tree)<- colnames(protree[[1]])
 return(protree)
}


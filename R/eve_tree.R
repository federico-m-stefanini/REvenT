#' Object builder for type  eve_tree
#'
#' This function crerates an object whose attribute Tipologia is  eve_tree.
#'
#'
#'@param myPart  a list of partitions for the sample space.
#'@return the created object
#'
#'
#'
#' @export
eve_tree <- function(myPart){
  numepart <- length(myPart)
  runlist<- vector("list", numepart +1)
  runlist[[1]]<-"CSI"
  runlist[2:(numepart+1)] <- myPart
  names(runlist) <- paste("Part.",0:numepart,sep="")
  compo1 <- expand.grid(runlist)
  attr(compo1,"Tipologia") <-"eve_tree"
  return(compo1)
}
#' Object builder for type  eve_tree
#'
#' This function crerates an object whose attribute Tipologia is  eve_tree.
#'
#'
#'@param myPart  a list of partitions for the sample space.
#'@return the created object
#'
#'
#'
#' @export
eve_tree <- function(myPart){
  numepart <- length(myPart)
  runlist<- vector("list", numepart +1)
  runlist[[1]]<-"CSI"
  runlist[2:(numepart+1)] <- myPart
  names(runlist) <- paste("Part.",0:numepart,sep="")
  compo1 <- expand.grid(runlist)
  attr(compo1,"Tipologia") <-"eve_tree"
  return(compo1)
}

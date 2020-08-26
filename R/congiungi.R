#'  Joint probability of paths
#'
#' @param objprob an object of type proba_tree where probability values have been already assigned
#' @return the joint distribution as defined by original partitions.
#'
#' @export
congiungi <- function(objprob){

   res1 <- apply(objprob$proba_tree,1,prod)
   res2 <- cbind(objprob$eve_tree,jointPro = res1)
  return(res2)
}

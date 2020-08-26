#' A helper function to load probability values within an eve_tree object
#'
#'
#' @param oggettoMat  an object that contains probability values and tree paths.
#' @param valoriProb  object that contains values to be inserted.
#' @return an updated proba_tree
#'
#' @examples
#' \dontrun{
#'   # from partitions to probability values
#'   partiVac <- list(par1 =c("M","notM"),
#'   par2 = c("T1","notT1"))
#'   spazioCampio <-eve_tree(partiVac)
#'   primoPTree<- proba_tree(spazioCampio)
#'   primoPTree
#'   tmpGen <- genProbLs(primoPTree)
#'   tmpGen$probVal[ c(1,2)] <- c(0.0002, 1-0.0002)
#'   tmpGen$probVal[c(3,4)] <- c(0.99, 1-0.95)
#'   tmpGen$probVal[c(5,6)] <- c(0.01, 0.95)
#'   tmpGen[,-1]
#'   primoPTreefilled <- fetchProb(primoPTree,tmpGen)
#'   primoPTreefilled
#'   }
#'
#'
#' @export
fetchProb <- function(oggettoMat, valoriProb){
  oggettoUpdated <- oggettoMat
  for(aux in 1:nrow(valoriProb)){
    oggettoUpdated <-assignPro(oggettoUpdated,valoriProb$path[aux],
                           valoriProb$probVal[aux])
  }
return(oggettoUpdated)
}

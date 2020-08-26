#' Conditionl probability of events of interest
#'
#'
#' @param tavolaCongiunta  an object of type proba_tree where probability values have been already assigned.
#' @param evento  a string describing the conditioned event of interest.
#' @return conditional probability value.
#'
#'
#' @export
condiziona <- function(tavolaCongiunta,evento){
  #tavolaCongiunta,"Pa2:cicatrix;Pa3:adulto"
  evento2 <- paste(strsplit(evento," ")[[1]],collapse="")
  test <- strsplit(evento2,"|",fixed=T)[[1]]
  eveFocus <- test[1]
  test2 <- strsplit(test[2],",",fixed=T)[[1]]
  if("CSI" %in% test2)test2 <- test2[-which(test2 =="CSI")]
  # marginalizza
  eveCondiz <- paste(test2,collapse=";")
  myMarginale <- marginalizza(tavolaCongiunta,eveCondiz)
  myCondizio <- myMarginale$sottoTavola
  myCondizio$jointPro <- myCondizio$jointPro/myMarginale$probaMargin
  if(eveFocus != ""){
     res <- marginalizza(myCondizio,eveFocus)[[1]]
  }else{
    res <- myCondizio
  }
  localizzazione <- which(colnames(res) =="jointPro")
  colnames(res)[localizzazione]<- "condPro"
  res
}

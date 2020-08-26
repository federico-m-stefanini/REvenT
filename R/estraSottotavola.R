#'  A helper to extract sub-tables
#'
#' This is a function that extracts the subtables of interest.
#'
#' @param tavolaPro  a full-sized table
#' @param evento event of interest
#' @return sub-table
#'
#'
#'
#' @export
estraSottotavola <- function(tavolaPro,evento){

  test2<-strsplit(evento,";",fixed=T)[[1]]
  test3<- strsplit(test2,":",fixed=T)
  test4<- lapply(test3,function(vx){
    res<- as.numeric(substr(vx[1],3,1000))+1
    #list(res,vx[2])
    tavolaPro[,res] == vx[2]
  })
  tmp<-  matrix(unlist(test4),ncol=length(test4))
  selettore <- apply(tmp,1,all)

  tavolaPro[selettore,]
}

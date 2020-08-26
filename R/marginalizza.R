#' Marginalization
#'
#'
#' @param tavolaPro  an object of conditional table where probability values are already assigned.
#' @param evento  a string describing the event of interest.
#' @return a probability value
#'
#'
#' @export
marginalizza <- function(tavolaPro, evento){
  test2<-strsplit(evento,";",fixed=T)[[1]]
  test3<- strsplit(test2,":",fixed=T)
  test4<- lapply(test3,function(vx){
    res<- as.numeric(substr(vx[1],3,1000))+1
    #list(res,vx[2])
    tavolaPro[,res] == vx[2]
    })
  tmp<-  matrix(unlist(test4),ncol=length(test4))
  selettore <- apply(tmp,1,all)
  tavolaRes <- tavolaPro[selettore,]
  #probaRes <- sum(tavolaPro$jointPro[selettore])
  posizpro<- ncol(tavolaPro)
  probaRes <- sum(tavolaPro[selettore,posizpro])

  # tavola ridotta
  dimeRes <- dim(tavolaRes)
  controlloMargin <- apply(tavolaRes[,-dimeRes[2]],2,
                            function(vx){length(unique(vx))})
  rigaCond <-tavolaRes[1,-posizpro]
  rigaCond$probaMargin <- probaRes
  for(aux in 2:length(controlloMargin)){
    if(controlloMargin[aux] !=1){
      rigaCond[,aux]<-factor("<OMEGA>")
      }
    }
  list(sottoTavola=tavolaRes,
       probaMargin= probaRes,
       tavolaMarginale = rigaCond
       )
}
#' Marginalization
#'
#'
#' @param tavolaPro  an object of conditional table where probability values are already assigned.
#' @param evento  a string describing the event of interest.
#' @return a probability value
#'
#'
#' @export
#'
marginalizza <- function(tavolaPro, evento){
  test2<-strsplit(evento,";",fixed=T)[[1]]
  test3<- strsplit(test2,":",fixed=T)
  test4<- lapply(test3,function(vx){
    res<- as.numeric(substr(vx[1],3,1000))+1
    #list(res,vx[2])
    tavolaPro[,res] == vx[2]
    })
  tmp<-  matrix(unlist(test4),ncol=length(test4))
  selettore <- apply(tmp,1,all)
  tavolaRes <- tavolaPro[selettore,]
  #probaRes <- sum(tavolaPro$jointPro[selettore])
  posizpro<- ncol(tavolaPro)
  probaRes <- sum(tavolaPro[selettore,posizpro])

  # tavola ridotta
  dimeRes <- dim(tavolaRes)
  controlloMargin <- apply(tavolaRes[,-dimeRes[2]],2,
                            function(vx){length(unique(vx))})
  rigaCond <-tavolaRes[1,-posizpro]
  rigaCond$probaMargin <- probaRes
  for(aux in 2:length(controlloMargin)){
    if(controlloMargin[aux] !=1){
      rigaCond[,aux]<-factor("<OMEGA>")
      }
    }
  list(sottoTavola=tavolaRes,
       probaMargin= probaRes,
       tavolaMarginale = rigaCond
       )
}

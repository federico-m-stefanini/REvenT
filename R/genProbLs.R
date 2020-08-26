#' Generate a list of all required assignements
#'
#'
#' @param objProT Object of type 'proba_tree'
#' @return a dataframe of required assignements; columns are path, event,
#' partition number, probability value.
#'
#' @export
genProbLs <- function(objProT){
  res<- list()
  myMat <- objProT$eve_tree
  dimensione <- dim(myMat)
  etichette<- paste("Pa",(2:dimensione[2])-1,":",sep="")
  myMatS <- as.matrix(myMat[,2:(dimensione[2])])
  myEtic <- matrix(etichette,byrow=T,ncol=dimensione[2]-1,nrow=dimensione[1])
  myTempla <- matrix(paste(myEtic,myMatS,sep=""),ncol= dimensione[2]-1)
  for(curCol in 1:ncol(myTempla)){
      if(curCol != 1){
      buffer<- unique(apply(cbind(myTempla[,1:curCol]),1,paste,collapse=";"))
      buffer3 <- apply(cbind(myTempla[,(curCol-1):1],"CSI"),1,paste,collapse=",")
      condiEve <- unique(apply(cbind(myTempla[,curCol],buffer3),1,paste,collapse=" | "))
      buffer4 <- cbind(Pa=curCol,path=buffer,condiEve,probVal =NA)
      }else{
      buffer <-  unique(myTempla[,1:curCol])
      condiEve <- unique(paste(buffer," | CSI",sep=""))
      buffer4 <- cbind(Pa=curCol,path=buffer,condiEve,probVal =NA)
      }
      res[[curCol]] <- buffer4
  }
# packaging
  resMat <- res[[1]]
  if(length(res)>1){
    for(aux in 2:length(res)){
      resMat <- rbind(resMat,res[[aux]])
      }
  }
  resDF<- data.frame(path=resMat[,2],
                     event= resMat[,3],
                     stringsAsFactors = FALSE)
  resDF$par <- as.numeric(resMat[,1])
  resDF$probVal <- NA
  class(resDF) <- c("data.frame","genProb")
  return(resDF)
}
#' Generate a list of all required assignements
#'
#'
#' @param objProT Object of type 'proba_tree'
#' @return a dataframe of required assignements; columns are path, event,
#' partition number, probability value.
#'
#' @export
genProbLs <- function(objProT){
  res<- list()
  myMat <- objProT$eve_tree
  dimensione <- dim(myMat)
  etichette<- paste("Pa",(2:dimensione[2])-1,":",sep="")
  myMatS <- as.matrix(myMat[,2:(dimensione[2])])
  myEtic <- matrix(etichette,byrow=T,ncol=dimensione[2]-1,nrow=dimensione[1])
  myTempla <- matrix(paste(myEtic,myMatS,sep=""),ncol= dimensione[2]-1)
  for(curCol in 1:ncol(myTempla)){
      if(curCol != 1){
      buffer<- unique(apply(cbind(myTempla[,1:curCol]),1,paste,collapse=";"))
      buffer3 <- apply(cbind(myTempla[,(curCol-1):1],"CSI"),1,paste,collapse=",")
      condiEve <- unique(apply(cbind(myTempla[,curCol],buffer3),1,paste,collapse=" | "))
      buffer4 <- cbind(Pa=curCol,path=buffer,condiEve,probVal =NA)
      }else{
      buffer <-  unique(myTempla[,1:curCol])
      condiEve <- unique(paste(buffer," | CSI",sep=""))
      buffer4 <- cbind(Pa=curCol,path=buffer,condiEve,probVal =NA)
      }
      res[[curCol]] <- buffer4
  }
# packaging
  resMat <- res[[1]]
  if(length(res)>1){
    for(aux in 2:length(res)){
      resMat <- rbind(resMat,res[[aux]])
      }
  }
  resDF<- data.frame(path=resMat[,2],
                     event= resMat[,3],
                     stringsAsFactors = FALSE)
  resDF$par <- as.numeric(resMat[,1])
  resDF$probVal <- NA
  class(resDF) <- c("data.frame","genProb")
  return(resDF)
}

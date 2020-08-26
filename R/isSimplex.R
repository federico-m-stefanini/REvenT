#' Check is simplex
#'
#' This function check if a collection of values adding to 1 is provided,
#' and in this case return TRUE.
#'
#' @param vettoval a vector to be tested
#' @return  TRUE if the condition is sactisfied otherwise FALSE.
#'
#' @export
#'
isSimplex <- function(vettoval){

  if(sum(vettoval) == 1.0){
     return(TRUE)
  }
  return(FALSE)
}
#' Check is simplex
#'
#' This function check if a collection of values adding to 1 is provided,
#' and in this case return TRUE.
#'
#' @param vettoval a vector to be tested
#' @return  TRUE if the condition is sactisfied otherwise FALSE.
#'
#' @export
#'
#'
isSimplex <- function(vettoval){

  if(sum(vettoval) == 1.0){
     return(TRUE)
  }
  return(FALSE)
}

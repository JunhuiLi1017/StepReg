#' Prints from a StepReg object
#'
#' print.StepReg prints to console the from an object of class StepReg
#'
#' @param x each dataframe from outputlist
#'
#' @return formatted dataframe
#' 
#' @importFrom purrr pmap_dfc pmap_chr
#' 
#' @importFrom dplyr `%>%`
#' 
#' @export
print.StepReg <- function(x){
  
  nameLen <- nchar(colnames(x),keepNA =FALSE)
  dfLen <- apply(sapply(x,nchar),2,max)
  lengths <- pmax(nameLen,dfLen)+2
  
  side <- rep("both",ncol(x))
  list(colnames(x),lengths,side) %>% pmap_chr(str_pad) -> dfHeader
  list(x,lengths,side) %>% pmap_dfc(str_pad) -> dataFrame
  
  cat(paste0(rep("\u2017",sum(lengths)),collapse=""));cat("\n")
  cat(paste0(dfHeader,collapse=""));cat("\n")
  cat(paste0(rep("\u2014",sum(lengths)),collapse=""));cat("\n")
  
  for(i in 1:nrow(dataFrame)){
    cat(paste0(dataFrame[i,],collapse=""),"\n")
  }
  cat(paste0(rep("\u2017",sum(lengths)),collapse=""));cat("\n")
}
#' Prints from a StepReg object
#'
#' print.StepReg prints to console the from an object of class StepReg
#'
#' @param x each dataframe from outputlist
#'
#' @return formatted dataframe
#' 
#' @importFrom purrr pmap_dfc map_int pmap_chr
#' 
#' @importFrom stringr str_trim
#' 
#' @importFrom dplyr `%>%`
#' 
#' @export
print.StepReg <- function(x){
  nameLen <- map_int(colnames(x),maxnchar)
  dfLen <- map_int(x,maxnchar)
  lengths <- pmax(nameLen,dfLen)+2
  
  dfHeader <- str_trim(colnames(x),"both")
  
  side <- rep("both",ncol(x))
  list(dfHeader,lengths,side) %>% pmap_chr(str_pad) -> dfHeader
  side <- c("right","right",rep("left",ncol(x)-2))
  
  list(x,lengths,side) %>% pmap_dfc(str_pad) -> dataFrame
  
  cat(paste0(rep("\u2017",sum(lengths)),collapse=""));cat("\n")
  cat(paste0(dfHeader,collapse=""));cat("\n")
  cat(paste0(rep("\u2014",sum(lengths)),collapse=""));cat("\n")
  
  for(i in 1:nrow(dataFrame)){
    cat(paste0(dataFrame[i,],collapse=""),"\n")
  }
  cat(paste0(rep("\u2017",sum(lengths)),collapse=""));cat("\n")
}
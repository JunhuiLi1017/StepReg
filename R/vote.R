#' Vote for all models
#'
#' Votes for all models across all combinations of strategies and metrics
#'
#' @param x each dataframe from outputlist
#' 
#' @param ... further parameters
#'
#' @return 
#' A dataframe with column names "model" and combinations of strategy and metric. 
#' The first column represents the model formula, and a checkmark indicates 
#' that the corresponding model was supported by the given strategy and metric 
#' combination.
#' 
#' @examples
#' data(mtcars)
#' formula <- mpg ~ .
#' x <- stepwise(formula = formula,
#'               data = mtcars,
#'               type = "linear",
#'               strategy = c("forward","bidirection","backward"),
#'               metric = c("AIC","BIC","SL"))
#' vote(x)
#' 
#' @export
#' 
vote <- function(x, ...){
  vote_df <- x[[which(names(x) %in% c("Vote_df"))]]
  uniq_model <- unique(vote_df[,1])
  vote_mat <- matrix("",length(uniq_model),nrow(vote_df))
  colnames(vote_mat) <- vote_df[,2]
  rownames(vote_mat) <- uniq_model
  for(i in 1:length(uniq_model)) {
    vote_mat[i,vote_df$model %in% uniq_model[i]] <- "\u2713"
  }
  vote_reform <- data.frame(rownames(vote_mat),vote_mat)
  colnames(vote_reform)[1] <- "model"
  rownames(vote_reform) <- NULL
  #class(vote_reform) <- c("StepReg")
  return(vote_reform)
}

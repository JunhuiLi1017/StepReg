#' Vote for Models Across Different Selection Strategies
#'
#' Creates a summary table showing which models were selected by different combinations of 
#' stepwise regression strategies and selection metrics.
#'
#' @param x A list object returned by the \code{stepwise()} function containing model selection results
#' @param ... Additional arguments (currently not used)
#'
#' @return A data frame where:
#'   \item{model}{The formula of each selected model}
#'   \item{strategy:metric}{Columns for each combination of strategy and metric used}
#'   
#' Each cell contains a checkmark (✓) if that model was selected by the corresponding 
#' strategy-metric combination. For the subset strategy with Information Criteria (IC), 
#' only the single best model across all variable numbers is shown. This does not apply 
#' to Significance Level (SL) since F/Rao statistics can only be compared between models 
#' with the same number of variables.
#'
#' @examples
#' # Load example data
#' data(mtcars)
#' 
#' # Run stepwise regression with multiple strategies and metrics
#' formula <- mpg ~ .
#' results <- stepwise(
#'   formula = formula,
#'   data = mtcars,
#'   type = "linear",
#'   strategy = c("forward", "backward", "subset"),
#'   metric = c("AIC", "BIC")
#' )
#' 
#' # Get voting summary
#' vote(results)
#'
#' @export
#' 
vote <- function(x, ...){
  vote_df <- x[["voted_model"]]
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

#' StepReg Shiny App
#' 
#' Launch the StepReg Shiny App for stepwise regression. In Step 1, upload your
#' dataset, and in Step 2, choose the desired arguments to perform stepwise 
#' regression.
#' 
#' @export
#' 
StepRegShinyApp <- function() {
  shiny::runApp(appDir = system.file('shiny', package='StepReg'))
}
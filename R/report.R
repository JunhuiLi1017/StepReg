#' Plots from a StepReg object
#'
#' plot.StepReg visualizes the variable selection procedure using a StepReg object
#'
#' @param x StepReg object
#' 
#' @param report_name report name
#' 
#' @param format the format of report, choose one or more from 'html', 'pdf', 'docx', 'rtf', 'pptx', 'xlsx'. default is 'html'
#'
#' @return plot
#' 
#' @import ggplot2
#' 
#' @importFrom stringr str_starts
#' 
#' @importFrom ggrepel geom_label_repel
#' 
#' @importFrom flextable save_as_html save_as_pptx save_as_rtf save_as_docx 
#' 
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars$yes <- mtcars$wt
#' formula <- mpg ~ . + 0
#' x <- stepwise(formula = formula,
#'               data = mtcars,
#'               type = "linear",
#'               strategy = "bidirection",
#'               metric = c("AIC", "BIC"))
#' report(x,report_name = "report", format = c("html","docx"))

report <- function(x, report_name, format=c('html', 'pdf', 'docx', 'rtf', 'pptx', 'xlsx')) {
  format <- match.arg(format, several.ok = TRUE)
  if(!is.null(report_name)) {
    if(any('xlsx' %in% format)) {
      write.xlsx(x = x,  file = paste0(report_name,".xlsx"))
    }else if(any(c('html', 'pdf', 'docx', 'rtf', 'pptx') %in% format)) {
      results <- list()
      for(j in seq_along(x)) {
        tb <- x[[j]] %>% 
          as.data.frame() %>%
          regulartable() %>% 
          autofit() %>% 
          align(align = "center", part = "all")
        results[names(x)[j]] <- list(tb)
      }
      for(i in format) {
        if(i %in% 'html') {
          save_as_html(values=results,
                       path = paste0(report_name,".html"))
        }else if (i %in% 'pdf') {
          save_as_pdf(values=results,
                      path = paste0(report_name,".pdf"))
        }else if (i %in% 'docx') {
          save_as_docx(values=results,
                       path = paste0(report_name,".docx"))
        }else if (i %in% 'rtf') {
          save_as_rtf(values=results,
                      path = paste0(report_name,".rtf"))
        }else if (i %in% 'pptx') {
          save_as_pptx(values=results,
                       path = paste0(report_name,".pptx"))
        }
      }
    }
  }
}
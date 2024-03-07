#' report from a StepReg object
#'
#' report output all tables in StepReg object to a report with format of html, docx, pptx, rtf, and xlsx.
#'
#' @param x StepReg object
#' 
#' @param report_name report name
#' 
#' @param format the format of report, choose one or more from 'html', 'docx', 'rtf', 'pptx', 'xlsx'. default is 'html'
#' 
#' @importFrom xlsx createWorkbook createSheet CellStyle Font Border addDataFrame saveWorkbook
#' 
#' @importFrom flextable save_as_html save_as_pptx save_as_rtf save_as_docx autofit flextable align
#' 
#' @importFrom dplyr %>%
#' 
#' @importFrom stringr str_split
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mtcars$yes <- mtcars$wt
#' formula <- mpg ~ . + 0
#' x <- stepwise(formula = formula,
#'               data = mtcars,
#'               type = "linear",
#'               strategy = "bidirection",
#'               metric = c("AIC", "BIC"))
#' report(x,report_name = "report", format = c("html","docx"))
#' }

report <- function(x, report_name, format = c('html', 'docx', 'rtf', 'pptx', 'xlsx')) {
  format <- match.arg(format, several.ok = TRUE)
  
  if(!is.null(report_name)) {
    if ('xlsx' %in% format) {
      wb <- createWorkbook()
      cs <- CellStyle(wb) + Font(wb, isBold = TRUE)
      
      for (i in 1:length(x)) {
        x[[i]] <- as.data.frame(x[[i]]) # tbl won't work properly when addDataFrame
        x[[i]] <- x[[i]] %>% mutate(across(where(is.numeric), as.character)) # Inf will not display properly in excel, so convert numeric to character
        
        sheet <- createSheet(wb, paste0("table", i))
        # Add sheet title and content:
        addDataFrame(data.frame(Title = names(x)[[i]]), sheet, startRow = 1, startColumn = 1, col.names = FALSE, row.names = FALSE)
        addDataFrame(x[[i]], sheet, startRow = 2, row.names = FALSE, colnamesStyle = cs)
      }
      saveWorkbook(wb, file = paste0(report_name, ".xlsx"))
    }
    
    if (any(c('html', 'docx', 'rtf', 'pptx') %in% format)) {
      results <- list()
      for (j in seq_along(x)) {
        tb <- x[[j]] %>% 
          as.data.frame() %>%
          flextable() %>% 
          autofit() %>% 
          align(align = "center", part = "all")
        results[names(x)[j]] <- list(tb)
      }
      for (i in format) {
        if (i %in% 'html') {
          save_as_html(values = results,
                       path = paste0(report_name,".html"))
          } else if (i %in% 'docx') {
            save_as_docx(values = results,
                         path = paste0(report_name,".docx"))
            } else if (i %in% 'rtf') {
              save_as_rtf(values=results,
                          path = paste0(report_name,".rtf"))
              } else if (i %in% 'pptx') {
                save_as_pptx(values = results,
                             path = paste0(report_name,".pptx"))
        }
      }
    }
  }
}
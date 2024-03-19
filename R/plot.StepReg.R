#' Plots from a StepReg object
#'
#' plot.StepReg visualizes the variable selection procedure using a StepReg object
#'
#' @param x StepReg object
#' 
#' @param ... Not used
#'
#' @return line and point plot for summary of selection process.
#' 
#' @import ggplot2
#' 
#' @importFrom stringr str_starts
#' 
#' @importFrom ggrepel geom_label_repel
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' formula <- mpg ~ . + 0
#' p <- stepwise(formula = formula,
#'               data = mtcars,
#'               type = "linear",
#'               strategy = c("forward","bidirection","subset"),
#'               metric = c("AIC", "BIC"),
#'               best_n = 3)
#' plot(p)
#' }

plot.StepReg <- function(x, ...) {
  x1 <- x[which(str_starts(names(x), "Summary of selection process under"))]
  # Combine tables if multiple of them
  plot_list <- list()
  #n="forward"
  for(n in class(x)[!class(x) %in% c("StepReg","list")]) {
    process_table <- x1[which(str_starts(names(x1), paste0("Summary of selection process under ",n)))]
    plot_data <- NULL
    if ("subset" == n) {
      for (i in 1:length(process_table)) {
        sub_table <- process_table[[i]]
        sub_plot_data <- cbind(sub_table, colnames(sub_table)[2])
        colnames(sub_plot_data) <- c("Step", "IC", "Variable", "Metric")
        plot_data <- rbind(plot_data, sub_plot_data)
      }
    } else{
      for (i in 1:length(process_table)) {
        sub_table <- process_table[[i]]
        var_plus <- var_minus <- NULL
        var_sym <- rep(NA, nrow(sub_table))
        if ("Enter_effect" %in% colnames(sub_table)) {
          var_plus <- paste0("(+)", sub_table[, colnames(sub_table) %in% "Enter_effect"])
          index_plus <- which(!sub_table[, colnames(sub_table) %in% "Enter_effect"] %in% "")
          var_sym[index_plus] <- var_plus[!var_plus %in% "(+)"]
        }
        if ("Remove_effect" %in% colnames(sub_table)) {
          var_minus <- paste0("(-)", sub_table[, colnames(sub_table) %in% "Remove_effect"])
          index_minus <- which(!sub_table[, colnames(sub_table) %in% "Remove_effect"] %in% "")
          var_sym[index_minus] <- var_minus[!var_minus %in% "(-)"]
        }
        ic_index <- which(colnames(sub_table) %in% constant_metrics) # constant_metrics in constants.R
        IC <- sub_table[, c(1, ic_index)]
        colnames(IC) <- NULL
        sub_plot_data <- data.frame(IC, var_sym, colnames(sub_table)[ic_index])
        plot_data <- rbind(plot_data, sub_plot_data)
      }
    }
    colnames(plot_data) <- c("Step", "Metric_value", "Variable", "Metric")
    plot_data$Step <- as.factor(as.numeric(plot_data$Step))
    plot_data$Metric_value <- as.numeric(plot_data$Metric_value)
    
    p <- ggplot(data = plot_data) + 
      aes(x = .data$Step,
          y = .data$Metric_value, 
          label = .data$Variable,
          group = .data$Metric) + 
      geom_point(aes(color = .data$Metric)) + 
      geom_label_repel(label.size = 0.05,
                       aes(color = .data$Metric),
                       show.legend = FALSE) +
      labs(title = paste0("Summary of selection process under ",n)) + 
      theme_minimal()
    
    if ("subset" != n) { # check if stepwise or best_subset
      p <- p + 
        geom_line(aes(linetype = .data$Metric,
                      color = .data$Metric)) + 
        xlab("Step")
      
    } else{
      p <- p + 
        xlab("Variable Number")
    }
    plot_list[n] <- list(p)
  }
  print(plot_list)
}

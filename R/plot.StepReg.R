#' Plots from a StepReg object
#'
#' plot.StepReg visualizes the variable selection procedure using a StepReg object
#'
#' @param x StepReg object
#' 
#' @param ... Not used
#'
#' @return plot
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
#' data(mtcars)
#' mtcars$yes <- mtcars$wt
#' formula <- mpg ~ . + 0
#' p <- stepwise(formula = formula,
#'               data = mtcars,
#'               type = "linear",
#'               strategy = "bidirection",
#'               metric = c("AIC", "BIC"))
#' plot(p)

plot.StepReg <- function(x, ...){
  process_table <- x[which(str_starts(names(x), "Selection Process"))]
  # Combine tables if multiple of them:
  plot_data <- NULL
  if ("subset" %in% class(x)){
    for (i in 1:length(process_table)){
      for (i in 1:length(process_table)){
        sub_table <- process_table[[i]]
        sub_plot_data <- cbind(sub_table, colnames(sub_table)[2])
        colnames(sub_plot_data) <- c("Step", "IC", "Variable", "IC_type")
        plot_data <- rbind(plot_data, sub_plot_data)
      }
    }
  } else{
    for (i in 1:length(process_table)){
      sub_table <- process_table[[i]]
      var_plus <- var_minus <- NULL
      var_sym <- rep(NA, nrow(sub_table))
      if ("EnteredEffect" %in% colnames(sub_table)){
        var_plus <- paste0("(+)", sub_table[, colnames(sub_table) %in% "EnteredEffect"])
        index_plus <- which(!sub_table[, colnames(sub_table) %in% "EnteredEffect"] %in% "")
        var_sym[index_plus] <- var_plus[!var_plus %in% "(+)"]
      }
      if ("RemovedEffect" %in% colnames(sub_table)){
        var_minus <- paste0("(-)", sub_table[, colnames(sub_table) %in% "RemovedEffect"])
        index_minus <- which(!sub_table[, colnames(sub_table) %in% "RemovedEffect"] %in% "")
        var_sym[index_minus] <- var_minus[!var_minus %in% "(-)"]
      }
      ic_index <- which(colnames(sub_table) %in% constant_metrics) # constant_metrics in constants.R
      IC <- sub_table[, c(1, ic_index)]
      colnames(IC) <- NULL
      sub_plot_data <- data.frame(IC, var_sym, colnames(sub_table)[ic_index])
      plot_data <- rbind(plot_data, sub_plot_data)
    }
  }
  colnames(plot_data) <- c("Step", "IC", "Variable", "IC_type")
  plot_data$Step <- as.factor(as.numeric(plot_data$Step))
  plot_data$IC <- as.numeric(plot_data$IC)
  
  p <- ggplot(data = plot_data) + 
    aes(x = .data$Step,
        y = .data$IC, 
        label = .data$Variable,
        group = .data$IC_type) + 
    geom_point(aes(color = .data$IC_type)) + 
    geom_label_repel(label.size = 0.05,
                     aes(color = .data$IC_type),
                     show.legend = FALSE) +
    xlab("Variable Number") +
    theme_minimal()
  
  if (ncol(sub_table) > 3){ # check if stepwise or best_subset
     p <- p + 
       geom_line(aes(linetype = .data$IC_type,
                     color = .data$IC_type))
  } else{
     p <- p + 
       labs(title = "Selection Process")
  }
  print(p)
}








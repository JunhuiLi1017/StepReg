#' Plots from a StepReg object
#'
#' print.StepReg prints to console the from an object of class StepReg
#'
#' @param x StepReg object
#' 
#' @param ... further paramters
#'
#' @return formatted dataframe
#' 
#' @import ggplot2
#' 
#' @importFrom stringr str_starts
#' 
#' @importFrom ggrepel geom_label_repel
#' 
#' @export
#' 
plot.StepReg <- function(x, ...) {
  process_table <- x[which(str_starts(names(x),"Process of Selection"))]
  #i=2
  plot_data <- NULL
  for(i in 1:length(process_table)){
    sub_table <- process_table[[i]]
    var_plus <- var_minus <- NULL
    var_sym <- rep(NA,nrow(sub_table))
    if("EnteredEffect" %in% colnames(sub_table)){
      var_plus <- paste0("(+)",sub_table[,colnames(sub_table) %in% "EnteredEffect"])
      index_plus <- which(!sub_table[,colnames(sub_table) %in% "EnteredEffect"] %in% "")
      var_sym[index_plus] <- var_plus[!var_plus %in% "(+)"]
    }
    if("RemovedEffect" %in% colnames(sub_table)){
      var_minus <- paste0("(-)",sub_table[,colnames(sub_table) %in% "RemovedEffect"])
      index_minus <- which(!sub_table[,colnames(sub_table) %in% "RemovedEffect"] %in% "")
      var_sym[index_minus] <- var_minus[!var_minus %in% "(-)"]
    }
    ic_index <- which(colnames(sub_table) %in% c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"))
    IC <- sub_table[,c(1,ic_index)]
    colnames(IC) <- NULL
    sub_plot_data <- data.frame(IC,var_sym,colnames(sub_table)[ic_index])
    plot_data <- rbind(plot_data,sub_plot_data)
  }
  colnames(plot_data) <- c("Step","IC","variable","IC_type")
  plot_data$Step <- as.factor(as.numeric(plot_data$Step))
  plot_data$IC <- as.numeric(plot_data$IC)
  p <- ggplot(data=plot_data) + 
    aes(x=Step,y=IC,label=variable,group=IC_type) + 
    geom_point(aes(color=IC_type)) + 
    geom_label_repel(label.size=0.05,aes(color=IC_type)) +
    geom_line(aes(linetype=IC_type,color=IC_type)) + 
    labs(title="Process of Selection") +
    theme_minimal()
  print(p)
}

#' Plots from a StepReg object
#'
#' plot.StepReg visualizes the variable selection procedure using a StepReg object
#'
#' @param x StepReg object
#' 
#' @param num_digits The number of digits to keep when rounding the results. Default is 6.
#' 
#' @param ... Not used
#'
#' @return a list of plot with detailed selection in each step
#' 
#' @import ggplot2
#' 
#' @importFrom stringr str_starts
#' 
#' @importFrom cowplot plot_grid
#' 
#' @importFrom ggrepel geom_label_repel
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' formula <- mpg ~ .
#' x <- stepwise(formula = formula,
#'               data = mtcars,
#'               type = "linear",
#'               strategy = c("forward","bidirection","backward"),
#'               metric = c("AIC","BIC","SL"),
#'               best_n = 3)
#' plot(x)
#' }

plot.StepReg <- function(x, num_digits = 6, ...) {
  y <- x$Detail_selection_summary
  process_list <- x[which(str_starts(names(x), "Summary of selection process under"))]
  strategy_vec <- class(x)[!class(x) %in% c("StepReg","list","linear","logit","cox","gamma","negbin","poisson")]
  plot_list <- list()

  for(n in strategy_vec) {
    detail_selection <- y[y$strategy == n,]
    if(n != "subset") {
      p1 <- ggplot(detail_selection, 
                   aes(x = .data$step,
                       y = .data$variable)) +
        geom_tile(aes(fill = .data$selected), width = 0.99, height = 0.95) +
        geom_text(aes(label = round(.data$value, num_digits)),
                  color = "white",
                  size = 1) +
        scale_fill_manual(values = c("YES" = "red", "NO" = "black")) +
        theme_light() + 
        scale_x_continuous(breaks = unique(detail_selection$step)) + 
        theme(axis.text.y = element_text(size = 6)) +
        facet_wrap(~ .data$metric,ncol=1) + 
        ggtitle(paste0("Statistics for all variables at each step"))
    } else {
      p1 <- NULL
    }

    process_table <- process_list[which(str_starts(names(process_list), paste0("Summary of selection process under ",n)))]
    if (n == "subset") {
      process_table_reformat <- lapply(process_table, function(df) {
        df <- cbind(df,colnames(df)[2])
        colnames(df) <- c("Step", "MetricValue", "Variable", "Metric")
        return(df)
      })
    } else {
      process_table_reformat <- lapply(process_table, function(df) {
        df <- cbind(df,colnames(df)[ncol(df)])
        var_sym <- rep(NA, nrow(df))
        var_plus <- paste0("(+)", df[, colnames(df) %in% "EffectEntered"])
        index_plus <- which(!var_plus %in% "(+)")
        var_minus <- paste0("(-)", df[, colnames(df) %in% "EffectRemoved"])
        index_minus <- which(!var_minus %in% "(-)")
        var_sym[index_plus] <- var_plus[index_plus]
        var_sym[index_minus] <- var_minus[index_minus]
        df <- cbind(df[,c(1,ncol(df)-1,ncol(df))],var_sym)
        colnames(df) <- c("Step", "MetricValue", "Metric", "Variable")
        return(df)
      })
    }
    names(process_table_reformat) <- NULL
    plot_data <- do.call(rbind, process_table_reformat)
    plot_data$Step <- as.numeric(plot_data$Step)
    plot_data$MetricValue <- as.numeric(plot_data$MetricValue)
    ## make a dual y-axis with log10 transformed for 'SL' if 'SL' is selected
    if("SL" %in% plot_data$Metric & n != 'subset') {
      df <- plot_data
      df[df$Metric == "SL",]$MetricValue <- detail_selection[detail_selection$metric == "SL" & detail_selection$selected == "YES","value"]
      df$MetricValue[df$MetricValue %in% Inf] <- max(df$MetricValue[!df$MetricValue %in% Inf]) * 1.1
      a <- range(log10(df[df$Metric == "SL", ]$MetricValue))
      
      if(all(plot_data$Metric %in% "SL")) {
        p2 <- ggplot(df, aes(x = .data$Step, group = .data$Metric)) +
          geom_point(aes(y = (log10(.data$MetricValue) - a[1])/diff(a), color = .data$Metric)) + 
          geom_label_repel(aes(y = (log10(.data$MetricValue) - a[1])/diff(a), color = .data$Metric, label = .data$Variable),
                           label.size = 0.05,
                           show.legend = FALSE) + 
          scale_y_continuous(
            labels = function(x) sprintf("%.1e", 10^(x * diff(a) + a[1])),
            #labels = ~ 10^(. * diff(a) + a[1]),
            breaks = (pretty(log10(df$MetricValue)) - a[1])/diff(a), 
            name = "SL"
          )
      } else {
        b <- range(df[df$Metric != "SL", ]$MetricValue)
        p2 <- ggplot(df, aes(x = .data$Step, group = .data$Metric)) +
          geom_point(aes(y = ifelse(.data$Metric == "SL", (log10(.data$MetricValue) - a[1])/diff(a), (.data$MetricValue - b[1])/diff(b)), color = .data$Metric)) + 
          geom_label_repel(aes(y = ifelse(.data$Metric == "SL", (log10(.data$MetricValue) - a[1])/diff(a), (.data$MetricValue - b[1])/diff(b)), color = .data$Metric, label = .data$Variable),
                           label.size = 0.05,
                           show.legend = FALSE) + 
          scale_y_continuous(
            labels = function(x) sprintf("%.1e", 10^(x * diff(a) + a[1])),
            #labels = ~ 10^(. * diff(a) + a[1]),
            breaks = (pretty(log10(df$MetricValue)) - a[1])/diff(a), 
            name = "SL",
            sec.axis = sec_axis(~(diff(b) * . + b[1]), name = "Information Criteria"),
          ) 
      }
       p2 <- p2 + 
        scale_x_continuous(breaks = df$Step) + 
        labs(title ="Selection process") + 
        theme_light()
    } else {
      p2 <- ggplot(data = plot_data) + 
        aes(x = .data$Step,
            y = .data$MetricValue, 
            label = .data$Variable,
            group = .data$Metric) + 
        geom_point(aes(color = .data$Metric)) + 
        geom_label_repel(label.size = 0.05,
                         aes(color = .data$Metric),
                         show.legend = FALSE) +
        labs(title ="Selection process") + 
        theme_light()
    }
    
    if (n != "subset") { # check if stepwise or best_subset
      if("SL" %in% plot_data$Metric) {
        if(all(plot_data$Metric %in% "SL")) {
          p2 <- p2 + geom_line(aes(y = (log10(.data$MetricValue) - a[1])/diff(a), color = .data$Metric)) + xlab("step")
        } else {
          p2 <- p2 + geom_line(aes(y = ifelse(.data$Metric == "SL", (log10(.data$MetricValue) - a[1])/diff(a), (.data$MetricValue - b[1])/diff(b)), color = .data$Metric)) + xlab("step")
        }
        if(n == "forward") {
          sle <- x[[1]][which(x[[1]][,1] %in% "significance level for entry (sle)"), 2]
          p2 <- p2 + 
            geom_hline(yintercept=(log10(as.numeric(sle)) - a[1])/diff(a), linetype="dashed", color = "gray") + 
            geom_text(aes(0,(log10(as.numeric(sle)) - a[1])/diff(a), label = paste0("sle=",sle), vjust = -1), color = "gray")
        } else if (n == "backward") {
          sls <- x[[1]][which(x[[1]][,1] %in% "significance level for stay (sls)"), 2]
          p2 <- p2 + 
            geom_hline(yintercept=(log10(as.numeric(sls)) - a[1])/diff(a), linetype="dotdash", color = "gray") + 
            geom_text(aes(max(as.numeric(.data$Step)), (log10(as.numeric(sls)) - a[1])/diff(a), label = paste0("sls=",sls), vjust = -1), color = "gray")
        } else {
          sle <- x[[1]][which(x[[1]][,1] %in% "significance level for entry (sle)"), 2]
          sls <- x[[1]][which(x[[1]][,1] %in% "significance level for stay (sls)"), 2]
          p2 <- p2 +
            geom_hline(yintercept=(log10(as.numeric(sle)) - a[1])/diff(a), linetype="dashed", color = "gray") + 
            geom_text(aes(1.5,(log10(as.numeric(sle)) - a[1])/diff(a), label = paste0("sle=",sle), vjust = -1), color = "gray") + 
            geom_hline(yintercept=(log10(as.numeric(sls)) - a[1])/diff(a), linetype="dotdash", color = "gray") + 
            geom_text(aes(max(as.numeric(.data$Step)), (log10(as.numeric(sls)) - a[1])/diff(a), label = paste0("sls=",sls), vjust = -1), color = "gray")
        }
      } else {
        p2 <- p2 + geom_line(aes(linetype = .data$Metric, color = .data$Metric)) + xlab("step")
      }
    } else{
      p2 <- p2 + xlab("Variable Number")
    }
    if(is.null(p1)) {
      p3 <- p2
    } else {
      #rel_num <- length(unique(plot_data$Metric))
      p3 <- plot_grid(p1, p2, ncol=1, rel_heights = c(1, 0.75))
    }
    plot_list[n] <- list(p3)
  }
  return(plot_list)
}


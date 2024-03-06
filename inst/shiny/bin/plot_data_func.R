# Define the plot_data_func function

plot_data_func <- function(plot_type_value, var_plot_value, data_value) {
  plot_type <- switch(
    plot_type_value,
    "Bar plot" = {
      # Create a bar plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(x = .data[[var]])) +
          geom_bar() +
          labs(title = paste("Bar Plot of", var), x = var)
      })
    },
    "Box plot" = {
      # Create a box plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(y = .data[[var]])) +
          geom_boxplot() +
          labs(title = paste("Box Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)
      })
    },
    "Correlation plot" = {
      # Create a Correlation plot for each selected variable
      corr <- round(cor(select(data_value, all_of(var_plot_value))), 1)
      p <- list(corr %>% ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower", lab = TRUE))
    },
    "Density plot" = {
      # Create a density plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(.data[[var]])) +
          geom_density() +
          labs(title = paste("Density Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)
      })
    },
    "Dot plot" = {
      # Create a dot plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(.data[[var]])) +
          geom_dotplot() +
          labs(title = paste("Dot Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)
      })
    },
    "Histogram" = {
      # Create a histogram plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(.data[[var]])) +
          geom_histogram() +
          labs(title = paste("Histogram Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)
      })
    },
    "QQ plot" = {
      # Create a qq plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(sample = .data[[var]])) +
          stat_qq() + 
          stat_qq_line() +
          labs(title = paste("QQ Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)
      })
    },
    "Scatter and Line plot" = {
      # Create a scatter plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          select(all_of(var_plot_value)) %>% 
          gather(-{{var}}, key = "var", value = "value") %>%
          ggplot(aes(x = value, y = .data[[var]])) +
          geom_point() +
          stat_smooth() +
          facet_wrap(~ var, scales = "free") +
          theme_bw()
      })
    },
    "Pairs plot" = {
      # Create a pairs plot
      data_value %>% 
        select(all_of(var_plot_value)) %>% 
        ggpairs
    }
  )
  return(plot_type)
}
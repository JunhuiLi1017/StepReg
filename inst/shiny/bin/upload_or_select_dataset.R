upload_or_select_dataset <- function(example_dataset, upload_file, header_value, sep_value, quote_value){
  if (example_dataset != " ") {
    # Read the selected example dataset
    data(CreditCard, package = 'AER')
    data(remission,package="StepReg")
    survival::lung %>%
      mutate(sex = factor(sex, levels = c(1,2))) %>% # make sex as factor
      mutate(status = ifelse(status == 1, 0, 1)) %>% # recode status: 0 means cencored, 1 means dead
      na.omit() -> lung# get rid of incomplete records
    
    
    df <- switch(example_dataset,
                 "base::mtcars" = mtcars,
                 "StepReg::remission" = remission,
                 "survival::lung" = lung,
                 "AER::CreditCard" = CreditCard)
  }
  if (!is.null(upload_file)) {
    # Read the uploaded file
    df <- read.table(upload_file$datapath,
                     header = header_value,
                     sep = sep_value,
                     quote = quote_value)
  }
  return(df)
}
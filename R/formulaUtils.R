#' Formula helper functions
#' 
#' Extract information from formula object.
#'
#' @return data.frame
#' 
#' @author Junhui Li, Kai Hu

# Paramters            Value
# 1          Response Variable cbind(mpg, drat)
# 2          Included Variable             NULL
# 3            Strategy Method      bidirection
# 4           Metric Criterion              AIC
# 5 Variable significance test           Pillai
# 6    Multicollinearity Terms              yes
# 7                  Intercept                0
# 

extractParams <- function(formula, data, include, ){
	term_form <- terms(formula, data = data)
	vars <- as.character(attr(term_form, "variables"))[-1]
	y_name <- vars[attr(term_form, "response")]
	x_name <- attr(term_form, "term.labels")
	if(attr(term_form, "intercept") == 0){
		intercept <- "0"
	}else{
		intercept <- "1"
	}
	if(is.character(include)){
		include_name <- include
		merge_include_name <- paste0(include_name,collapse=" ")
	}else if(is.null(include)){
		include_name <- NULL
		merge_include_name <- "NULL"
	}
	if(!is.null(weights)){
		if(length(weights) == nrow(data)){
			weight_data <- data*sqrt(weights)
		}else{
			stop("Variable length is different ('(weights)')")
		}
	}else{
		weight_data <- data
	}
	
	# convert "chr" factor into "numeric" factor: chrToNum()
	charTonumFac(df_in, col_name) {
		df_subset <- df_in[, col_name] 
		char_cols <- sapply(df_subset, class) == "character"
		df_subset[, char_cols] <- lapply(df_subset[, char_cols], function(x) as.numeric(as.factor(x))) # need to further debug this line
		df_subset
	}
		
		
		char_cols <- sapply(my_data, class) == "character"
		my_data[, char_cols] <- lapply(my_data[, char_cols], function(x) as.numeric(as.factor(x)))
		
		sapplay(char_cols,)
		data[[char_cols]] <- as.numeric(as.factor(data[[char_cols]]))
	}
	removeColinear <- function(){
		
	}
	
	# detect and remove multicollinearity
	lm_raw <- lm(formula, data = weight_data)
	all_var_class <- attr(lm_raw$terms, "dataClasses")
	class_table <- as.data.frame(table(all_var_class))
	colnames(class_table) <- c("class", "variable")
	
	for(i in names(table(all_var_class))){
		class_table[names(table(all_var_class)) %in% i,2] <- paste0(names(all_var_class[all_var_class %in% i]), collapse=" ")
	}
	if(any(all_var_class=="factor")){
		factVar <- names(which(all_var_class=="factor"))
		for(i in factVar){
			weight_data[,i] <- as.factor(as.numeric(weight_data[,i]))
		}
	}
	
	xMatrix <- as.matrix(weight_data[,xName])
	qrXList <- qr(xMatrix,tol=1e-7) # detect multicolinearity
	rank0 <- qrXList$rank
	pivot0 <- qrXList$pivot
	if(rank0 < length(pivot0)){
		mulcolX <- colnames(qrXList$qr)[pivot0[(rank0+1):length(pivot0)]]
		mulcolMergeName <- paste0(mulcolX,collapse=" ")
	}else{
		mulcolX <- NULL
		mulcolMergeName <- "NULL"
	}
	xName <- setdiff(xName,mulcolX)
	
	x_name
	y_name
	intercept 
	weight_data
	merge_include_name # for easier reformatting of formula
	multicollinearity
	
}

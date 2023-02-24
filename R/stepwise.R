#' Stepwise Regression
#' 
#' Stepwise regression analysis metrics model based on information criteria and F or approximate F test with 'forward', 'backward', 'bidirection' and 'subset' model strategy method.
#' 
#' @param type stepwise regression type. Linear, Logit or Cox. Default is linear.
#' 
#' @param formula Model formulae. The models fitted by the lm functions are specified in a compact symbolic form. The basic structure of a formula is the tilde symbol (~) and at least one independent (righthand) variable. In most (but not all) situations, a single dependent (lefthand) variable is also needed. Thus we can construct a formula quite simple formula (y ~ x). Multiple independent variables by simply separating them with the plus (+) symbol (y ~ x1 + x2). Variables in the formula are removed with a minus(-) symbol (y ~ x1 - x2). One particularly useful feature is the . operator when modelling with lots of variables (y ~ .). The \%in\% operator indicates that the terms on its left are nested within those on the right. For example y ~ x1 + x2 \%in\% x1 expands to the formula y ~ x1 + x1:x2. A model with no intercept can be specified as y ~ x - 1 or y ~ x + 0 or y ~ 0 + x. Multivariate multiple regression can be specified as cbind(y1,y2) ~ x1 + x2.
#'
#' @param data Data set including dependent and independent variables to be analyzed
#'
#' @param include Force vector of effects name to be included in all models.
#' 
#' @param strategy Model strategy method including "forward", "backward", "bidirection" and 'subset',forward strategy starts with no effects in the model and adds effects, backward strategy starts with all effects in the model and removes effects, while bidirection regression is similar to the forward method except that effects already in the model do not necessarily stay there, and subset method requests specifies the best-subset strategy method, which uses the branch-and-bound technique to efficiently search for subsets of model effects that best predict the response variable.
#' 
#' @param metric Specify the criterion that uses to determine the order in which effects enter and leave at each step of the specified strategy method including "AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SBC" and "SL".
#' 
#' @param sle Specify the significance level for entry, default is 0.15
#' 
#' @param sls Specify the significance level for staying in the model, default is 0.15
#' 
#' @param weights Numeric vector to provide a weight for each observation in the input data set. Note that weights should be ranged from 0 to 1, while negative numbers are forcibly converted to 0, and numbers greater than 1 are forcibly converted to 1. If you do not specify a weight vector, each observation has a default weight of 1.
#' 
#' @param test_method Statistic for multivariate regression analysis, including Wilks' lamda ("Wilks"), Pillai Trace ("Pillai"), Hotelling-Lawley's Trace ("Hotelling"), Roy's Largest Root ("Roy")
#'
#' @param best Control the number of models displayed in the output, default is NULL, which means all possible model will be displayed.
#' 
#' @references 
#' 
#' Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable strategy in multivariable regression using sas/iml. , 07(i12).
#' 
#' Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.
#' 
#' Dharmawansa, P. , Nadler, B. , & Shwartz, O. . (2014). Roy's largest root under rank-one alternatives:the complex valued case and applications. Statistics.
#' 
#' Hannan, E. J., & Quinn, B. G. (1979). The determination of the order of an autoregression. Journal of the Royal Statistical Society, 41(2), 190-195.
#' 
#' Harold Hotelling. (1992). The Generalization of Student's Ratio. Breakthroughs in Statistics. Springer New York.
#' 
#' Hocking, R. R. (1976). A biometrics invited paper. the analysis and strategy of variables in linear regression. Biometrics, 32(1), 1-49.
#' 
#' Hurvich, C. M., & Tsai, C. (1989). Regression and time series model strategy in small samples. Biometrika, 76(2), 297-307.
#' 
#' Judge, & GeorgeG. (1985). The Theory and practice of econometrics /-2nd ed. The Theory and practice of econometrics /. Wiley.
#' 
#' Mallows, C. L. (1973). Some comments on cp. Technometrics, 15(4), 661-676.
#' 
#' Mardia, K. V., Kent, J. T., & Bibby, J. M. (1979). Multivariate analysis. Mathematical Gazette, 37(1), 123-131.
#' 
#' Mckeon, J. J. (1974). F approximations to the distribution of hotelling's t20. Biometrika, 61(2), 381-383.
#' 
#' Mcquarrie, A. D. R., & Tsai, C. L. (1998). Regression and Time Series Model strategy. Regression and time series model strategy /. World Scientific.
#' 
#' Pillai, K. . (1955). Some new test criteria in multivariate analysis. The Annals of Mathematical Statistics, 26(1), 117-121.
#' 
#' R.S. Sparks, W. Zucchini, & D. Coutsourides. (1985). On variable strategy in multivariate regression. Communication in Statistics- Theory and Methods, 14(7), 1569-1587.
#' 
#' Sawa, T. (1978). Information criteria for discriminating among alternative regression models. Econometrica, 46(6), 1273-1291.
#' 
#' Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), pags. 15-18.
#' 
#' @author Junhui Li 
#' 
#' @examples
#' data(mtcars)
#' mtcars$yes <- mtcars$wt
#' formula <- cbind(mpg,drat) ~ . + 0
#' stepwise(formula=formula,
#'          data=mtcars,
#'          strategy="bidirection",
#'          metric="AIC")
#' @keywords stepwise regression
#' 
#' @importFrom utils combn
#' 
#' @importFrom stats anova coef glm lm logLik pf reformulate sigma terms
#' 
#' @export
#' 
#' 
stepwise <- function(type = c("linear","logit","cox"),
                     formula,
                     data,
                     include = NULL,
                     strategy = c("forward","backward","bidirection","subset"),
                     metric = c("AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SL","SBC","IC(3/2)","IC(1)"),
                     sle = 0.15,
                     sls = 0.15,
                     test_method = c("Pillai","Wilks","Hotelling-Lawley","Roy","Rao","LRT","efron","breslow","exact"),
                     weights = NULL,
                     best_n = NULL){
  type <- match.arg(type)
  strategy <- match.arg(strategy)
  metric <- match.arg(metric)
  test_method <- match.arg(test_method)
  
  if(missing(data)){ 
    stop("'data' parameter is missing.") 
  }
  
  if(missing(formula)){ 
    stop("'formula' parameter is missing.") 
  }else{
    if(!inherits(formula, "formula")){
      stop("'formula' must be a formula class.")
    }
    term_form <- terms(formula,data=data)
    vars <- as.character(attr(term_form, "variables"))[-1]
    y_name <- vars[attr(term_form, "response")]
    x_name <- attr(term_form,"term.labels")
    if(attr(term_form, "intercept") == 0){
      intercept <- "0"
    }else{
      intercept <- "1"
    }
  }
  
  if(is.null(include)){
    include_name <- NULL
    merge_inc_name <- "NULL"
  }else{
    if(!all(include %in% x_name)){
      stop(paste0("'include' must be a subset of: c('",paste0(x_name,collapse = "','"),"')"))
    }else{
      include_name <- include
      merge_inc_name <- paste0(include_name,collapse=" ")
    }
  }
  
  if(is.numeric(sle) & is.numeric(sls)){
    if(sle <= 0 | sle > 1){
      stop("'sle' shoule be 0~1")
    }
    if(sls <= 0 | sls > 1){
      stop("'sls' shoule be 0~1")
    }
  }else{
    stop("'sle' and 'sls' should be numeric variable")
  }
  ## check 'metric' and 'test_method' according to 'type'
  linear_metric <- c("AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SL","SBC")
  logit_metric <- c("SL","AIC","AICc","SBC","HQ","HQc","IC(3/2)","IC(1)")
  cox_metric <- c("SL","AIC","AICc","SBC","HQ","HQc","IC(3/2)","IC(1)")
  
  linear_test_method = c("Pillai","Wilks","Hotelling-Lawley","Roy")
  logit_test_method = c("Rao","LRT")
  cox_test_method = c("efron","breslow","exact")
  
  if(type == "linear"){
    if(!metric %in% linear_metric){
      stop("for type 'linear': 'metric' must be from one of the c('",paste0(linear_metric,collapse="','"),"').")
    }
    if(startsWith(y_name, "cbind(")){
      if(!test_method %in% linear_test_method){
        stop("for type 'linear'(multivariate): 'test_method' must be from one of the c('",paste0(linear_test_method,collapse="','"),"').")
      }
    }
  }else if(type == "logit"){
    if(!metric %in% linear_metric){
      stop("for type 'logit': 'metric' must be from one of the c('",paste0(linear_metric,collapse="','"),"').")
    }
    if(!test_method %in% logit_test_method){
      stop("for type 'logit': 'test_method' must be from one of the c('",paste0(logit_test_method,collapse="','"),"').")
    }
  }else if(type == 'cox'){
    if(!metric %in% linear_metric){
      stop("for type 'cox': 'metric' must be from one of the c('",paste0(linear_metric,collapse="','"),"').")
    }
    if(!test_method %in% cox_test_method){
      stop("for type 'cox': 'test_method' must be from one of the c('",paste0(cox_test_method,collapse="','"),"').")
    }
  }
}








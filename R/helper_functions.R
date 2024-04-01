dummify <- function(x) {
  if(is.matrix(x) || is.data.frame(x)) {
    x <- as.data.frame(x)
    y <- do.call(data.frame, lapply(x, dummify))
    return(as.matrix(y))
  }
  # x is 1-dimensional
  if(is.complex(x))
    return(as.matrix(data.frame(Re=Re(x), Im=Im(x))))
  # convert factors etc
  if(is.character(x))
    x <- factor(x)
  if(is.logical(x))
    x <- factor(x, levels=c(FALSE,TRUE))
  if(is.factor(x)) {
    # convert to dummy variables
    nx <- length(x)
    lev <- levels(x)
    y <- matrix(0L, nrow=nx, ncol=length(lev))
    colnames(y) <- lev
    y[cbind(seq_len(nx), as.integer(x))] <- 1L
    return(y)
  }
  # convert to numeric
  y <- as.numeric(x)
  if(!is.matrix(y))
    y <- matrix(y, ncol=1)
  return(y)
}



### model selection by country
countrymodel_select <- function(dt, xvars, y){

  dt <- as.data.table(dt)

  dt <- dt[,which(unlist(lapply(dt, function(x)!all(is.na(x))))), with = F]

  xvars <- xvars[xvars %in% colnames(dt)]

  dt <- na.omit(dt[,c(y, xvars), with = F])

  xset <- dt[, xvars, with = F]

  y <- dt[,y]

  model_dt <- cbind(y, xset)

  lasso_model <- hdm::rlasso(y ~ ., data = model_dt, post = TRUE)

  lasso_model <- cbind(names(lasso_model$coefficients), as.data.table(lasso_model$coefficients))

  colnames(lasso_model) <- c("variable_name", "value")

  varsselect_list <- lasso_model$variable_name[lasso_model$value != 0]
  varsselect_list <- varsselect_list[!varsselect_list == "(Intercept)"]


  return(varsselect_list)

}


specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#### some povmap style estimation helpers for povmap
fh_reportcoef_table <- function(model,
                                decimals = 3){

  options(scipen = 999)

  variables <- rownames(model$model$coefficients)

  coef_dt <- as.data.frame(model$model$coefficients)

  coef_dt <- cbind(variables, coef_dt)

  coef_dt$Sig <- ifelse(coef_dt$p.value < 0.001, "***",
                        ifelse(coef_dt$p.value < 0.05 &
                                 coef_dt$p.value >= 0.001, "**",
                               ifelse(coef_dt$p.value < 0.01 &
                                        coef_dt$p.value >= 0.05, "*", "")))

  coef_dt$coefficients <- ifelse(coef_dt$coefficients < abs(0.0004999999),
                                 signif(coef_dt$coefficients, 2),
                                 specify_decimal(coef_dt$coefficients, decimals))

  coef_dt$std.error <- ifelse(coef_dt$std.error < abs(0.0004999999),
                              signif(coef_dt$std.error, 2),
                              specify_decimal(coef_dt$std.error, decimals))

  coef_dt$coefficients <- paste0(coef_dt$coefficients, coef_dt$Sig)

  colnames(coef_dt)[colnames(coef_dt) %in% c("std.error")] <-
    c("std_error")

  rownames(coef_dt) <- seq(nrow(coef_dt))

  return(coef_dt[, c("variables", "coefficients", "std_error")])



}

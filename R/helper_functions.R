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


stepAIC_wrapper <- function(dt, xvars, y){

  dt <- as.data.table(dt)

  dt <- dt[,which(unlist(lapply(dt, function(x)!all(is.na(x))))), with = F]

  xvars <- xvars[xvars %in% colnames(dt)]

  dt <- na.omit(dt[,c(y, xvars), with = F])

  xset <- dt[, xvars, with = F]

  y <- dt[,y]

  model_dt <- cbind(y, xset)

  full_model <- lm(y ~ ., data = model_dt)

  ### use the vif method to drop multicollinear variables
  vif_model <- vif(full_model)


  stepwise_model <- stepAIC(full_model,
                            direction = "both",
                            trace = 0)

  return(stepwise_model)

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

#### model results
fh_normality_fit <- function(model){

  rsqval_list <- model$model$model_select[c(5, 6)]

  re_skew <- moments::skewness(model$model$random_effects)
  re_kurt <- moments::kurtosis(model$model$random_effects)

  residual_skew <- moments::skewness(model$model$std_real_residuals)
  residual_kurt <- moments::kurtosis(model$model$std_real_residuals)

  table_dt <-
    data.frame(indicator = c("rsq_marginal", "rsq_conditional",
                             "epsilon_skewness", "epsilon_kurtosis",
                             "random_skewness", "random_kurtosis"),
               value = c(rsqval_list[[1]], rsqval_list[[2]],
                         residual_skew, residual_kurt,
                         re_skew, re_kurt))

  return(table_dt)


}



### modify the benchmark function for FH to support group benchmarking


benchmark2 <- function(object, benchmark, share, type = "raking",
                       overwrite = FALSE, group) {
  check_benchmark_arguments2(
    object = object,
    benchmark = benchmark,
    share = share,
    type = type,
    overwrite = overwrite
  )

  estim <- object$ind$FH
  FH_bench <- rep(NA, length(object$ind$FH))
  mse <- object$MSE$FH

  bench_dt <- data.frame(benchmark = benchmark,
                         share = share,
                         estim = estim,
                         group = group,
                         mse = mse)


  benchwork <- function(estim,
                        benchmark,
                        share,
                        mse){

    if (type == "raking") {
      FH_bench <- estim + benchmark - sum(share * estim)
    } else if (type == "ratio") {
      phi <- share / estim
      FH_bench <- estim + (1 / (sum(share^2 / phi))) *
        (benchmark - sum(share * estim)) * (share / phi)
    } else if (type == "MSE_adj") {
      phi <- share / mse
      FH_bench <- estim + (1 / (sum(share^2 / phi))) *
        (benchmark - sum(share * estim)) * (share / phi)
    }

    return(FH_bench)

  }

  result <- list()

  for (i in seq_along(unique(group))){

    result[[i]] <- benchwork(estim = bench_dt$estim[bench_dt$group == unique(group)[i]],
                             benchmark = bench_dt$benchmark[bench_dt$group == unique(group)[i]],
                             share = bench_dt$share[bench_dt$group == unique(group)[i]],
                             mse = bench_dt$mse[bench_dt$group == unique(group)[i]])

  }

  FH_bench <- unlist(result)

  if (overwrite == FALSE) {
    eblup_data_bench <- data.frame(Domain = object$ind$Domain)
    eblup_data_bench$Direct <- object$ind$Direct
    eblup_data_bench$FH <- object$ind$FH
    eblup_data_bench$FH_Bench <- FH_bench
    eblup_data_bench$Out <- object$ind$Out
    result <- eblup_data_bench
  } else {
    object$ind$FH_Bench <- FH_bench
    object$ind <- object$ind[, c("Domain", "Direct", "FH", "FH_Bench", "Out")]
    object["MSE"] <- list(NULL)

    message(strwrap(prefix = " ", initial = "",
                    "Please note that only point estimates are benchmarked.
                    Thus, the MSE element in the new emdi object is NULL."))
    result <- object
  }

  return(result)

}











######################################################################
# Argument checking
check_benchmark_arguments2 <- function(object, benchmark, share, type,
                                      overwrite) {
  if (!inherits(object, "fh")) {
    stop("Object needs to be fh object.")
  }

  povmap:::throw_class_error(object, "fh")

  if ((any(is.na(object$ind$FH)))) {
    stop(strwrap(prefix = " ", initial = "",
                 "If no predictions for out-of-sample domains are available,
                the benchmarking algorithm does not work."))
  }
  if (is.null(type) || !(is.character(type)) || !(type == "raking" ||
                                                  type == "ratio" ||
                                                  type == "MSE_adj")) {
    stop(strwrap(prefix = " ", initial = "",
                 "Type must be a character. The three options for types are
                 ''raking'', ''ratio'' and ''MSE_adj''."))
  }
  if ((is.null(object$MSE)) && type == "MSE_adj") {
    stop(strwrap(prefix = " ", initial = "",
                 "If no MSE estimates are available, ''MSE_adj'' benchmarking
                 does not work. The fh object has to contain MSE estimates.
                 Therefore set the MSE argument of the fh function to TRUE."))
  }
  if ((any(is.na(object$MSE$FH))) && type == "MSE_adj") {
    stop(strwrap(prefix = " ", initial = "",
                 "For the benchmarking type ''MSE_adj'' the MSE estimates of
                 the fh object must not contain NAs. If no MSE estimates for
                 out-of-sample domains are available,''MSE_adj'' benchmarking
                 does not work."))
  }
  # if (is.null(benchmark) || !(is.numeric(benchmark) &&
  #                             length(benchmark) == 1)) {
  #   stop(strwrap(prefix = " ", initial = "",
  #                "benchmark needs to be a single numeric value. See also
  #                help(benchmark)."))
  # }
  if (!is.vector(share) || length(share) != length(object$ind$Domain)) {
    stop(strwrap(prefix = " ", initial = "",
                 "share must be a vector with length equal to the number of
                 domains. See also help(benchmark)."))
  }
  if (any(is.na(share))) {
    stop("share must not contain NAs.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "overwrite must be a logical value. Set overwrite to TRUE or
                 FALSE. The default is set to FALSE. See also
                 help(benchmark)."))
  }
  if (!is.vector(benchmark) || length(benchmark) != length(object$ind$Domain)) {
    stop(strwrap(prefix = " ", initial = "",
                 "benchmark must be a vector with length equal to the number of
                 domains."))
  }


}


select_candidates <- function(dt,
                              y,
                              xvars,
                              N,
                              threshold){


  dt <- as.data.table(dt)

  cor_matrix <- cor(dt[, c(y, xvars), with = FALSE])

  # cor_dt <- data.table(vars = names(cor_matrix[,1]),
  #                      cor = cor_matrix[,1])
  #
  # top_vars <- cor_dt[order(abs(cor_dt$cor), decreasing = TRUE),]$vars[2:N+1]
  cor_matrix <- as.data.frame(cor_matrix)

  cor_matrix$names <- rownames(cor_matrix)

  cor_matrix <- as.data.table(cor_matrix)

  cor_matrix <- melt(cor_matrix,
                     id.vars = "names",
                     measure.vars = colnames(cor_matrix)[!(colnames(cor_matrix) %in% "names")])

  cor_matrix <- cor_matrix[!grepl(".1", cor_matrix$names),]
  cor_matrix <- cor_matrix[!grepl(".1", cor_matrix$variable),]

  ycor_dt <- cor_matrix[cor_matrix$variable == y,]
  xcor_dt <- cor_matrix[cor_matrix$variable != y,]

  xcor_dt <- xcor_dt[!is.na(xcor_dt$value),]

  xcor_dt <- xcor_dt[abs(xcor_dt$value) < 1,]

  drop_vars <- unique(xcor_dt[abs(xcor_dt$value) >= threshold,]$names)

  #### select the top variables that are on the candidate_vars list
  ycor_dt <- ycor_dt[order(-ycor_dt$value),]

  top_vars <- ycor_dt$names[!grepl(y, ycor_dt$names)]

  top_vars <- top_vars[!(top_vars %in% drop_vars)]

  ycor_dt <- ycor_dt[ycor_dt$names %in% top_vars,]

  top_vars <- ycor_dt$names[1:N]

  top_vars <- top_vars[!is.na(top_vars)]

  # if (top_vars %in% c("rai", "rri")){
  #
  #   top_vars <- top_vars[!(top_vars %in% "rri")]
  #
  # }

  return(top_vars)


}



#### some functions for variance smoothing
varsmoothie_king <- function(domain,
                             direct_var,
                             sampsize,
                             y){

  dt <- data.table(Domain = domain,
                   var = direct_var,
                   n = sampsize)

  dt$log_n <- log(dt$n)
  dt$log_var <- log(dt$var)

  lm_model <- lm(formula = log_var ~ log_n,
                 data = dt)

  dt$pred_var <- predict(lm_model, newdata = dt)

  dt$var_smooth <- exp(dt$pred_var) * exp(var(y, na.rm = TRUE)/2)

  return(dt[, c("Domain", "var_smooth"), with = F])

}




































































































#' dynamic_impact_matrix_variate
#' Generate specific summaries of numeric columns in a data frame
#'
#' @export
#'
dynamic_impact_matrix_variate <-  function(Y, X,
                                  initial_intervention_index,
                                  keep_evolution_static_for_prediction=TRUE,
                                  share_columns_var=TRUE,
                                  final_intervention_date=NULL,
                                  predefined_row_variance= NULL,
                                  predefined_col_variance= NULL,
                                  colum_names=NULL,
                                  row_names=NULL,
                                  return_stan_output=FALSE,
                                  return_df_format=TRUE,
                                  confidence_level=0.9,
                                  time_index = NULL,
                                  n_cores=NULL,
                                  .is_vector_model=FALSE,
                                  ...) {

  # browser()



  if(!is.null(final_intervention_date)) {
    Y <- Y[1:final_intervention_date,,]
    X <- X[1:final_intervention_date,,]
  }

  stan_data <- list()


  stan_data$R <- dim(Y)[2] # number of rows
  stan_data$K <- dim(Y)[3] # number of columns
  stan_data$P <- dim(X)[3] # number of regresors

  stan_data$N        <- dim(Y)[1]
  stan_data$N_before <- initial_intervention_index

  if(is.null(time_index)) {
    time_index <- 1:stan_data$N
  } else {

    if(length(time_index) != stan_data$N) {
      stop("The length of the time series(first dim of X and Y) is diferent form the length of time_index.")
    }

  }


  if(!is.null(confidence_level)) {

    if( (confidence_level >= 1) | (confidence_level <= 0) ) {

      stop("The confidence level must be in (0, 1).")
    }
  }



  if(!is.null(row_names)) {

    if(length(row_names) != stan_data$R) {
      stop("The length of row_names is different from the number of rows")
    }

  } else {
    row_names <- paste0("ROW_",1:stan_data$R)
  }

  if(!is.null(colum_names)) {

    if(length(colum_names) != stan_data$K) {
      stop("The length of colum_names is different from the number of columns")
    }

  } else {
    colum_names <- paste0("COL_",1:stan_data$K)
  }

  if(any(is.na(Y))) {
    stop("There should be no missing values in Y.")
  }

  if(any(is.na(X))) {
    stop("There should be no missing values in X.")
  }

  if(dim(Y)[2] != dim(X)[2]) {
    stop("The number of rows(second dim of X and Y) is diferent.")
  }

  if(dim(Y)[1] != dim(X)[1]) {
    stop("The length of the time series(first dim of X and Y) is diferent.")
  }

  if(stan_data$N  <=  initial_intervention_index) {
    stop("The initial_intervention_index is greater or equal the length of the time series(first dim of X and Y).")
  }

  stan_data$X <- X
  stan_data$y <- Y

  stan_data$use_discount_factor <- 0 # TODO no yet implemented


  stan_data$keep_theta_static_for_prediction <- as.integer(keep_evolution_static_for_prediction)
  stan_data$share_stations_var <- as.integer(share_columns_var)

  if(!is.null(predefined_row_variance)) {


    error_message <- paste0("predefined_row_variance must be ",
                            stan_data$R, "X", stan_data$R,
                            " semi-positive definite matrix")

    if( length(dim(predefined_row_variance)) != 2) {

      stop(error_message)

    }

    if( (dim(predefined_row_variance)[1] != stan_data$R) | (dim(predefined_row_variance)[2] != stan_data$R)  ) {
      stop(error_message)
    }


    stan_data$predefined_sensors_var  <- predefined_row_variance
    stan_data$use_predefined_sensors_var  <- 1

  } else {
    # browser()
    stan_data$predefined_sensors_var  <- diag(stan_data$R)
    stan_data$use_predefined_sensors_var  <- 0

  }

  # browser()
  if(!is.null(predefined_col_variance)) {

    error_message <- paste0("predefined_col_variance must be ",
                            stan_data$K, "X", stan_data$K,
                            " semi-positive definite matrix")

    if( length(dim(predefined_col_variance)) != 2) {

      stop(error_message)

    }

    if( (dim(predefined_col_variance)[1] != stan_data$K) | (dim(predefined_col_variance)[2] != stan_data$K)  ) {
      stop(error_message)
    }

    stan_data$predefined_stations_var  <- predefined_col_variance
    stan_data$use_predefined_stations_var  <- 1

  } else {

    stan_data$predefined_stations_var  <- diag(stan_data$K)
    stan_data$use_predefined_stations_var  <- 0


  }

  if(is.null(n_cores)) {
    n_cores = parallel::detectCores()
  }
  options(mc.cores = n_cores)

  # rstan_options(auto_write = TRUE)

  stan_result <- rstan::sampling(stanmodels$dynamic_matrix_variate_model, data = stan_data, ...)
  # browser()
  result <- build_result_df(stan_result         = stan_result,
                            Y = stan_data$y,
                            row_names           = row_names,
                            colum_names         = colum_names,
                            confidence_level    = confidence_level,
                            return_stan_output  = return_stan_output,
                            return_df_format    = return_df_format,
                            time_index          = time_index
                            )

  return(result)

}

matrix_to_df <- function(input_matrix, row_names, colum_names) {

  new_df <- data.frame(
    value= as.vector(input_matrix),
    row= rep(row_names, length(input_matrix)/length(row_names)),
    col=rep(colum_names, each = length(row_names))
  )

  return(new_df)

}

adjust_3d_array <- function(x) {

  if(length(dim(x)) == 3) {
    return(x)
  }

  if(length(dim(x)) == 4) {

      if(dim(x)[1] == 1) {

        temp_result <- x[1,,,]

        if(length(dim(temp_result)) == 2) {
          temp_result <- array(temp_result, dim = c(dim(temp_result)[1], 1, dim(temp_result)[2]))
        }

        return(temp_result)
      }
  }

  stop("The 3d array dimensions are consistent")

}

array_3d_to_df <- function(array_3d, row_names, colum_names, time_index) {

  array_3d <- adjust_3d_array(array_3d)


  result_df <- list()

  N <- dim(array_3d)[1]

  if(N != length(time_index)) {
    stop("The 3d array first dimension is diferent form the length of time_index")
  }

  for(i in 1:N) {

    index <- length(result_df)+1

    result_df[[index]] <- matrix_to_df(input_matrix=array_3d[i,,],
                                                     row_names=row_names,
                                                     colum_names=colum_names)

    result_df[[index]]$t <- time_index[i]

  }


  result_df <- do.call(rbind, result_df)

  return(result_df)

}

build_result_df <- function(stan_result, Y, row_names, colum_names,
                            confidence_level, return_stan_output,
                            return_df_format, time_index) {

  # browser()


  extract_data <- rstan::extract(stan_result)

  result <- list(
    Y                 = Y,
    Y_pred            = apply(extract_data$y_pred,            c(2,3,4), median),
    difference        = apply(extract_data$difference,        c(2,3,4), median),
    cumsum_difference = apply(extract_data$cumsum_difference, c(2,3,4), median),
    cumsum_only_after = apply(extract_data$cumsum_only_after, c(2,3,4), median)
  )

  if(return_stan_output) {
    result$stan_output <- stan_result
  }

  if(return_df_format) {

    # browser()

    result$Y_real_df            <- array_3d_to_df(result$Y,                 row_names, colum_names, time_index)
    result$Y_pred_df            <- array_3d_to_df(result$Y_pred,            row_names, colum_names, time_index)
    result$difference_df        <- array_3d_to_df(result$difference,        row_names, colum_names, time_index)
    result$cumsum_difference_df <- array_3d_to_df(result$cumsum_difference, row_names, colum_names, time_index)
    result$cumsum_only_after_df <- array_3d_to_df(result$cumsum_only_after, row_names, colum_names, time_index)

    result$Y_pred_df$Y_real <- result$Y_real_df$value
  }



  # browser()

  if(!is.null(confidence_level)) {

    result$Y_pred_ci <-  apply(extract_data$y_pred ,c(2,3,4), function(x, ci) {
      result <- bayestestR::hdi(x, ci=ci)
      return ( c(result$CI_low, result$CI_high))
    }, ci=confidence_level)

    result$difference_ci <-  apply(extract_data$difference, c(2,3,4), function(x, ci) {
      result <- bayestestR::hdi(x, ci=ci)
      return ( c(result$CI_low, result$CI_high))
    }, ci=confidence_level)


    result$cumsum_difference_ci <-  apply(extract_data$cumsum_difference, c(2,3,4), function(x, ci) {
      result <- bayestestR::hdi(x, ci=ci)
      return ( c(result$CI_low, result$CI_high))
    }, ci=confidence_level)

    result$cumsum_only_after_ci <-  apply(extract_data$cumsum_only_after, c(2,3,4), function(x, ci) {
      result <- bayestestR::hdi(x, ci=ci)
      return ( c(result$CI_low, result$CI_high))
    }, ci=confidence_level)


    if(return_df_format) {

      # browser()

      result$Y_pred_df$lower <- array_3d_to_df(result$Y_pred_ci[1,,,, drop=FALSE],
                                               row_names, colum_names, time_index)$value

      result$Y_pred_df$upper <- array_3d_to_df(result$Y_pred_ci[2,,,, drop=FALSE],
                                               row_names, colum_names, time_index)$value


      result$difference_df$lower <- array_3d_to_df(result$difference_ci[1,,,, drop=FALSE],
                                                   row_names, colum_names, time_index)$value

      result$difference_df$upper <- array_3d_to_df(result$difference_ci[2,,,, drop=FALSE],
                                                   row_names, colum_names, time_index)$value

      result$cumsum_difference_df$lower <- array_3d_to_df(result$cumsum_difference_ci[1,,,, drop=FALSE],
                                                          row_names, colum_names, time_index)$value

      result$cumsum_difference_df$upper <- array_3d_to_df(result$cumsum_difference_ci[2,,,, drop=FALSE],
                                                          row_names, colum_names, time_index)$value


      result$cumsum_only_after_df$lower <- array_3d_to_df(result$cumsum_only_after_ci[1,,,, drop=FALSE],
                                                          row_names, colum_names, time_index)$value

      result$cumsum_only_after_df$upper <- array_3d_to_df(result$cumsum_only_after_ci[2,,,, drop=FALSE],
                                                          row_names, colum_names, time_index)$value

    }

  }

  return(result)

}

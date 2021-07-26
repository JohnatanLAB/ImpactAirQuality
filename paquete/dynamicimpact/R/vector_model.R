#' dynamic_impact_vector_variate
#' Generate specific summaries of numeric columns in a data frame
#'
#' @export
#'
#' @include matrix_model.R
#'
dynamic_impact_vector_variate <- function(Y, X,
                                          initial_intervention_index,
                                          keep_evolution_static_for_prediction=TRUE,
                                          share_columns_var=FALSE,
                                          final_intervention_date=NULL,
                                          predefined_variance=NULL,
                                          variable_names=NULL,
                                          return_stan_output=FALSE,
                                          return_df_format=TRUE,
                                          confidence_level=0.9,
                                          time_index = NULL,
                                          n_cores=NULL,
                                          ...) {


    if( length(dim(Y)) != 2 ) {
        stop("Y must be a 2D of (Number of observations)X(length response)")
    }

    if( length(dim(X)) == 2 ) {
        X_array <- array(X, dim = c(dim(X)[1], 1, dim(X)[2]))
    } else if( length(dim(X)) == 3) {
        X_array <- X
    } else {
        stop("X must be a 2D or 3D array")
    }

    Y_array <- array(Y, dim = c(dim(Y)[1], 1, dim(Y)[2]))


    predefined_row_variance <- matrix(1, nrow=1, ncol=1)

    # result <- dynamicimpact::dynamic_impact_matrix_variate(
     result <-  dynamic_impact_matrix_variate(
    # result <-  dynamicimpact:::dynamic_impact_matrix_variate(
                            Y=Y_array,
                            X=X_array,
                            initial_intervention_index=initial_intervention_index,
                            keep_evolution_static_for_prediction=TRUE,
                            share_columns_var=share_columns_var,
                            final_intervention_date=final_intervention_date,
                            predefined_row_variance=predefined_row_variance,
                            predefined_col_variance=predefined_variance,
                            colum_names=variable_names,
                            row_names=NULL,
                            return_stan_output=return_stan_output,
                            return_df_format=return_df_format,
                            confidence_level=confidence_level,
                            time_index = time_index,
                            n_cores=n_cores,
                            ...
                )


    return(result)
}

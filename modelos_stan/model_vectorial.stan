//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//



functions {
  
  vector[] cumsum_vector(vector[] input_vector) {
    
    int N_SIZE = dims(input_vector)[1];

    vector[dims(input_vector)[2]] result_vector[N_SIZE];
    
    result_vector[1] = input_vector[1];

    for(t in 2:(N_SIZE) ) {
       result_vector[t] = result_vector[t - 1] + input_vector[t];

    }
    return(result_vector);
  }
  
}

// The input data is a vector 'y' of length 'N'.
data {
  
  
  
  int<lower=0> N; // total of observations
  int<lower=0> N_before;
  
  int<lower=0> K; // number of stations
  // int<lower=0> P; // number of regresors
  
  vector[K] Y[N];
  vector[K] X[N];
  
  
  int<lower=0, upper=1> use_predefined_stations_var; // use a predefined (pass by the user) as the observation variance of stations
  matrix[K , K]  predefined_stations_var; // Rstan does not allow to leave this parameter empty, even when is not in use.
  
  
  
  
}


transformed data {

  int N_after = N - N_before; // number of times after the intervention
  
}


// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  
  cov_matrix[use_predefined_stations_var ? 0 : K] sigma_entry_obs_stations; // V_t
  
  cov_matrix[K] level_sigma_stations; // V_t
  
  
  // vector[P] theta[N];
  vector[K] theta[N];
  
}



// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  vector[K] mu[N_before];
  
  //  https://mc-stan.org/docs/2_22/stan-users-guide/multivariate-outcomes.html
  for (t in 1:N_before) {
    // mu[t] = X[t] * theta[t];
    // mu[t] = (X[t] .* theta[t]) * rep_vector(1, P);
    // print(X[t] .* theta[t]);
    mu[t] = (X[t] .* theta[t]);
  }
    
  // print(mu[1]);
  
  if(use_predefined_stations_var == 0) {
    sigma_entry_obs_stations ~ inv_wishart(1.0*K, diag_matrix(rep_vector(100, K)) );
  }
  
  
  level_sigma_stations ~ inv_wishart(1.0*K, diag_matrix(rep_vector(100, K)) );
  
  // print(dims(theta[1:(N_after-1)]));
  // print(theta[1:(N_after-1)]);
  
  theta[1] ~  multi_normal(rep_vector(0, K) , level_sigma_stations);
  theta[2:N_before] ~ multi_normal(theta[1:(N_before-1)] , level_sigma_stations);
  
   // Y ~ multi_normal(theta * X , sigma_entry_obs_stations);
   if(use_predefined_stations_var) {
     Y[1:N_before] ~ multi_normal(mu[1:N_before] , predefined_stations_var);
   } else {
     Y[1:N_before] ~ multi_normal(mu[1:N_before] , sigma_entry_obs_stations);
   }
   

}

generated quantities {
  
  vector[K] Y_pred[N]; // matricial version of y
  vector[K] theta_pred[N]; // matricial version of theta
  vector[K] mu[N_after];
  
  vector[K] difference[N];
  vector[K] cumsum_difference[N]; // array of cumsum matrices, it starts of the initial time.
  vector[K] cumsum_only_after[N]; // array of cumsum matrices, it starts after the intervention.
  
  
  Y_pred[1:N_before] = Y[1:N_before];
  theta_pred[1:N_before] = theta[1:N_before];
  
  // theta_pred[(N_before+1):N] = multi_normal_rng(theta_pred[N_before:(N-1)], level_sigma_stations);
  
  theta_pred[(N_before+1):N] = multi_normal_rng(rep_array(theta_pred[N_before], N_after), level_sigma_stations);
  
  for (t in (N_before+1):N) {
    // mu[t] = X[t] * theta[t];
    // mu[t] = (X[t] .* theta[t]) * rep_vector(1, P);
    mu[t-N_before] = (X[t] .* theta_pred[t]);
  }
  
  if(use_predefined_stations_var) {
    
    Y_pred[(N_before+1):N] = multi_normal_rng(mu, predefined_stations_var);

  } else {
    Y_pred[(N_before+1):N] = multi_normal_rng(mu, sigma_entry_obs_stations);
  }
  
  for (t in 1:N) {
    difference[t] = Y[t] - Y_pred[t];
  }
  
  cumsum_only_after[1:N_before] = rep_array(rep_vector(0, K), N_before);
  cumsum_only_after[(N_before+1):N] = cumsum_vector(difference[(N_before+1):N]);
  
  cumsum_difference = cumsum_vector(difference);
  
  
  // for(t in (N_before+1):N) {
  //   
  //   if(use_predefined_stations_var) {
  //     
  //   } else {
  //     
  //   }
  //   
  // }
  
}




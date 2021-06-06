data {
int<lower=1> N; // number of data points
int<lower=1> N2; // the size of the new_X matrix for model prediction
int<lower=1> K; // the number of columns in the model matrix
int<lower=1> G; // the number of sites
real y[N]; // response variable
matrix[N,K] X; // the model matrix
int<lower=1, upper=G> site[N]; // site id
matrix[N2,K] new_X; // the matrix for the predicted values
}

parameters {
vector[K] beta; // the regression parameters (applied to X)
vector[G] gamma; // //the random site parameter
real<lower=0> sigma_e; //the error sd
real<lower=0> gamma_sigma; //sd of random site effect
}

 model {  
  sigma_e ~ cauchy(0,2.5);
  gamma_sigma ~ cauchy(0,2.5);

  for(i in 1:K){
    beta[i] ~ normal(0,5);
  }
  
  for(i in 1:G){
   gamma[i] ~ normal(0,gamma_sigma); 
  }

// The 4 predictors in X are (in order):
// lei_s1, del_ei_s, and t_scaled
  for (i in 1:N)  {
    y[i] ~ normal(beta[1]*X[i,1] + beta[2]*X[i,2] + 
                  beta[3]*X[i,3] + beta[4]*X[i,4] + gamma[site[i]], sigma_e);
}
}

generated quantities {
vector[N2] new_y_pred; //predicted values on evenly spaced major predictor variables
vector[N] y_pred;      //predicted values for evaluating fit
vector[N] log_lik;     //log-likelihood for calculating WAIC and loo
for(i in 1:N2){
   new_y_pred[i] = beta[1]*new_X[i,1] + beta[2]*new_X[i,2] +
                  beta[3]*new_X[i,3] + beta[4]*new_X[i,4]; //the y values predicted by the model
   }
for(i in 1:N){
  y_pred[i] = beta[1]*X[i,1] + beta[2]*X[i,2] +
                  beta[3]*X[i,3] + beta[4]*X[i,4] + gamma[site[i]];
   }
for (i in 1:N) log_lik[i] = normal_lpdf(y[i] | (beta[1]*X[i,1] + beta[2]*X[i,2] +
                  beta[3]*X[i,3] + beta[4]*X[i,4] + gamma[site[i]]), sigma_e);
}

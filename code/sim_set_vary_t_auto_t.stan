data {
int<lower=1> N;     // number of data points
int<lower=1> N2;    // the number of rows in model prediction data set
int<lower=1> G;     // the number of sites (grouping variable)
real y[N];          // response variable
real ei[N];         // EI predictor variable
real del_ei[N];     // delta-EI predictor variable
real t_scaled[N];   // time predictor variable
real auto_t[N];     // temporal autocorrelation variable
int<lower=1, upper=G> site[N]; // site predictor variable
real ei_p[N2];       // EI predictor variable for prediction data set
real del_ei_p[N2];   // delta-EI predictor variable for prediction data set
real t_scaled_p[N2]; // time predictor variable for prediction data set
int<lower=1, upper=G> site_p[N2]; // site predictor variable for prediction data set
}

parameters {
real beta_intercept;          // the intercept regression parameter
real beta_ei;                 // the ei regression parameter
real beta_del_ei;             // the del_ei regression parameter
real beta_auto_t;           // the auto_t regression parameter
vector[G] beta_t_scaled;           // the t regression parameter
vector[G] gamma;              // the random site parameter
real<lower=0> sigma_e;        // the error sd
real<lower=0> gamma_sigma;    // sd of random site effect
}

 model {  
  sigma_e ~ cauchy(0,2.5);
  gamma_sigma ~ cauchy(0,2.5);
  beta_intercept ~ normal(0,5);
  beta_ei ~ normal(0,5);
  beta_del_ei ~ normal(0,5);
  beta_auto_t ~ normal(0,5);

  for(i in 1:G){
   gamma[i] ~ normal(0,gamma_sigma); 
   beta_t_scaled[i] ~ normal(0,5);
  }

  for (i in 1:N)  {
    y[i] ~ normal(beta_intercept + beta_ei * ei[i] + beta_del_ei * del_ei[i] + 
                  beta_t_scaled[site[i]] * t_scaled[i] + beta_auto_t * auto_t[i] + gamma[site[i]], sigma_e);
}
}

generated quantities {
vector[N2] new_y_pred; // predicted values on evenly spaced major predictor variables
vector[N] y_pred;      // predicted values for evaluating fit
vector[N] log_lik;     // log-likelihood for calculating WAIC and loo
for(i in 1:N2){
   new_y_pred[i] = beta_intercept + beta_ei * ei_p[i] +
                   beta_del_ei * del_ei_p[i] +
                   beta_t_scaled[site_p[i]] * t_scaled_p[i] + gamma[site_p[i]];
   }
for(i in 1:N){
  y_pred[i] = beta_intercept + beta_ei * ei[i] + beta_del_ei * del_ei[i] +
                  beta_t_scaled[site[i]] * t_scaled[i] + beta_auto_t * auto_t[i] + gamma[site[i]];
   }
for (i in 1:N) log_lik[i] = normal_lpdf(y[i] | (beta_intercept +
                                                beta_ei * ei[i] +
                                                beta_del_ei * del_ei[i] +
                                                beta_t_scaled[site[i]] * t_scaled[i] + 
                                                beta_auto_t * auto_t[i] +
                                                gamma[site[i]]), sigma_e);
}

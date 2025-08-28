data {
  int<lower=0> n1, n2, p;
  array[n1] vector[p] X1;
  vector[n1] y1;
  array[n2] vector[p] X2;
}

transformed data {
  real delta = 1e-9;
  int<lower=1> n = n1+n2;
  array[n] vector[p] X;
  
  for(i in 1:n1){
    X[i] = X1[i];
  }
  for(j in 1:n2){
    X[n1 + j] = X2[j];
  }
}

parameters {
  vector[p] beta;
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> sigma;
  vector[n] eta;
}

transformed parameters {
  vector[n] mu;
  vector[n] f;
  matrix[n, n] K;
  matrix[n, n] L_K;
  
  for(i in 1:n){
    mu[i] = dot_product(X[i], beta);
  }
  
  K = gp_exp_quad_cov(X, alpha, rho);
  
  for(i in 1:n){
    K[i, i] = K[i, i] + delta;
  }
  
  L_K = cholesky_decompose(K);
  f = mu + L_K * eta;
}

model {
  beta ~ normal(0, 1);
  alpha ~ normal(0, 1);
  rho ~ inv_gamma(5, 5);
  sigma ~ normal(0, 1);
  eta ~ std_normal();
  
  y1 ~ normal(f[1:n1], sigma);
}

generated quantities {
  vector[n2] y2;
  vector[n2] f2;
  
  f2 = f[(n1+1):n];

  for(i in 1:n2){
    y2[i] = normal_rng(f2[i], sigma);
  }  
}

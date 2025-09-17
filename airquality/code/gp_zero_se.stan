data {
  int<lower=1> n1, n2, p;
  array[n1] vector[p] X1;
  vector[n1] y1;
  array[n2] vector[p] X2;
}

transformed data {
  real delta = 1e-9;
  int<lower=1> n = n1 + n2;
  array[n] vector[p] X;
  
  for (i in 1:n1) {
    X[i] = X1[i];
  }
  for (j in 1:n2) {
    X[n1 + j] = X2[j];
  }
}

parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> sigma;
  vector[n] eta;
}

transformed parameters {
  vector[n] f;
  {
    matrix[n, n] L_K;
    matrix[n, n] K = gp_exp_quad_cov(X, alpha, rho);
    
    for (i in 1:n) {
      K[i, i] = K[i, i] + delta;
    }
    
    L_K = cholesky_decompose(K);
    f = L_K * eta;
  }
}

model {
  alpha ~ std_normal();
  rho ~ inv_gamma(5, 5);
  sigma ~ std_normal();
  eta ~ std_normal();
  
  y1 ~ normal(f[1:n1], sigma);
}

generated quantities {
  vector[n2] y_test;
  for (i in 1:n2) {
    y_test[i] = normal_rng(f[n1 + i], sigma);
  }
}

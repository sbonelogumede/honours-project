data {
  int<lower=1> n1;
  int<lower=1> p;
  matrix[n1, p] X1;
  vector[n1] y1;
  
  int<lower=1> n2;
  matrix[n2, p] X2;
}

transformed data {
  real delta = 1e-9;
}

parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  vector[p] beta;
}

transformed parameters {
  vector[n1] mu = X1 * beta;
  matrix[n1, n1] K = gp_exp_quad_cov(X1, alpha, rho);
  matrix[n1, n1] L; = cholesky_decompose(K);
  
}

model {
  alpha ~ std_normal();
  rho ~ inv_gamma(1, 1);
  beta ~ std_normal();
}

generated quantities {
  vector[n2] y2;
  y2 = multi_normal_cholesky(mu, Lk);
}

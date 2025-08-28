data {
  int<lower=1> N1;           // number of observed data points
  array[N1] real x1;         // observed inputs
  vector[N1] y1;             // observed outputs
  int<lower=1> N2;           // number of points to predict
  array[N2] real x2;         // future inputs
}

transformed data {
  int<lower=1> N = N1 + N2;
  array[N] real x;
  for (n1 in 1:N1) 
    x[n1] = x1[n1];
  for (n2 in 1:N2) 
    x[N1 + n2] = x2[n2];
}

parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
  vector[N] eta;             // latent function basis
}

transformed parameters {
  vector[N] f;      //latent fn values 
  {
    matrix[N,N] K = gp_exp_quad_cov(x, alpha, rho) +
                    diag_matrix(rep_vector(1e-9, N)); // jitter for stability
    matrix[N,N] L_K = cholesky_decompose(K);
    f = L_K * eta;
  }
}

model {
  // priors
  rho ~ normal(0, 3);
  alpha ~ normal(0, 1);
  sigma ~ normal(0, 1);
  eta ~ normal(0, 1);

  // likelihood
  y1 ~ normal(f[1:N1], sigma); //observations are modeled directly conditional on f
}

generated quantities { //forecast 
  vector[N2] y2;
  for (n2 in 1:N2)
    y2[n2] = normal_rng(f[N1 + n2], sigma);
}


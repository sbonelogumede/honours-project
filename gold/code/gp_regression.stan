data {
  int<lower=1> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real<lower=0> alpha;     // signal sd
  real<lower=0> rho;       // lengthscale
  real<lower=0> sigma;     // noise sd
  vector[N] f;             // latent function
}
model {
  matrix[N, N] K;
for (i in 1:N) {
  for (j in 1:N) {
    K[i, j] = alpha^2 * exp(-0.5 * square(x[i] - x[j]) / square(rho));
  }
}
for (i in 1:N)
  K[i, i] += sigma^2;  // add noise to the diagonal

  for (i in 1:N) K[i, i] += sigma^2;
  f ~ multi_normal(rep_vector(0, N), K);
  y ~ normal(f, sigma);
  // Priors: weakly informative
  alpha ~ normal(0, 2);
  rho ~ normal(0, 10);
  sigma ~ normal(0, 2);
}
generated quantities {
  vector[N] y_rep;
  for (i in 1:N) {
    y_rep[i] = normal_rng(f[i], sigma);
  }
}

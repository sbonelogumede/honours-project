data {
  int<lower=1> N;            // number of observations
  int<lower=1> N_pred;       // number of prediction points
  vector[N] x;               // training inputs
  vector[N] y;               // training outputs
  vector[N_pred] x_pred;     // prediction inputs
}
parameters {
  real<lower=0> alpha;       // signal variance
  real<lower=0> rho;         // length scale
  real<lower=0> sigma;       // noise
}
model {
  matrix[N, N] K;

  // build covariance for training
  for (i in 1:N) {
    for (j in 1:N) {
      K[i, j] = alpha^2 * exp(-0.5 * square((x[i] - x[j]) / rho));
    }
    K[i,i] += sigma^2;
  }

  y ~ multi_normal(rep_vector(0, N), K);
}
generated quantities {
  matrix[N, N] K;
  matrix[N_pred, N] K_star;
  matrix[N_pred, N_pred] K_starstar;
  vector[N_pred] f_mean;
  matrix[N_pred, N_pred] f_cov;

  for (i in 1:N) {
    for (j in 1:N) {
      K[i, j] = alpha^2 * exp(-0.5 * square((x[i] - x[j]) / rho));
    }
    K[i,i] += sigma^2;
  }

  for (i in 1:N_pred) {
    for (j in 1:N) {
      K_star[i,j] = alpha^2 * exp(-0.5 * square((x_pred[i] - x[j]) / rho));
    }
  }

  for (i in 1:N_pred) {
    for (j in 1:N_pred) {
      K_starstar[i,j] = alpha^2 * exp(-0.5 * square((x_pred[i] - x_pred[j]) / rho));
    }
    K_starstar[i,i] += sigma^2;
  }

  f_mean = K_star * inverse(K) * y;
  f_cov  = K_starstar - K_star * inverse(K) * K_star';
}

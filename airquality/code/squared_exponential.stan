functions {
  matrix se_kernel(matrix X1, matrix X2, real alpha, real rho) {
    int n1 = rows(X1);
    int n2 = rows(X2);
    matrix[n1, n2] K;

    for (i in 1:n1) {
      for (j in 1:n2) {
        // squared Euclidean distance between rows i and j
        real sqdist = dot_self(X1[i] - X2[j]);
        K[i, j] = square(alpha) * exp(-0.5 * sqdist / square(rho));
      }
    }
    return K;
  }
}

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

model {
  // Priors
  alpha ~ normal(0, 1);
  rho ~ normal(24, 12); // half-normal since <lower=0>
  beta ~ normal(0, 1);

  // Build covariance for training
  matrix[n1, n1] K11 = se_kernel(X1, X1, alpha, rho)
                       + diag_matrix(rep_vector(delta, n1));
  vector[n1] mu1 = X1 * beta;

  // Likelihood
  y1 ~ multi_normal(mu1, K11);
}

generated quantities {
  vector[n2] y2;

  // Covariance blocks
  matrix[n1, n1] K11 = se_kernel(X1, X1, alpha, rho)
                       + diag_matrix(rep_vector(delta, n1));
  matrix[n1, n2] K12 = se_kernel(X1, X2, alpha, rho);
  matrix[n2, n2] K22 = se_kernel(X2, X2, alpha, rho)
                       + diag_matrix(rep_vector(delta, n2));

  // Means
  vector[n1] mu1 = X1 * beta;
  vector[n2] mu2 = X2 * beta;

  // Cholesky factor of K11
  matrix[n1, n1] L = cholesky_decompose(K11);

  // Predictive mean
  vector[n1] y1_centered = y1 - mu1;
  vector[n1] v = mdivide_left_tri_low(L, y1_centered);
  vector[n1] w = mdivide_right_tri_low(v', L)';  // equivalent to solve(L', v)
  vector[n2] pred_mean = mu2 + K12' * w;

  // Predictive covariance
  matrix[n1, n2] A = mdivide_left_tri_low(L, K12);
  matrix[n2, n2] pred_cov = K22 - A' * A;

  // Draw sample
  y2 = multi_normal_rng(pred_mean, pred_cov);
}

functions {
  matrix se_kernel(matrix X1, matrix X2, real alpha, vector rho) {
    int n1 = rows(X1);
    int n2 = rows(X2);
    int p = cols(X1);
    matrix[n1, n2] K;
    for (i in 1:n1) {
      for (j in 1:n2) {
        // Compute squared distance with ARD (separate lengthscale per dimension)
        real sqdist = 0;
        for (d in 1:p) {
          sqdist += square((X1[i, d] - X2[j, d]) / rho[d]);
        }
        K[i, j] = square(alpha) * exp(-0.5 * sqdist);
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
parameters {
  real<lower=0> alpha;      // Output scale
  vector<lower=0>[p] rho;   // Lengthscale per predictor (ARD)
  real<lower=0> sigma;      // Noise std dev
  vector[p] beta;           // Regression coefficients
}
model {
  // Priors - calibrated for standardized data
  alpha ~ normal(1, 0.5);           // Output scale around 1
  rho[1] ~ normal(0, 0.5);          // Intercept - not used in kernel, keep tight
  rho[2] ~ inv_gamma(5, 50);        // Time: needs larger lengthscale (mode ~10)
  for (i in 3:p) {
    rho[i] ~ inv_gamma(3, 3);       // Other predictors: mode around 1
  }
  sigma ~ normal(0, 0.5);           // Tighter noise prior
  beta ~ normal(0, 3);              // Tighter prior for standardized predictors
  
  // Build covariance for training
  matrix[n1, n1] K11 = se_kernel(X1, X1, alpha, rho)
                       + diag_matrix(rep_vector(square(sigma), n1));
  vector[n1] mu1 = X1 * beta;
  // Likelihood
  y1 ~ multi_normal(mu1, K11);
}
generated quantities {
  vector[n2] y2;
  // Covariance blocks
  matrix[n1, n1] K11 = se_kernel(X1, X1, alpha, rho)
                       + diag_matrix(rep_vector(square(sigma), n1));
  matrix[n1, n2] K12 = se_kernel(X1, X2, alpha, rho);
  matrix[n2, n2] K22 = se_kernel(X2, X2, alpha, rho)
                       + diag_matrix(rep_vector(square(sigma), n2));
  // Means
  vector[n1] mu1 = X1 * beta;
  vector[n2] mu2 = X2 * beta;
  // Cholesky factor of K11
  matrix[n1, n1] L = cholesky_decompose(K11);
  // Predictive mean
  vector[n1] y1_centered = y1 - mu1;
  vector[n1] v = mdivide_left_tri_low(L, y1_centered);
  vector[n1] w = mdivide_right_tri_low(v', L)';
  vector[n2] pred_mean = mu2 + K12' * w;
  // Predictive covariance
  matrix[n1, n2] A = mdivide_left_tri_low(L, K12);
  matrix[n2, n2] pred_cov = K22 - A' * A;
  // Draw sample
  y2 = multi_normal_rng(pred_mean, pred_cov);
  
  vector[n2] gp_component = pred_mean - mu2;
}

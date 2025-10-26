functions {
  // Vectorized periodic kernel using precomputed sin/cos
  matrix periodic_kernel(matrix X1, matrix X2, real alpha, real rho, real T) {
    int n1 = rows(X1);
    int n2 = rows(X2);
    int p = cols(X1);
    matrix[n1, p] S1 = sin(pi() * X1 / T);
    matrix[n1, p] C1 = cos(pi() * X1 / T);
    matrix[n2, p] S2 = sin(pi() * X2 / T);
    matrix[n2, p] C2 = cos(pi() * X2 / T);
    // squared Euclidean distance in sine–cosine space
    matrix[n1, n2] dist2 = rep_matrix(0.0, n1, n2);
    for (d in 1:p) {
      vector[n1] s1 = S1[, d];
      vector[n2] s2 = S2[, d];
      vector[n1] c1 = C1[, d];
      vector[n2] c2 = C2[, d];
      // Use outer subtraction via broadcasting
      dist2 += square(s1 * ones_row_vector(n2) - ones_vector(n1) * s2')
             + square(c1 * ones_row_vector(n2) - ones_vector(n1) * c2');
    }
    return square(alpha) * exp(-0.5 * dist2 / square(rho));
  }
}
data {
  int<lower=1> n1;          // number of training observations
  int<lower=1> p;           // number of predictors
  matrix[n1, p] X1;         // training inputs
  vector[n1] y1;            // training outputs
  int<lower=1> n2;          // number of test observations
  matrix[n2, p] X2;         // test inputs
  real<lower=0> T;          // Period (e.g., 24 for hourly data)
}
parameters {
  real<lower=0> alpha;      // Output scale (marginal std dev)
  real<lower=0> rho;        // Lengthscale in periodic space
  real<lower=0> sigma;      // Observational noise std dev
  vector[p] beta;           // Regression coefficients
}
model {
  // Priors - calibrated for standardized data
  alpha ~ normal(1, 0.5);         // Output scale around 1 for standardized data
  rho ~ inv_gamma(3, 3);          // More flexible lengthscale, mode around 1
  sigma ~ normal(0, 0.5);         // Tighter noise prior
  beta ~ normal(0, 3);            // Tighter prior for standardized predictors
  
  // Covariance for training
  matrix[n1, n1] K11 = periodic_kernel(X1, X1, alpha, rho, T)
                       + diag_matrix(rep_vector(square(sigma), n1));  // FIXED: square(sigma)
  vector[n1] mu1 = X1 * beta;
  // Likelihood
  y1 ~ multi_normal(mu1, K11);
}
generated quantities {
  vector[n2] y2;
  // Covariance blocks
  matrix[n1, n1] K11 = periodic_kernel(X1, X1, alpha, rho, T)
                       + diag_matrix(rep_vector(square(sigma), n1));  // FIXED: square(sigma)
  matrix[n1, n2] K12 = periodic_kernel(X1, X2, alpha, rho, T);
  matrix[n2, n2] K22 = periodic_kernel(X2, X2, alpha, rho, T)
                       + diag_matrix(rep_vector(square(sigma), n2));  // FIXED: square(sigma)
  // Means
  vector[n1] mu1 = X1 * beta;
  vector[n2] mu2 = X2 * beta;
  // Cholesky decomposition
  matrix[n1, n1] L = cholesky_decompose(K11);
  // Predictive mean
  vector[n1] y1_centered = y1 - mu1;
  vector[n1] v = mdivide_left_tri_low(L, y1_centered);
  vector[n1] w = mdivide_right_tri_low(v', L)';
  vector[n2] pred_mean = mu2 + K12' * w;
  // Predictive covariance
  matrix[n1, n2] A = mdivide_left_tri_low(L, K12);
  matrix[n2, n2] pred_cov = K22 - A' * A;
  // Predictive draws
  y2 = multi_normal_rng(pred_mean, pred_cov);
  
  vector[n2] gp_component = pred_mean - mu2;
}

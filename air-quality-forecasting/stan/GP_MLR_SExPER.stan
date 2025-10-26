functions {
  // SE kernel with ARD
  matrix se_kernel(matrix X1, matrix X2, real alpha_se, vector rho_se) {
    int n1 = rows(X1);
    int n2 = rows(X2);
    int p = cols(X1);
    matrix[n1, n2] K;
    for (i in 1:n1) {
      for (j in 1:n2) {
        real sqdist = 0;
        for (d in 1:p) {
          sqdist += square((X1[i, d] - X2[j, d]) / rho_se[d]);
        }
        K[i, j] = square(alpha_se) * exp(-0.5 * sqdist);
      }
    }
    return K;
  }
  
  // Periodic kernel
  matrix periodic_kernel(matrix X1, matrix X2, real alpha_per, real rho_per, real T) {
    int n1 = rows(X1);
    int n2 = rows(X2);
    int p = cols(X1);
    matrix[n1, p] S1 = sin(pi() * X1 / T);
    matrix[n1, p] C1 = cos(pi() * X1 / T);
    matrix[n2, p] S2 = sin(pi() * X2 / T);
    matrix[n2, p] C2 = cos(pi() * X2 / T);
    matrix[n1, n2] dist2 = rep_matrix(0.0, n1, n2);
    for (d in 1:p) {
      vector[n1] s1 = S1[, d];
      vector[n2] s2 = S2[, d];
      vector[n1] c1 = C1[, d];
      vector[n2] c2 = C2[, d];
      dist2 += square(s1 * ones_row_vector(n2) - ones_vector(n1) * s2')
             + square(c1 * ones_row_vector(n2) - ones_vector(n1) * c2');
    }
    return square(alpha_per) * exp(-0.5 * dist2 / square(rho_per));
  }
  
  // Product of SE and Periodic kernels
  matrix product_kernel(matrix X1, matrix X2, 
                        real alpha_se, vector rho_se,
                        real alpha_per, real rho_per, real T) {
    return se_kernel(X1, X2, alpha_se, rho_se) .* 
           periodic_kernel(X1, X2, alpha_per, rho_per, T);
  }
}

data {
  int<lower=1> n1;
  int<lower=1> p;
  matrix[n1, p] X1;
  vector[n1] y1;
  int<lower=1> n2;
  matrix[n2, p] X2;
  real<lower=0> T;          // Period (e.g., 24 for hourly data)
}

parameters {
  // SE kernel parameters
  real<lower=0> alpha_se;
  vector<lower=0>[p] rho_se;
  
  // Periodic kernel parameters
  real<lower=0> alpha_per;
  real<lower=0> rho_per;
  
  // Noise and regression
  real<lower=0> sigma;
  vector[p] beta;
}

model {
  // Priors for SE kernel
  alpha_se ~ normal(1, 0.5);
  rho_se[1] ~ normal(0, 0.5);          // Intercept
  rho_se[2] ~ inv_gamma(5, 50);        // Time
  for (i in 3:p) {
    rho_se[i] ~ inv_gamma(3, 3);       // Other predictors
  }
  
  // Priors for Periodic kernel
  alpha_per ~ normal(1, 0.5);
  rho_per ~ inv_gamma(3, 3);
  
  // Other priors
  sigma ~ normal(0, 0.5);
  beta ~ normal(0, 3);
  
  // Build covariance
  matrix[n1, n1] K11 = product_kernel(X1, X1, alpha_se, rho_se, 
                                      alpha_per, rho_per, T)
                       + diag_matrix(rep_vector(square(sigma), n1));
  vector[n1] mu1 = X1 * beta;
  
  // Likelihood
  y1 ~ multi_normal(mu1, K11);
}

generated quantities {
  vector[n2] y2;
  
  // Covariance blocks
  matrix[n1, n1] K11 = product_kernel(X1, X1, alpha_se, rho_se,
                                      alpha_per, rho_per, T)
                       + diag_matrix(rep_vector(square(sigma), n1));
  matrix[n1, n2] K12 = product_kernel(X1, X2, alpha_se, rho_se,
                                      alpha_per, rho_per, T);
  matrix[n2, n2] K22 = product_kernel(X2, X2, alpha_se, rho_se,
                                      alpha_per, rho_per, T)
                       + diag_matrix(rep_vector(square(sigma), n2));
  
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
functions {
  // Squared Exponential (RBF) kernel - operates ONLY on time dimension (scalar)
  matrix se_kernel(vector t1, vector t2, real alpha, real rho) {
    int n1 = rows(t1);
    int n2 = rows(t2);
    matrix[n1, n2] K;
    
    for (i in 1:n1) {
      for (j in 1:n2) {
        // Squared Euclidean distance for scalar time values
        real sqdist = square((t1[i] - t2[j]) / rho);
        K[i, j] = square(alpha) * exp(-0.5 * sqdist);
      }
    }
    return K;
  }
}

data {
  int<lower=1> n1;                // Number of training observations
  int<lower=1> p_mean;            // Number of predictors for mean function (includes intercept)
  matrix[n1, p_mean] X1_mean;     // Training covariates for mean (with intercept)
  vector[n1] X1_time;             // Training time values (for kernel only)
  vector[n1] y1;                  // Training outputs
  
  int<lower=1> n2;                // Number of test observations
  matrix[n2, p_mean] X2_mean;     // Test covariates for mean (with intercept)
  vector[n2] X2_time;             // Test time values (for kernel only)
}

parameters {
  real<lower=0> alpha;            // Output scale (marginal std dev)
  real<lower=0> rho;              // Lengthscale for time
  real<lower=0> sigma;            // Observational noise std dev
  vector[p_mean] beta;            // Regression coefficients (beta[1] is intercept)
}

model {
  // Priors
  alpha ~ normal(1, 0.5);         // Output scale
  rho ~ inv_gamma(5, 50);         // Lengthscale for time
  sigma ~ normal(0, 0.5);         // Noise prior
  beta[1:p_mean] ~ normal(0, 3);  // Beta coefficients
  
  // Covariance for training - kernel ONLY on time
  matrix[n1, n1] K11 = se_kernel(X1_time, X1_time, alpha, rho)
                       + diag_matrix(rep_vector(square(sigma), n1));
  
  // Mean function uses ALL covariates (with intercept)
  vector[n1] mu1 = X1_mean * beta;
  
  // Likelihood
  y1 ~ multi_normal(mu1, K11);
}

generated quantities {
  vector[n1] y1_fitted;           // Fitted values (training data posterior predictive)
  vector[n2] y2;                  // Forecasts (test data predictions)
  vector[n2] y2_mean;             // Mean function component
  vector[n2] gp_component;        // GP component (deviation from mean)
  
  {
    // Covariance blocks - kernel ONLY on time
    matrix[n1, n1] K11 = se_kernel(X1_time, X1_time, alpha, rho)
                         + diag_matrix(rep_vector(square(sigma), n1));
    matrix[n1, n2] K12 = se_kernel(X1_time, X2_time, alpha, rho);
    matrix[n2, n2] K22 = se_kernel(X2_time, X2_time, alpha, rho)
                         + diag_matrix(rep_vector(square(sigma), n2));
    
    // Mean functions use ALL covariates (with intercept)
    vector[n1] mu1 = X1_mean * beta;
    vector[n2] mu2 = X2_mean * beta;
    
    // Fitted values: posterior predictive for training data
    y1_fitted = multi_normal_rng(mu1, K11);
    
    // Cholesky decomposition for forecasts
    matrix[n1, n1] L = cholesky_decompose(K11);
    
    // Forecasts: use OBSERVED y1 data (from data block) for predictions
    vector[n1] y1_centered = y1 - mu1;
    vector[n1] v = mdivide_left_tri_low(L, y1_centered);
    vector[n1] w = mdivide_right_tri_low(v', L)';
    vector[n2] pred_mean = mu2 + K12' * w;
    
    // Predictive covariance
    matrix[n1, n2] A = mdivide_left_tri_low(L, K12);
    matrix[n2, n2] pred_cov = K22 - A' * A;
    
    // Predictive draws
    y2 = multi_normal_rng(pred_mean, pred_cov);
    
    // Decompose into mean and GP components for interpretation
    y2_mean = mu2;
    gp_component = pred_mean - mu2;
  }
}

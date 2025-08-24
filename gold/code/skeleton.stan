data {
  int<lower=1> N;          // training size
  vector[N] x;             // standardized inputs (time)
  vector[N] y;             // training outputs
  int<lower=1> N_new;      // test size
  vector[N_new] x_new;     // standardized test inputs
}
parameters {
  real               mu;           // mean
  real<lower=0>      sigma_f;      // signal std
  real<lower=0>      ell;          // length-scale
  real<lower=0>      sigma_n;      // noise std
}
model {
  // Squared-exponential (RBF) kernel for training inputs
  matrix[N,N] K  = cov_exp_quad(x, square(sigma_f), ell);
  matrix[N,N] Ky = K + diag_matrix(rep_vector(square(sigma_n) + 1e-8, N)); // jitter

  // Weakly-informative priors
  mu      ~ normal(0, 2);
  sigma_f ~ student_t(3, 0, 1);   // half-Student-t via <lower=0>
  ell     ~ lognormal(0, 1);      // length-scale on standardized time
  sigma_n ~ student_t(3, 0, 1);

  // Marginal GP likelihood with Cholesky
  y ~ multi_normal_cholesky(rep_vector(mu, N), cholesky_decompose(Ky));
}
generated quantities {
  vector[N_new] y_new_mean;  // predictive mean at x_new (observations)
  vector[N_new] y_new_sd;    // predictive sd  at x_new (observations)

  {
    matrix[N,N] K   = cov_exp_quad(x, square(sigma_f), ell);
    matrix[N,N] Ky  = K + diag_matrix(rep_vector(square(sigma_n) + 1e-8, N));
    matrix[N,N] L   = cholesky_decompose(Ky);
    vector[N]   yc  = y - rep_vector(mu, N);

    // Cross- and test covariances
    matrix[N,N_new]     Ks  = cov_exp_quad(x, x_new, square(sigma_f), ell);
    matrix[N_new,N_new] Kss = cov_exp_quad(x_new, square(sigma_f), ell)
                              + diag_matrix(rep_vector(1e-8, N_new)); // jitter

    // alpha = Ky^{-1} * yc via triangular solves
    vector[N] alpha = mdivide_left_tri_low(L, yc);
    alpha = mdivide_left_tri_low(L', alpha);

    // Predictive mean and variance (for observations)
    y_new_mean = rep_vector(mu, N_new) + (Ks' * alpha);

    matrix[N,N_new] v = mdivide_left_tri_low(L, Ks);
    vector[N_new] var_diag = diagonal(Kss) - columns_dot_self(v);
    y_new_sd = sqrt(fmax(1e-12, var_diag + square(sigma_n)));
  }
}

// https://betanalpha.github.io/assets/case_studies/gaussian_processes.html
// https://users.aalto.fi/~ave/casestudies/Motorcycle/motorcycle_gpcourse.html#49_Variational_inference

functions {
  vector gp_pred_rng(array[] real x2,
                     vector y1,
                     array[] real x1,
                     real sigma_f,
                     real lengthscale_f,
                     real sigma,
                     real jitter) {
    int N1 = rows(y1);
    int N2 = size(x2);
    vector[N2] f2;
    {
      matrix[N1, N1] L_K;
      vector[N1] K_div_y1;
      matrix[N1, N2] k_x1_x2;
      matrix[N1, N2] v_pred;
      vector[N2] f2_mu;
      matrix[N2, N2] cov_f2;
      matrix[N1, N1] K;
      K = gp_exp_quad_cov(x1, sigma_f, lengthscale_f);
      for (n in 1:N1)
        K[n, n] = K[n,n] + square(sigma);
      L_K = cholesky_decompose(K);
      K_div_y1 = mdivide_left_tri_low(L_K, y1);
      K_div_y1 = mdivide_right_tri_low(K_div_y1', L_K)';
      k_x1_x2 = gp_exp_quad_cov(x1, x2, sigma_f, lengthscale_f);
      f2_mu = (k_x1_x2' * K_div_y1);
      v_pred = mdivide_left_tri_low(L_K, k_x1_x2);
      cov_f2 = gp_exp_quad_cov(x2, sigma_f, lengthscale_f) - v_pred' * v_pred;

      f2 = multi_normal_rng(f2_mu, add_diag(cov_f2, rep_vector(jitter, N2)));
    }
    return f2;
  }
}

data {
  int<lower=1> N_obs;
  vector[N_obs] y_obs;
  vector[N_obs] x_obs;
  int<lower=0> N2;
  vector[N2] x2;
}

transformed data {
  real ymean = mean(y_obs);
  real xmean = mean(x_obs);
  real xsd = sd(x_obs);
  real ysd = sd(y_obs);
  vector[N_obs] yn = (y_obs - ymean) / ysd;
  array[N_obs] real xn = to_array_1d((x_obs - xmean) / xsd);
  array[N2] real x2n = to_array_1d((x2 - xmean) / xsd);
}

parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
}

model {
  // priors
  rho ~ normal(0, 1);
  alpha ~ normal(0, 1);
  sigma ~ normal(0, 1);
  
  matrix[N_obs, N_obs] cov =   gp_exp_quad_cov(xn, alpha, rho)
                             + diag_matrix(rep_vector(square(sigma), N_obs));
  matrix[N_obs, N_obs] L_cov = cholesky_decompose(cov);

  y_obs ~ multi_normal_cholesky(rep_vector(0, N_obs), L_cov);
}

generated quantities {
  //real y_predict[N_predict] = normal_rng(f_predict, sigma);
    // function scaled back to the original scale
  vector[N2] f = gp_pred_rng(x2n, yn, xn, alpha, rho, sigma, 1e-9) * ysd + ymean;
  real sigma_y = sigma * ysd;
}


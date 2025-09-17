data {
  int<lower=0> n1, n2, p;
  matrix[n1, p] X1;
  vector[n1] y1;
  
  matrix[n2, p] X2;
}

parameters {
  vector[p] beta;
  real<lower=0> sigma;
}

model {
  y1 ~ normal(rep_vector(0, n1), sigma);
}

generated quantities {
  vector[n2] y2;

  for(i in 1:n2){
    y2[i] = normal_rng(0, sigma);
  }  
}

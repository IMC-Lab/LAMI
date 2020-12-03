functions {
  /*
   * compute a latent Gaussian process
   * args:
   *   x: the GP predictors
   *   alpha: the gp marginal SD parameter
   *   rho: the length-scales for each dimension of x
   *   eta: independent standard normal variates
   * returns:
   *   a vector of GP predictions at predictor locations x
   */
  vector gp_exp_quad_ARD(vector[] x, real alpha, vector rho, vector eta) {
    int N = size(x);
    real delta = 1e-8;
    matrix[N, N] K;
    real alpha_sq = square(alpha);

    for (i in 1:(N-1)) {
      K[i, i] = alpha_sq + delta;
      for (j in (i + 1):N) {
        K[i, j] = alpha_sq * exp(-0.5 * dot_self((x[i] - x[j]) ./ rho));
        K[j, i] = K[i, j];
      }
    }
    K[N, N] = alpha_sq + delta;
    return cholesky_decompose(K) * eta;
  }
}

data {
  int<lower=1> N;  // total number of data points
  int<lower=1> N_grid;   // number of data points per trial (grid size)
  int<lower=1> N_group;  // number of groups (e.g., subjects)
  int<lower=1> D;        // number of dimensions

  int<lower=1, upper=N_group> idx_group[N];    // group index for each trial
  int<lower=1, upper=N_grid> idx_grid[N];  // grid index for each trial

  vector[D] x[N_grid];             // input data (same across trials)
  int<lower=0> y[N];  // output data (counts)
  
}

parameters {
  // Group-level parameters (non-centered)
  real z_intercept[N_group];  // standardized intercepts
  vector[D] z_rho[N_group];   // standardized length scales
  real z_alpha[N_group];      // standardized marginal sds
  vector[N_grid] z_eta[N_group];  // std normal random variates


  // Population-level parameters  
  real m_intercept;            // mean of population intercept
  real<lower=0> sd_intercept;  // sd of population intercept
  vector<lower=0>[D] md_rho;   // median of population rho
  vector<lower=0>[D] sd_rho;   // sd of population rho
  real<lower=0> md_alpha;      // median of population alpha
  real<lower=0> sd_alpha;      // sd of population alpha
  vector[N_grid] m_eta;            // mean of GP mean variates
  vector<lower=0>[N_grid] sd_eta;  // sd of GP mean variates
}

transformed parameters {
  // Group-level parameters
  real intercept[N_group];          // intercept
  vector<lower=0>[D] rho[N_group];  // length scale
  real<lower=0> alpha[N_group];     // marginal sd
  vector[N_grid] eta[N_group];      // std normal variates
  vector[N_grid] f[N_group];        // GPs

  for (i in 1:N_group) {
    intercept[i] = m_intercept + sd_intercept * z_intercept[i];
    rho[i] = exp(log(md_rho) + sd_rho .* z_rho[i]);
    alpha[i] = exp(log(md_alpha) + sd_alpha * z_alpha[i]);
    eta[i] = m_eta + sd_eta .* z_eta[i];
    f[i] = gp_exp_quad_ARD(x, alpha[i], rho[i], eta[i]);
    
  }
}

model {
  vector[N] lambda;
  for (i in 1:N) {
    lambda[i] = intercept[idx_group[i]] + f[idx_group[i], idx_grid[i]];
  }
  
  // group-level parameters
  z_intercept ~ std_normal();
  z_alpha ~ std_normal();
  for (i in 1:N_group) {
    z_rho[i] ~ std_normal();
    z_eta[i] ~ std_normal();
  }

  // population-level parameters
  sd_intercept ~ normal(0, 0.1);
  md_rho ~ inv_gamma(20, 600);
  sd_rho ~ normal(0, 0.25);
  md_alpha ~ std_normal();
  sd_alpha ~ normal(0, 0.25);
  m_eta ~ std_normal();
  sd_eta ~ std_normal();

  y ~ poisson_log(lambda);
}

generated quantities {
  vector[N] lambda;
  int yhat[N];
  
  for (i in 1:N) {
    lambda[i] = intercept[idx_group[i]] + f[idx_group[i], idx_grid[i]];
  }
  yhat = poisson_log_rng(lambda);
}

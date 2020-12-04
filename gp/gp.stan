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
  int<lower=1> N;     // total number of data points
  int<lower=1> G;     // number of data points per trial (grid size)
  int<lower=1> P;     // number of participants
  int<lower=1> D;     // number of dimensions

  int<lower=1, upper=G> g[N];  // grid index for each trial
  int<lower=1, upper=P> p[N];  // participant index for each trial

  vector[D] x[G];     // input data (same across trials)
  int<lower=0> y[N];  // output data (counts)
}

parameters {
  // Population-level parameters 
  real m_intercept;               // population intercept
  real<lower=0> sd_intercept;     // sd of individual intercepts
  vector[G] eta;                  // group-level GP mean variates

  // Participant-level parameters
  real z_intercept[P];            // standardized intercepts
  vector[G] eta_p[P];             // GP mean variates

  // Hyperparameters
  vector<lower=0>[D] rho;         // group-level length-scale
  real<lower=0> alpha;            // group-level marginal standard deviation
  vector<lower=0>[D] rho_p;       // participant-level length-scale
  real<lower=0> alpha_p;          // participant-level marginal standard deviation
}

transformed parameters {
  vector[G] f;                    // group-level GP
  real intercept[P];              // actual participant-level intercepts
  vector[G] f_p[P];               // participant-level GPs

  f = gp_exp_quad_ARD(x, alpha, rho, eta);
  for (i in 1:P) {
    intercept[i] = m_intercept + sd_intercept * z_intercept[i];
    f_p[i] = gp_exp_quad_ARD(x, alpha_p, rho_p, eta_p[i]);
  }
}

model {
  vector[N] lambda;
  for (i in 1:N) {
    lambda[i] = intercept[p[i]] + f[g[i]] + f_p[p[i], g[i]];
  }

  // Population-level parameters
  // *improper uniform prior for m_intercept*
  sd_intercept ~ std_normal();
  eta ~ std_normal();

  // Participant-level parameters
  z_intercept ~ std_normal();
  for (i in 1:P) { eta_p[i] ~ std_normal(); }

  // Hyperparameters
  rho ~ inv_gamma(10, 500);
  alpha ~ std_normal();
  rho_p ~ inv_gamma(20, 600);
  alpha_p ~ std_normal();
  
  y ~ poisson_log(lambda);
}

generated quantities {
  vector[N] lambda;
  int yhat[N];
  
  for (i in 1:N) {
    lambda[i] = intercept[p[i]] + f[g[i]] + f_p[p[i], g[i]];
  }
  yhat = poisson_log_rng(lambda);
}

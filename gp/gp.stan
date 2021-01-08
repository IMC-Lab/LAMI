functions {
  /*
   * compute the covariance matrix for a latent Gaussian process
   * args:
   *   grid: the GP predictors
   *   alpha: the gp marginal SD parameter
   *   rho: the length-scales for each dimension of grid
   * returns:
   *   a covariance matrix between locations on the grid
   */
  matrix cov_exp_quad_ARD(vector[] grid, real alpha, vector rho) {
    int N = size(grid);
    real delta = 1e-8;
    matrix[N, N] K;
    real alpha_sq = square(alpha);

    for (i in 1:(N-1)) {
      K[i, i] = alpha_sq + delta;
      for (j in (i + 1):N) {
        K[i, j] = alpha_sq * exp(-0.5 * dot_self((grid[i] - grid[j]) ./ rho));
        K[j, i] = K[i, j];
      }
    }
    K[N, N] = alpha_sq + delta;
    return cholesky_decompose(K);
  }
}

data {
  int<lower=1> N;     // total number of data points
  int<lower=1> G;     // number of data points per trial (grid size)
  int<lower=1> P;     // number of participants
  int<lower=1> D;     // number of GP input dimensions
  int<lower=1> C;     // number of conditions

  int<lower=1, upper=G> g[N];  // grid index for each trial
  int<lower=1, upper=P> p[N];  // participant index for each trial
  int<lower=1, upper=C> c[N];  // condition index for each trial

  vector[D] grid[G];  // input data (same across trials)
  int<lower=0> y[N];  // output data (counts)
}

parameters {
  // Population-level parameters
  real a;                        // global intercept mean
  vector[G] eta[C];               // population-level GP mean variates (by condition)

  // Participant-level parameters
  vector[G] eta_tilde[C, P];      // participant-level GP mean variates

  // Hyperparameters
  vector<lower=0>[D] rho;         // population-level length-scale
  real<lower=0> alpha;            // population-level marginal standard deviation
  vector<lower=0>[D] rho_tilde;   // participant-level length-scale
  real<lower=0> alpha_tilde;      // participant-level marginal standard deviation
}

transformed parameters {
  vector[G] f[C];                 // group-level GPs (by condition)
  vector[G] f_tilde[C, P];        // participant-level GPs (by participantXcondition)
  
  {
    // pre-compute GP covariance matrices
    matrix[G, G] Lf = cov_exp_quad_ARD(grid, alpha, rho);
    matrix[G, G] Lf_tilde = cov_exp_quad_ARD(grid, alpha_tilde, rho_tilde);

    for (i in 1:C) {
      f[i] = Lf * eta[i];
      for (j in 1:P) {
        f_tilde[i, j] = Lf_tilde * eta_tilde[i, j];
      }
    }
  }
}

model {
  // intercept terms
  a ~ normal(0, 5);
  
  // GP terms
  for (i in 1:C) {
    eta[i] ~ std_normal();
    for (j in 1:P) {
      eta_tilde[i, j] ~ std_normal();
    }
  }

  // Hyperparameters
  rho ~ inv_gamma(10, 1000);
  alpha ~ std_normal();
  rho_tilde ~ inv_gamma(10, 1000);
  alpha_tilde ~ std_normal();

  {
    vector[N] lambda;
    for (i in 1:N) {
      lambda[i] = a + f[c[i], g[i]] + f_tilde[c[i], p[i], g[i]];
    }

    y ~ poisson_log(lambda);
  }
}

generated quantities {
  vector[N] lambda;
  int yhat[N];
  vector[N] prior_lambda;
  vector[G] prior_f[C];
  vector[G] prior_f_tilde[C, P];
  
  // sample from priors
  real prior_a = normal_rng(0, 5);
  vector<lower=0>[D] prior_rho = [inv_gamma_rng(10, 1000),
				  inv_gamma_rng(10, 1000)]';
  real prior_alpha = normal_rng(0, 1);
  vector<lower=0>[D] prior_rho_tilde = [inv_gamma_rng(10, 1000),
					inv_gamma_rng(10, 1000)]';
  real prior_alpha_tilde = normal_rng(0, 1);
  {
    // pre-compute GP covariance matrices
    matrix[G, G] prior_Lf = cov_exp_quad_ARD(grid, prior_alpha, rho);
    matrix[G, G] prior_Lf_tilde = cov_exp_quad_ARD(grid, prior_alpha_tilde,
						   prior_rho_tilde);
    vector[G] prior_eta[C];
    vector[G] prior_eta_tilde[C, P];
    
    for (i in 1:C) {
      for (j in 1:G) prior_eta[i, j] = normal_rng(0, 1);
      prior_f[i] = prior_Lf * prior_eta[i];
      for (j in 1:P) {
	for (k in 1:G) prior_eta_tilde[i, j, k] = normal_rng(0, 1);
        prior_f_tilde[i, j] = prior_Lf_tilde * prior_eta_tilde[i, j];
      }
    }
  }

  // trial-level predictions
  for (i in 1:N) {
    prior_lambda[i] = prior_a + prior_f[c[i], g[i]] + prior_f_tilde[c[i], p[i], g[i]];
    lambda[i] = a + f[c[i], g[i]] + f_tilde[c[i], p[i], g[i]];
  }
  yhat = poisson_log_rng(lambda);
}

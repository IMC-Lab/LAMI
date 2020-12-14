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
  int<lower=1> K;     // number of population-level effects
  int<lower=1> M;     // number of participant-level effects
  int<lower=1> C;     // number of conditions

  int<lower=1, upper=G> g[N];  // grid index for each trial
  int<lower=1, upper=P> p[N];  // participant index for each trial
  int<lower=1, upper=C> c[N];  // condition index for each trial

  vector[D] grid[G];  // input data (same across trials)
  matrix[N, K] X;     // population-level design matrix
  matrix[N, M] Z;     // participant-level design matrix
  int<lower=0> y[N];  // output data (counts)

  // group-level predictions
  int<lower=0> Npred;
  int<lower=1, upper=G> gpred[Npred];
  int<lower=1, upper=C> cpred[Npred];
  matrix[Npred, K] Xpred;
}

transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;    // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  matrix[Npred, Kc] Xpred2 = Xpred[,2:K];

  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}

parameters {
  // Population-level parameters
  real ac;                        // temporary intercept for centered predictors
  vector[Kc] beta;                // population-level effects
  vector<lower=0>[M] sigma;       // sds of participant-level effects
  cholesky_factor_corr[M] L;      // correlation matrix of participant-level effects
  vector[G] eta[C];               // population-level GP mean variates (by condition)

  // Participant-level parameters
  matrix[M, P] z;                 // standardized group-level effects
  vector[G] eta_tilde[C, P];      // participant-level GP mean variates

  // Hyperparameters
  vector<lower=0>[D] rho;         // population-level length-scale
  real<lower=0> alpha;            // population-level marginal standard deviation
  vector<lower=0>[D] rho_tilde;   // participant-level length-scale
  real<lower=0> alpha_tilde;      // participant-level marginal standard deviation
}

transformed parameters {
  vector[G] f[C];                 // group-level GPs (by condition)
  matrix[P, M] gamma;             // actual participant-level effects
  vector[G] f_tilde[C, P];        // participant-level GPs (by participantXcondition)

  gamma = transpose(diag_pre_multiply(sigma, L) * z);

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
  vector[N] lambda;
  for (i in 1:N) {
    lambda[i] = ac + Xc[i]*beta + f[c[i], g[i]] +   // population-level effects
		dot_product(Z[i], gamma[p[i]]) +    // participant-level effects
		f_tilde[c[i], p[i], g[i]];      
  }

  // Population-level parameters
  // *improper uniform prior for ac*
  beta ~ student_t(3, 0, 1);
  sigma ~ student_t(3, 0, 2.5);
  L ~ lkj_corr_cholesky(1);
  for (i in 1:C) {
    eta[i] ~ std_normal();
    for (j in 1:P) {
      eta_tilde[i, j] ~ std_normal();
    }
  }

  // Participant-level parameters
  to_vector(z) ~ std_normal();

  // Hyperparameters
  rho ~ inv_gamma(10, 500);
  alpha ~ std_normal();
  rho_tilde ~ inv_gamma(20, 600);
  alpha_tilde ~ std_normal();
  
  y ~ poisson_log(lambda);
}

generated quantities {
  // obtain the true intercept (for non-centered design matrices)
  real a = ac - dot_product(means_X, beta);
  vector[N] lambda;
  vector[Npred] lambda_group;
  int yhat[N];
  int yhat_group[Npred];
  
  // trial-level predictions
  for (i in 1:N) {
    lambda[i] = ac + Xc[i]*beta + f[c[i], g[i]] +
		dot_product(Z[i], gamma[p[i]]) + f_tilde[c[i], p[i], g[i]];      
  }
  yhat = poisson_log_rng(lambda);

  // group-level predictions
  for (i in 1:Npred) {
    lambda_group[i] = a + Xpred2[i]*beta + f[cpred[i], gpred[i]];
  }
  yhat_group = poisson_log_rng(lambda_group);
}

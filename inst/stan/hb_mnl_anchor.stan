// Hierarchical Bayes MNL for anchored MaxDiff.
// Alternatives 1..J are messages, J+1 is the threshold (anchor), fixed at 0.
// Block A: K-alternative choice tasks (best: sign = +1, worst: sign = -1).
// Block B: binary message-vs-threshold comparisons (y = 1 if message wins).
data {
  int<lower=1> N;                       // respondents
  int<lower=1> J;                       // messages (anchor = J + 1)
  int<lower=0> TA;                      // K-alt tasks
  int<lower=2> K;
  array[TA, K] int<lower=1> idxA;       // flat index into [N x (J+1)] matrix, col-major
  array[TA] int<lower=1, upper=K> chA;  // chosen position
  vector[TA] sA;                        // +1 best, -1 worst
  int<lower=0> TB;                      // binary tasks
  array[TB] int<lower=1> idxB;          // flat index of the message
  array[TB] int<lower=0, upper=1> yB;
}
parameters {
  vector[J] mu;
  vector<lower=0>[J] tau;
  cholesky_factor_corr[J] L_Omega;
  matrix[N, J] z;
}
transformed parameters {
  matrix[N, J] beta = rep_matrix(mu', N) + z * diag_pre_multiply(tau, L_Omega)';
}
model {
  vector[N * (J + 1)] flat = append_row(to_vector(beta), rep_vector(0, N));
  mu ~ normal(0, 2);
  tau ~ normal(0, 1.5);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(z) ~ std_normal();
  for (t in 1:TA) {
    vector[K] u = sA[t] * flat[idxA[t]];
    target += u[chA[t]] - log_sum_exp(u);
  }
  if (TB > 0) yB ~ bernoulli_logit(flat[idxB]);
}

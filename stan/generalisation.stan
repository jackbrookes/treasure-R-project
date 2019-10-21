data {
  
  // assumes data are ordered in an unconventional way.
  // by trial, then ppid.
  // i.e. all trials 1s across participants, all trial 2s, etc
  
  vector<lower = 0, upper = 1>[2] initial_est;
  int<lower = 0> N; // total num trials/observations
  int<lower = 0> trial_idx[N]; // trial numbers
  int<lower = 0> ppid_idx[N]; // ppid
  int<lower = 1, upper = 2> y[N]; // selected target
  int<lower = 0, upper = 1> k[N]; // key? 0 or 1
  int<lower = 0, upper = 1> r[N]; // coin? 0 or 1
  vector[2] value_probe[N]; // reported values
}


parameters {
  real<lower = 0, upper = 1> g[18]; // generalisation, per participant
  real<lower = 0, upper = 1> eta; // learning rate
  real<lower = 0, upper = 100> sigma; // sd of value estimates
}

transformed parameters {
  // model estimates of finding key
  vector[2] k_est[N];
  // model estimates of finding reward GIVEN we found a key
  vector[2] r_est[N];

  for (i in 1:N) {
    int ppid = ppid_idx[i];
    if (trial_idx[i] == 1) {
      // first trial: assume 50% chance in each stage
      k_est[i] = initial_est;
      r_est[i] = initial_est;
    } else {
      // otherwise we use outcomes from previous trial
      int chosen = y[i - 1];
      int other = chosen == 1 ? 2 : 1;
      
      // we try to find a key, then update our k_est
      k_est[i, chosen] = k_est[i - 1, chosen] + eta * (k[i - 1] - k_est[i - 1, chosen]);
      
      // but, some of that generalises to the estimate of the other key's probability
      k_est[i, other] = k_est[i - 1, other] + g[ppid] * eta * (k[i - 1] - k_est[i - 1, other]);
      
      // further, we update the chest we chose IF we found a key last time.
      if (k[i - 1]) { 
        r_est[i, chosen] = r_est[i - 1, chosen] + eta * (r[i - 1] - r_est[i - 1, chosen]);

        // no generalisation here. just carry over previous
        r_est[i, other] = r_est[i - 1, other];
      } else { // if not, just carry over...
        r_est[i, chosen] = r_est[i - 1, chosen];
        r_est[i, other] = r_est[i - 1, other];
      }
    }
  }
}


model {
  for (i in 1:N) {
    if (trial_idx[i] >= 30) { // can only predict on trials with a probe
      value_probe[i] ~ normal(k_est[i] .* r_est[i], sigma);
    }
  }
}


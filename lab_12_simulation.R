generate_data = function(n, p) {
  covariates = matrix(rnorm(n*p), ncol = p, nrow = n)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff) {
  #browser()
  orig_lm = lm(responses ~ covariates)
  p_vals = summary(orig_lm)$coef[-1,4]
  #print("Storing p values")
  #print(p_vals)
  p_vals_sig = p_vals <= cutoff
  vec = c()
  #print("Printing the model")
  #print(p_vals)
  #print(sum(p_vals_sig))
  if (sum(p_vals_sig) == 0) return(vec)
  new_lm = lm(responses ~ covariates[, p_vals_sig])
  new_p_vals = summary(new_lm)$coef[-1,4]
  return(new_p_vals)
}

run_simulation = function(n_trials, n, p, cutoff) {
  all_p_vals = c()
  
    for (num_ftrs in p) {
      for (num_obs in n) {
        all_p_vals = c()
        for (trial in 1:n_trials) {
        #browser()
        data = generate_data(num_obs, num_ftrs)
        p_vals = model_select(data$covariates, data$responses, cutoff = cutoff)
        all_p_vals = c(all_p_vals, p_vals)
        
        }
        obs_str = toString(num_obs)
        ftrs_str = toString(num_ftrs)
        path_idx = paste(obs_str, paste("_", ftrs_str, sep = ""), sep = "")
        save(all_p_vals, paste("p_vals_", path_idx, sep = ""))
        }
      }
    }

#run_simulation(1, c(100), c(50), .05)
#n = c(100, 1000,   10000)
#p = c(10, 20, 50)
#cutoff = 0.05
#run_simulation(1, n, p, cutoff)

make_plot = function(datapath) {
  load(datapath)
  if(length(all_p_vals) > 0) {
    hist(all_p_vals, xlab = "p-values", ylab = "Frequency",
         main = "Distribution of p-values")
  }
}

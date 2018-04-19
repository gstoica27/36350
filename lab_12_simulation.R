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
  for (trial in 1:n_trials) {
    for (num_ftrs in p) {
      for (num_obs in n) {
        #browser()
        data = generate_data(num_obs, num_ftrs)
        p_vals = model_select(data$covariates, data$responses, cutoff = cutoff)
        all_p_vals = c(all_p_vals, p_vals)
      }
    }
  }
  #print(all_p_vals)
  if(length(all_p_vals) > 0) {
    plot(all_p_vals, xlab = "p-values", ylab = "Frequency",
         main = "Distribution of p-values")
  }
  
}

#run_simulation(1, c(100), c(50), .05)


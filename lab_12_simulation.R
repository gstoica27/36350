generate_data = function(n, p) {
  covariates = matrix(rnorm(n*p), ncol = p, nrow = n)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff) {
  orig_lm = lm(responses ~ covariates)
  p_vals = summary(orig_lm)$coef[-1,4]
  p_vals_sig = p_vals <= cutoff
  vec = c()
  if (sum(p_vals_sig) == 0) return(vec)
  new_lm = lm(responses ~ covariates[, p_vals_sig])
  new_p_vals = summary(new_lm)$coef[-1,4]
  return(new_p_vals)
}
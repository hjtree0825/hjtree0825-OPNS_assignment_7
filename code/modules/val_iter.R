val_update <- function(ccp, theta, beta){
  ccp %>%
    mutate(prob_1p = prob_1,
           prob_0p = prob_0,
           f = theta$theta_1*ccp$s - theta$theta_2*ccp$mile + beta*(log(ccp$prob_1) + log((1 - ccp$prob_1))),
           prob_1 = 1/(1+exp(f)),
           prob_0 = 1 - prob_1,
           delta_p1 = prob_1 - prob_1p,
           delta_p0 = prob_0 - prob_0p
    ) #%>%
  #select(s, mile, prob_1, prob_0)
}

val_iter <- function(ccp, val_tol, theta, beta){
  delta <- 100
  while (delta > val_tol){
    ccp <- val_update(ccp, theta, beta)
    delta <- max(abs(ccp$prob_1 - ccp$prob_1p))
    ccp <- ccp %>%
      select(s, mile, prob_1, prob_0)
  }
  return(ccp) # Just to make sure
}
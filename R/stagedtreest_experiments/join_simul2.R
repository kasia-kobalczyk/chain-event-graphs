# Likelihood of joining bins

n <- 100
theta <- c(0.999,0.001,0.001,0.999,0.8,0.001,0.999,0.001,0.5,0.001,0.999)
n_bins <- length(theta)
#n_obs <- rep(n, n_bins)
n_obs <- c(1,1,2,1,3,1,5,1,1,2,1)


get_ll <- function(n_obs, theta) {
  ll <- sum(n_obs*theta*log(theta) + n_obs*(1-theta)*log(1-theta))
}

base_LL <- get_ll(n_obs, theta)

get_new_ll<- function(threshold) {
  theta_1 <- theta[1:threshold] %*% n_obs[1:threshold] / sum(n_obs[1:threshold])
  theta_2 <- theta[(threshold+1):length(theta)] %*% n_obs[(threshold+1):length(theta)] / sum(n_obs[(threshold+1):length(theta)])

  new_theta <- c(theta_1, theta_2)
  new_n_obs <- c(sum(n_obs[1:threshold]), sum(n_obs[(threshold+1):length(theta)]))
  
  new_LL <- get_ll(new_n_obs, new_theta)
  new_LL
}

new_LL <- get_new_ll(9)
dll <- base_LL - new_LL 
dll

#' R script for simulating merges between bins of different size and probabilities,
#' Tests for loglikelihood calculations.

n <- 100
theta <- c(0.999,0.001,0.001,0.999,0.8,0.001,0.999,0.001,0.5,0.001,0.999)
n_bins <- length(theta)
#n_obs <- rep(n, n_bins)
n_obs <- c(1,1,2,1,3,1,5,1,1,2,1)


get_ll <- function(n_obs, theta) {
  ll <- sum(n_obs*theta*log(theta) + n_obs*(1 - theta)*log(1 - theta))
}

base_LL <- get_ll(n_obs, theta)

get_new_ll <- function(threshold) {
  theta_1 <- theta[1:threshold] %*% n_obs[1:threshold] / sum(n_obs[1:threshold])
  theta_2 <- theta[(threshold + 1):length(theta)] %*% n_obs[(threshold + 1):length(theta)] / sum(n_obs[(threshold + 1):length(theta)])
  
  new_theta <- c(theta_1, theta_2)
  new_n_obs <- c(sum(n_obs[1:threshold]), sum(n_obs[(threshold + 1):length(theta)]))
  
  new_LL <- get_ll(new_n_obs, new_theta)
  new_LL
}

new_LL <- get_new_ll(9)
dll <- base_LL - new_LL 
dll

get_join_ll <- function(p1, p2, n1, n2) {
  theta_1 = c(p1, 1 - p1)
  theta_2 = c(p2, 1 - p2)
  theta_new = (theta_1 * n1 + theta_2 * n2) / (n1 + n2)
  ll_old = sum(theta_1 * n1 * log(theta_1) + theta_2 * n2 * log(theta_2))
  ll_new = sum(theta_new * (n1 + n2) * log(theta_new))
  ll_old - ll_new
}

get_ll_diff <- function(k, p1, n1, n2) {
  p2 <- p1 * k
  theta_1 = c(p1, 1 - p1)
  theta_2 = c(p2, 1 - p2)
  theta_new = (theta_1 * n1 + theta_2 * n2) / (n1 + n2)
  ll_old = sum(theta_1 * n1 * log(theta_1) + theta_2 * n2 * log(theta_2))
  ll_new = sum(theta_new * (n1 + n2) * log(theta_new))
  ll_old - ll_new
}

k <- seq(1, 2, 0.01)
ll_big <- sapply(k, get_ll_diff, p1 = 0.5, n1 = 100, n2 = 200)
ll_small <- sapply(k, get_ll_diff, p1 = 0.5, n1 = 100, n2 = 100)
plot(k, ll_big, "l", col = "red")
lines(k, ll_small, col = "blue")

get_join_ll(0.46, 0.70, 300, 10)
get_join_ll(0.70, 0.35, 10, 200)

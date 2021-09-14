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

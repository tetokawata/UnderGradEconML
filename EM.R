set.seed(123)

# データ生成（2つの正規分布から混合）
n <- 300
x <- c(rnorm(n / 2, mean = 0, sd = 1), rnorm(n / 2, mean = 5, sd = 1))

# 初期値の設定
mu1 <- quantile(x, 0.25)
mu2 <- quantile(x, 0.75)
sigma1 <- 1
sigma2 <- 1
pi1 <- 0.5
pi2 <- 0.5

# EMアルゴリズムの本体
em_gmm <- function(x, max_iter = 100, tol = 1e-6) {
  n <- length(x)

  # 初期値
  mu1 <- quantile(x, 0.25)
  mu2 <- quantile(x, 0.75)
  sigma1 <- 1
  sigma2 <- 1
  pi1 <- 0.5
  pi2 <- 0.5

  log_likelihood <- function(x, mu1, mu2, sigma1, sigma2, pi1, pi2) {
    sum(log(pi1 * dnorm(x, mu1, sigma1) + pi2 * dnorm(x, mu2, sigma2)))
  }

  ll_old <- log_likelihood(x, mu1, mu2, sigma1, sigma2, pi1, pi2)

  for (iter in 1:max_iter) {
    # Eステップ：責任度（各点がどちらの成分に属するかの確率）
    gamma1 <- pi1 * dnorm(x, mu1, sigma1)
    gamma2 <- pi2 * dnorm(x, mu2, sigma2)
    gamma_sum <- gamma1 + gamma2
    z1 <- gamma1 / gamma_sum
    z2 <- gamma2 / gamma_sum

    # Mステップ：パラメータ更新
    N1 <- sum(z1)
    N2 <- sum(z2)

    mu1 <- sum(z1 * x) / N1
    mu2 <- sum(z2 * x) / N2

    sigma1 <- sqrt(sum(z1 * (x - mu1)^2) / N1)
    sigma2 <- sqrt(sum(z2 * (x - mu2)^2) / N2)

    pi1 <- N1 / n
    pi2 <- N2 / n

    # 対数尤度の計算
    ll_new <- log_likelihood(x, mu1, mu2, sigma1, sigma2, pi1, pi2)

    # 収束判定
    if (abs(ll_new - ll_old) < tol) {
      break
    }
    ll_old <- ll_new
  }

  list(mu = c(mu1, mu2), sigma = c(sigma1, sigma2), pi = c(pi1, pi2), iter = iter)
}

# 実行
result <- em_gmm(x)
print(result)

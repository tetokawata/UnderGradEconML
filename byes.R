set.seed(123)

# データ生成
n <- 100
true_mu <- 5
true_sigma <- 2
x <- rnorm(n, mean = true_mu, sd = true_sigma)

# 事前分布のパラメータ
mu0 <- 0
tau2 <- 10^2
a <- 2
b <- 2

# 初期値
mu <- mean(x)
sigma2 <- var(x)

# ギブスサンプリングの設定
n_iter <- 5000
mu_samples <- numeric(n_iter)
sigma2_samples <- numeric(n_iter)

for (i in 1:n_iter) {
  # 条件付き分布から mu をサンプリング
  mu_n <- (n * mean(x) / sigma2 + mu0 / tau2) / (n / sigma2 + 1 / tau2)
  tau_n2 <- 1 / (n / sigma2 + 1 / tau2)
  mu <- rnorm(1, mean = mu_n, sd = sqrt(tau_n2))

  # 条件付き分布から sigma^2 をサンプリング（Inverse-Gamma）
  a_n <- a + n / 2
  b_n <- b + sum((x - mu)^2) / 2
  sigma2 <- 1 / rgamma(1, shape = a_n, rate = b_n)

  # 保存
  mu_samples[i] <- mu
  sigma2_samples[i] <- sigma2
}

# 結果の表示
cat("推定された mu の平均:", mean(mu_samples), "\n")
cat("推定された sigma の平均:", sqrt(mean(sigma2_samples)), "\n")

par(mfrow = c(1, 2))
hist(mu_samples, breaks = 50, main = "Posterior of μ", xlab = "μ")
hist(sqrt(sigma2_samples), breaks = 50, main = "Posterior of σ", xlab = "σ")

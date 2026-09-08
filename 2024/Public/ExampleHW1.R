# いくつか間違いを含んでますので、各自修正してください

set.seed(1) # シード値を決める

library(tidyverse) # tidyverseをロード

Data = read_csv("Example.csv") # データを取り込む

N = nrow(Data) # 事例数を数える

T = round(N*0.8) # 訓練データの数を指定

Group = sample(
  1:N,
  T
) # 乱数を発生
Train = Data[Group,] # 訓練データ
Test = Data[-Group,] # テストデータ

OLS = lm(
  Size ~ Tenure + Price, # SizeをTenureとPriceで予測
  Data # "Data"を使う
  ) # モデルを推定して、"OLS"として保存

Pred = predict(OLS, Data) # "OLS"と"Data"で使って予測し、"Pred"として保存

mean((Data$Size - Pred)^2) # 平均二乗誤差を計算する


# Y = Tenure, X = 各自選ぶとして、OLSで予測モデルを推定し、評価

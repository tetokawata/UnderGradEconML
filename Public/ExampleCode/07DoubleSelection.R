library(tidyverse)
library(hdm)

Data = read_csv("Example.csv")

Y = Data$Price
D = Data$After

X = model.matrix(~ (Size + Tenure + District)**2 + I(Size^2) + I(Tenure^2), Data)
X = X[,-1]

Model = rlassoEffect(y = Y, d = D, x = X) # 推定

summary(Model) # 推定結果
confint(Model) # 信頼区間
Model$selection.index # 変数選択の結果
# Ctr + A -> Ctr + Enter

## 宿題2: Y = Size, D = After (D)を、Tenure,District,Priceをバランスさせて推定する
## 複雑な定式化を用いて、OLSとDouble Selection両方を実行し、信頼区間も計算
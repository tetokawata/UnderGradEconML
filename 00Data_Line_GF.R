library(tidyverse)

data <- nanoparquet::read_parquet("Data/Tokyo_20053_20241.parquet") |>
  mutate(
    TradeYear = 取引時期 |>
      str_sub(1, 4) |>
      as.numeric()
  ) |>
  filter(
    str_detect(市区町村名, "区"),
    TradeYear == 2023
  ) |>
  rename(
    Station = 最寄駅_名称,
    Area = 市区町村名,
    Size = 面積
  ) |>
  left_join(
    read_csv("Data/Station.csv") |>
      rename(
        Station = 駅名
      )
  ) |>
  mutate(
    Price = 取引価格 / 1000000,
    LineYamate = if_else(
      Station %in% c(
        "浜松町",
        "大手町(東京)",
        "東京",
        "神田(東京)",
        "品川",
        "上野",
        "池袋",
        "大塚(東京)",
        "秋葉原",
        "目白",
        "駒込",
        "西日暮里",
        "大崎",
        "新宿",
        "代々木",
        "原宿",
        "渋谷",
        "日暮里",
        "恵比寿",
        "新大久保",
        "田町(東京)",
        "鶯谷",
        "高輪ゲートウェイ",
        "高田馬場",
        "御徒町"
      ),
      1,
      0
    ),
    LineCentral = if_else(
      Station %in% c(
        "東京",
        "神田(東京)",
        "御茶ノ水",
        "市ケ谷",
        "四ツ谷",
        "信濃町",
        "千駄ケ谷",
        "新宿",
        "大久保(東京)",
        "東中野",
        "中野(東京)",
        "高円寺",
        "阿佐ケ谷",
        "荻窪",
        "西荻窪",
        "吉祥寺",
        "武蔵関"
      ),
      1,
      0
    ),
    LineMaru = if_else(
      Station %in% c(
        "池袋",
        "新大塚",
        "茗荷谷",
        "本郷三丁目",
        "淡路町",
        "大手町(東京)",
        "東京",
        "銀座",
        "赤坂見附",
        "四谷三丁目",
        "新宿御苑前",
        "新宿三丁目",
        "新宿",
        "西新宿",
        "中野坂上",
        "新中野",
        "中野新橋",
        "中野富士見町",
        "中野坂上",
        "東高円寺",
        "新高円寺",
        "南阿佐ケ谷",
        "方南町"
      ),
      1,
      0
    ),
    LineToyoko = if_else(
      Station %in% c(
        "渋谷",
        "代官山",
        "中目黒",
        "祐天寺",
        "学芸大学",
        "都立大学",
        "自由が丘(東京)",
        "田園調布",
        "多摩川"
      ),
      1,
      0
    ),
    LineInogashira = if_else(
      Station %in% c(
        "渋谷",
        "神泉",
        "駒場東大前",
        "池ノ上",
        "下北沢",
        "新代田",
        "東松原",
        "明大前",
        "永福町",
        "西永福",
        "浜田山",
        "高井戸",
        "富士見ケ丘",
        "久我山",
        "三鷹台",
        "吉祥寺"
      ),
      1,
      0
    ),
    LineSubCBD = if_else(
      Station %in% c(
        "渋谷",
        "明治神宮前",
        "北参道",
        "新宿三丁目",
        "東新宿",
        "西早稲田",
        "雑司が谷(東京メトロ)",
        "池袋",
        "要町",
        "千川",
        "小竹向原",
        "氷川台",
        "平和台(東京)",
        "地下鉄赤塚",
        "地下鉄成増"
      ),
      1,
      0
    ),
    LineMita = if_else(
      Station %in% c(
        "新高島平",
        "西高島平",
        "高島平",
        "西台",
        "蓮根",
        "志村三丁目",
        "志村坂上",
        "本蓮沼",
        "板橋本町",
        "板橋区役所前",
        "新板橋",
        "西巣鴨",
        "巣鴨",
        "千石",
        "白山(東京)",
        "春日(東京)",
        "水道橋",
        "神保町",
        "大手町(東京)",
        "日比谷",
        "御成門",
        "芝公園",
        "三田(東京)",
        "白金台",
        "目黒"
      ),
      1,
      0
    ),
    LineTosai = if_else(
      Station %in% c(
        "葛西",
        "西葛西",
        "南砂町",
        "東陽町",
        "木場",
        "門前仲町",
        "茅場町",
        "日本橋(東京)",
        "大手町(東京)",
        "九段下",
        "飯田橋",
        "神楽坂",
        "早稲田(メトロ)",
        "高田馬場",
        "落合(東京)",
        "中野(東京)"
      ),
      1,
      0
    ),
    LineSaikyo = if_else(
      Station %in% c(
        "浮間舟渡",
        "北赤羽",
        "赤羽",
        "十条(東京)",
        "板橋",
        "池袋",
        "新宿",
        "渋谷",
        "恵比寿",
        "大崎"
      ),
      1,
      0
    ),
    LineZhoban = if_else(
      Station %in% c(
        "北千住",
        "綾瀬",
        "亀有",
        "金町"
      ),
      1,
      9
    ),
    LineHanzou = if_else(
      Station %in% c(
        "押上",
        "錦糸町",
        "住吉(東京)",
        "清澄白河",
        "水天宮前",
        "三越前",
        "大手町(東京)",
        "神保町",
        "九段下",
        "半蔵門",
        "永田町",
        "青山一丁目",
        "表参道",
        "渋谷"
      ),
      1,
      0
    ),
    LineSetagaya = if_else(
      Station %in% c(
        "三軒茶屋",
        "西太子堂",
        "若林(東京)",
        "松陰神社前",
        "世田谷",
        "上町",
        "宮の坂",
        "松原(東京)",
        "下高井戸"
      ),
      1,
      0
    ),
    LineDenen = if_else(
      Station %in% c(
        "渋谷",
        "三軒茶屋",
        "池尻大橋",
        "駒沢大学",
        "桜新町",
        "用賀",
        "二子玉川"
      ),
      1,
      0
    ),
    LineEdo = if_else(
      Station %in% c(
        "都庁前",
        "西新宿五丁目",
        "新宿西口",
        "新宿",
        "代々木",
        "国立競技場",
        "青山一丁目",
        "六本木",
        "麻布十番",
        "赤羽橋",
        "大門(東京)",
        "汐留",
        "築地市場",
        "勝どき",
        "月島",
        "清澄白河",
        "森下(東京)",
        "両国",
        "蔵前",
        "新御徒町",
        "春日(東京)",
        "牛込神楽坂",
        "若松河田",
        "東中野",
        "中野(東京)",
        "中野坂上",
        "中井",
        "落合南長崎",
        "新江古田",
        "練馬",
        "豊島園",
        "練馬春日町",
        "光が丘"
      ),
      1,
      0
    ),
    LineYuraku = if_else(
      Station %in% c(
        "辰巳",
        "豊洲",
        "月島",
        "新富町(東京)",
        "銀座一丁目",
        "永田町",
        "麹町",
        "江戸川橋",
        "護国寺",
        "東池袋",
        "池袋"
      ),
      1,
      0
    ),
    Distance = 最寄駅_距離 |> as.numeric(),
    BuildYear = 建築年 |> str_sub(1, 4) |> as.numeric(),
    Tenure = TradeYear - BuildYear
  ) |>
  select(
    Station,
    Area,
    Size,
    Price,
    starts_with("Line"),
    Distance,
    BuildYear,
    TradeYear,
    Tenure
  ) |>
  na.omit()

data |> summary()

train <- data

table(train$LineInogashira, train$Area)

model_D <- grf::regression_forest(
  Y = train$LineInogashira,
  X = train |> model.matrix(~ Area + Size + Tenure + Distance, data = _)
)

train$Weight <- if_else(
  train$LineInogashira == 1,
  1 / model_D$predictions[, 1],
  1 / (1 - model_D$predictions[, 1])
)

(train$Weight * train$Price * train$LineInogashira) |> mean()

(train$Weight * train$Price * (1 - train$LineInogashira)) |> mean()

lm(Price ~ LineInogashira + (Area + Size + Tenure + Distance)^2 + I(Size^2) + I(Tenure^2) + I(Distance^2), train)

X <- train |>
  model.matrix(
    ~ 0 + Area + Size + Tenure + Distance,
    data = _
  ) |>
  scale()

model <- grf::causal_forest(
  Y = train$Price |> log(),
  W = train$LineYamate,
  X = X
)

grf::test_calibration(model)

grf::average_treatment_effect(model, target.sample = "control")

grf::average_treatment_effect(model, target.sample = "control", subset = model$predictions >= median(model$predictions))

grf::average_treatment_effect(model, target.sample = "control", subset = model$predictions < median(model$predictions))


grf::best_linear_projection(
  model, X[, -1],
  target.sample = "overlap"
)

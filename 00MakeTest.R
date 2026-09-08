

readr::read_csv("Public/example.csv")[sample(1:10000,5),] |> 
  dplyr::select(-Price) |> 
  readr::write_csv("Public/test.csv")

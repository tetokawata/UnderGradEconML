bib2df::bib2df("ref.bib") |> 
  dplyr::distinct(
    BIBTEXKEY,
    .keep_all = TRUE
  ) |> 
  dplyr::mutate(
    YEAR = YEAR |> as.numeric()
  ) |> 
  bib2df::df2bib(file = "ref.bib")

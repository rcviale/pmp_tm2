lrets <- function(.data, .ticker_col, .px_col){
  .data |> 
    dplyr::group_by({{.ticker_col}}) |> 
    dplyr::mutate(ret = log({{.px_col}} / dplyr::lag({{.px_col}}, n = 1L))) |> 
    dplyr::ungroup() |> 
    dplyr::select(-{{.px_col}})
}

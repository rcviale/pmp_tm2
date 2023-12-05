fx_px <- function(.data, .px_col, .fx_col, .ticker_col, .date_col){
  .data |> 
    dplyr::mutate(px_usd = {{.px_col}} * {{.fx_col}}) |>  # Column with prices in USD
    dplyr::select({{.ticker_col}}, {{.date_col}}, px_usd) |> # Keep columns symbol, date and px_usd
    dplyr::arrange({{.ticker_col}}, {{.date_col}}) # Order by symbol and date
}

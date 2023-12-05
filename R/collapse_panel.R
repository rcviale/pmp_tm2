collapse_panel <- function(.data, 
                           .ticker_col, 
                           .date_col, 
                           .unit = "month"){
  .data |> 
    dplyr::arrange({{.ticker_col}}, {{.date_col}}) |> # Order by symbol and date
    dplyr::mutate(date = lubridate::ceiling_date({{.date_col}}, unit = .unit) - 1) |> # Convert
    # dates to the end of the month
    dplyr::group_by({{.ticker_col}}, {{.date_col}}) |> # Group by ticker and date
    dplyr::slice_tail(n = 1) |> # Take last observation in each group
    dplyr::ungroup() |> # Ungroup
    dplyr::arrange({{.ticker_col}}, {{.date_col}}) # Order by symbol and date
}

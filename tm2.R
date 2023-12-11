library(tidyverse)

##### RData Conversion #####
source("R/RData_rds.R")

# Names of the RData files
.rdatas <- list.files("Data")[endsWith(list.files("Data"), ".RData")]
# New names for the rds files
.rdss <- .rdatas |> 
  sub(pattern = "RData", replacement = "rds")

# Convert all the RData files to rds
purrr::map2(.x = .rdatas, 
            .y = .rdss, 
            .f = ~RData_rds(.rdata = .x,
                            .rds = .y,
                            .path = "Data/"))

rm(list = ls())



##### Bonds Data Treatment (+FX) #####
fx <- readr::read_rds("Data/FX.rds") |> # Open rds
  dplyr::mutate(date = lubridate::ceiling_date(date, unit = "month") - 1) # Convert
# dates to the end of the month

source("R/collapse_panel.R")
source("R/fx_px.R")

# Sell price of a 10y bond at t+1: there are 10y-1m to maturity, then, to
# interpolate the yield in t+1 of this 10y-1m bond, we can compute
# YTM_9y,t+1 + (11/12) * (YTM_10y,t+1 - YTM_9y,t+1) =: YTM_10y-1m,t+1

# Compute bond prices in USD, save rds
readr::read_rds("Data/BondData.rds") |> # Open rds
  dplyr::filter(maturity == "10Y" | maturity == "09Y") |>  # Filter 10y and 9y bonds
  # dplyr::mutate(symbol = sub(pattern = " Index", replacement = "", x = symbol), 
  #               # Get rid of the word Index in the symbol column 
  #               symbol = substr(symbol, 5, 7), # Keep only the maturity
  #               symbol = paste(symbol, country, sep = " ")) |> # Merge ticker and 
  # country in one column
  collapse_panel(symbol, date) |> # Convert to monthly data
  dplyr::select(-symbol) |> 
  tidyr::pivot_wider(names_from = maturity, values_from = PX_MID) |> 
  dplyr::mutate(`9Y11M` = `09Y` + (11/12) * (`10Y` - `09Y`),
                px_10y  = 100 / (1 + `10Y`)^10,
                px_9y11m   = 100 / (1 + `9Y11M`)^(9 + 11/12)) |> 
  dplyr::select(-`09Y`) |> 
  dplyr::left_join(fx, by = c("date","country")) |> # Left join with FX data
  dplyr::mutate(px_10y   = fx.rates * px_10y,
                px_9y11m = fx.rates * px_9y11m) |> 
  dplyr::select(-c(fx.rates, CRNCY)) |> 
  dplyr::group_by(country) |> 
  dplyr::mutate(b_ret = (px_9y11m / dplyr::lag(px_10y)) - 1) |> 
  dplyr::mutate(b_cum_ret  = dplyr::lag(slider::slide_dbl(.x = b_ret + 1,
                                                          .f = ~prod(.x),
                                                          .before = 10,
                                                          .complete = T))) |>
  dplyr::ungroup() |> 
  readr::write_rds("Data/bonds.rds") # Save rds



##### Price-to-book Ratio Data Treatment #####
# Price to Book Ratio
readr::read_rds("Data/BP.rds") |> 
  dplyr::rename(AUSTRALIA = `AUSTRALIA-DS Market - PRICE/BOOK RATIO`,
                BRITAIN = `UK-DS Market - PRICE/BOOK RATIO`,
                UNITED.STATES = `US-DS Market - PRICE/BOOK RATIO`,
                CANADA = `CANADA-DS Market - PRICE/BOOK RATIO`,
                GERMANY = `GERMANY-DS Market - PRICE/BOOK RATIO`,
                JAPAN = `JAPAN-DS Market - PRICE/BOOK RATIO`,
                NEW.ZEALAND = `NEW ZEALAN-DS Market - PRICE/BOOK RATIO`,
                NORWAY = `NORWAY-DS Market - PRICE/BOOK RATIO`,
                SWEDEN = `SWEDEN-DS Market - PRICE/BOOK RATIO`,
                SWITZERLAND = `SWITZ-DS Market - PRICE/BOOK RATIO`) |> 
  tidyr::pivot_longer(2:11, names_to = "country", values_to = "pb") |> 
  dplyr::mutate(date = lubridate::as_date(date)) |> 
  dplyr::arrange(country, date) |> # Order by symbol and date
  dplyr::mutate(date = lubridate::ceiling_date(date, unit = "month") - 1) |> # Convert
  # dates to the end of the month
  dplyr::group_by(country, date) |> # Group by country and date
  dplyr::slice_tail(n = 1) |> # Take last observation in each group
  dplyr::ungroup() |> # Ungroup
  readr::write_rds("Data/pb_conv.rds")



##### CPI Data Treatment #####
# CPI
readr::read_rds("Data/CPI.rds") |> 
  tidyr::pivot_longer(2:11, names_to = "country", values_to = "cpi") |> 
  dplyr::arrange(country, date) |> # Order by symbol and date
  dplyr::mutate(date = lubridate::as_date(date),
                date = lubridate::ceiling_date(date, unit = "month") - 1) |> # Convert
  # dates to the end of the month
  dplyr::group_by(country) |> 
  dplyr::mutate(cpi_rt = cpi / dplyr::lag(cpi) - 1) |> 
  dplyr::ungroup() |> 
  readr::write_rds("Data/cpi_conv.rds")



##### French Data Treatment #####
source("R/RData_rds.R")

RData_rds(.rdata = "fama_french.RData", .rds = "fama_french.rds")  

# Adjust Fama-French RF factor to percentage and isolate it
readr::read_rds("Data/fama_french.rds") |> 
  dplyr::select(date, RF) |> 
  dplyr::rename(rf = RF) |>
  dplyr::mutate(rf = rf / 100) |> 
  readr::write_rds("Data/ff_rf.rds")

##### Join FF with Bonds data #####
readr::read_rds("Data/ff_rf.rds") |> 
  dplyr::inner_join(readr::read_rds("Data/bonds.rds"), by = "date") |> 
  dplyr::mutate(bex_ret = b_ret - rf) |> 
  readr::write_rds("Data/bonds_ff.rds")



##### Stocks Data Treatment (+FX) & Join with Bonds and FF, Cumulative Returns #####
# source("R/RData_rds.R")

# Convert RData to rds
# RData_rds(.rdata = "Bloomberg Index.RData", .rds = "inds.rds")

# Load FX Data
fx <- readr::read_rds("Data/FX.rds") |> # Open rds
  dplyr::mutate(date = lubridate::ceiling_date(date, unit = "month") - 1) # Convert

# Compute linear returns for stock indices, join with bonds & FF, compute cumulative
# stock and bond returns
readr::read_rds("Data/inds.rds") |>
  dplyr::mutate(date = lubridate::ceiling_date(date, unit = "month") - 1) |> 
  dplyr::inner_join(fx, by = c("date", "country")) |> 
  dplyr::mutate(PX_LAST = fx.rates * PX_LAST) |> 
  dplyr::group_by(ticker) |> 
  dplyr::arrange(date) |> 
  dplyr::mutate(st_ret = PX_LAST / dplyr::lag(PX_LAST) - 1, 
                st_cum_ret = dplyr::lag(slider::slide_dbl(.x = st_ret + 1,
                                                          .f = ~prod(.x), 
                                                          .before = 10,
                                                          .complete = T))) |> 
  dplyr::ungroup() |>
  dplyr::left_join(readr::read_rds("Data/bonds_ff.rds"), 
                    by = c("date", "country")) |> 
  dplyr::mutate(st_ex_ret = st_ret - rf) |> 
  dplyr::group_by(country) |>
  dplyr::arrange(date) |> 
  dplyr::ungroup() |> 
  readr::write_rds("Data/stock_bonds_ff.rds")

# Plot for cumulative excess returns of stocks, in USD
# (readr::read_rds("Data/stock_bonds_ff.rds") |> 
#   dplyr::filter(!is.na(st_ex_ret)) |> 
#   dplyr::group_by(ticker) |> 
#   dplyr::arrange(date) |> 
#   dplyr::mutate(st_cumprod_ret = cumprod(tidyr::replace_na(st_ex_ret + 1, 1))) |> 
#   dplyr::ungroup() |> 
#   ggplot2::ggplot() +
#   ggplot2::geom_line(ggplot2::aes(x = date, y = st_cumprod_ret, col = country))) |> 
#   plotly::ggplotly()
  


##### Join CPI and PB with the rest ####
readr::read_rds("Data/stock_bonds_ff.rds") |> 
  dplyr::left_join(readr::read_rds("Data/cpi_conv.rds"), by = c("country", "date")) |> 
  dplyr::left_join(readr::read_rds("Data/pb_conv.rds"), by = c("country", "date")) |> 
  readr::write_rds("Data/measures_data.rds")



##### Compute B/P Ratio #####
readr::read_rds("Data/measures_data.rds") |> 
  dplyr::mutate(b_value  = `10Y` - cpi_rt,
                st_value = ifelse(!is.na(PX_LAST), log(1 / pb), NA)) |> 
  dplyr::arrange(date) |> 
  readr::write_rds("Data/all_data.rds")



##### VALUE AND MOMENTUM ####
readr::read_rds("Data/all_data.rds") |> 
  dplyr::group_by(date) |> 
  dplyr::mutate(st_mom_rank = rank(st_cum_ret, na.last = "keep"),
                st_mom_temp = st_mom_rank - mean(st_mom_rank, na.rm = T),
                st_mom_wgt  = ifelse(st_mom_temp < 0, 
                                -st_mom_temp / sum(st_mom_temp[which(st_mom_temp < 0)], na.rm = T),
                                st_mom_temp / sum(st_mom_temp[which(st_mom_temp > 0)], na.rm = T)),
                b_mom_rank  = rank(b_cum_ret, na.last = "keep"),
                b_mom_temp  = b_mom_rank - mean(b_mom_rank, na.rm = T),
                b_mom_wgt   = ifelse(b_mom_temp < 0, 
                                 -b_mom_temp / sum(b_mom_temp[which(b_mom_temp < 0)], na.rm = T),
                                 b_mom_temp / sum(b_mom_temp[which(b_mom_temp > 0)], na.rm = T))) |> 
  dplyr::mutate(st_val_rank = rank(st_value, na.last = "keep", ties.method = "first"),
                st_val_temp = st_val_rank - mean(st_val_rank, na.rm = T),
                st_val_wgt  = ifelse(st_val_temp < 0, 
                                     -st_val_temp / sum(st_val_temp[which(st_val_temp < 0)], na.rm = T),
                                     st_val_temp / sum(st_val_temp[which(st_val_temp > 0)], na.rm = T)),
                b_val_rank  = rank(b_value, na.last = "keep"),
                b_val_temp  = b_val_rank - mean(b_val_rank, na.rm = T),
                b_val_wgt   = ifelse(b_val_temp < 0, 
                                    -b_val_temp / sum(b_val_temp[which(b_val_temp < 0)], na.rm = T),
                                    b_val_temp / sum(b_val_temp[which(b_val_temp > 0)], na.rm = T))) |> 
  dplyr::group_by(country) |> 
  dplyr::filter(date>="1992-04-30") |>
  dplyr::mutate(st_mom_wgt_ret = dplyr::lag(st_mom_wgt) * st_ret,
                b_mom_wgt_ret  = dplyr::lag(b_mom_wgt) * b_ret,
                st_val_wgt_ret = dplyr::lag(st_val_wgt) * st_ret,
                b_val_wgt_ret  = dplyr::lag(b_val_wgt) * b_ret) |> 
  dplyr::group_by(date) |>
  dplyr::transmute(st_mom_ret = sum(st_mom_wgt_ret, na.rm = T),
                   b_mom_ret  = sum(b_mom_wgt_ret, na.rm = T),
                   st_val_ret = sum(st_val_wgt_ret, na.rm = T),
                   b_val_ret  = sum(b_val_wgt_ret, na.rm = T)) |> 
  dplyr::ungroup() |> 
  dplyr::distinct() |> 
  dplyr::mutate(st_cum_mom = cumprod(tidyr::replace_na(st_mom_ret + 1, 1)),
                b_cum_mom  = cumprod(tidyr::replace_na(b_mom_ret + 1, 1)),
                st_cum_val = cumprod(tidyr::replace_na(st_val_ret + 1, 1)),
                b_cum_val  = cumprod(tidyr::replace_na(b_val_ret + 1, 1))) |> 
  dplyr::filter(date <= "2020-02-01") |>
  readr::write_rds("Data/factor_returns.rds")

# Plot for cumulative factor returns
(readr::read_rds("Data/factor_returns.rds") |>
    dplyr::select(c(date, st_mom_ret, b_mom_ret, st_val_ret, b_val_ret)) |>
    tidyr::pivot_longer(-date, names_to = "factor", values_to = "ret") |>
    dplyr::group_by(factor) |> 
    dplyr::mutate(cum_ret = cumprod(1 + ret)) |> 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = date, y = cum_ret, col = factor))) |>
  plotly::ggplotly()



##### VALUE & MOMENTUM PORTFOLIOS #####
readr::read_rds("Data/factor_returns.rds") |>  
  dplyr::select(c(date, st_mom_ret, st_val_ret, b_mom_ret, b_val_ret)) |>
  tidyr::pivot_longer(-date, names_to = "factor", values_to = "ret") |>
  dplyr::mutate(class  = ifelse(stringr::str_starts(factor, "st"), "stocks", "bonds"),
                signal = ifelse(grepl("mom", factor), "mom", "val"),
                ret50  = 0.5 * ret,
                ret70  = ifelse(signal == "mom", 0.3 * ret, 0.7 * ret)) |> 
  dplyr::select(-factor) |> 
  dplyr::group_by(date, class) |> 
  dplyr::mutate(ret50 = sum(ret50),
                ret70 = sum(ret70),
                eqeq  = mean(c(ret50, ret70))) |> #FIXME
  dplyr::ungroup() |> 
  dplyr::select(-c(ret, signal)) |> 
  dplyr::distinct() |> 
  tidyr::pivot_longer(cols     = ret50:eqeq, 
                      names_to  = "portfolio",
                      values_to = "ret") |> 
  dplyr::mutate(portfolio = paste(class, portfolio, sep = "_")) |> 
  dplyr::select(-class) |> 
  readr::write_rds("Data/portfolio_rets.rds")

# Compute cumulative returns
readr::read_rds("Data/portfolio_rets.rds") |>
  dplyr::group_by(portfolio) |> 
  dplyr::mutate(ret = cumprod(ret + 1)) |> 
  dplyr::ungroup() |> 
  readr::write_rds("Data/portfolio_cumrets.rds")

# Plot for cumulative returns of Portfolios
(readr::read_rds("Data/portfolio_cumrets.rds") |> 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = date, y = ret, col = portfolio))) |> 
  plotly::ggplotly()



##### Benchmark Computation #####
readr::read_rds("Data/all_data.rds") |> 
  dplyr::filter(date >= "1991-01-01" & date <= "2020-02-01") |> 
  dplyr::select(date, country, st_ret, b_ret) |> 
  tidyr::pivot_longer(cols = st_ret:b_ret, 
                      names_to = "class", 
                      values_to = "ret") |> 
  dplyr::group_by(date) |> 
  dplyr::mutate(bench_ret = mean(ret, na.rm = T)) |> 
  dplyr::ungroup() |> 
  dplyr::select(-c(country, class, ret)) |> 
  dplyr::distinct() |> 
  readr::write_rds("Data/bench.rds")

# Plot
(readr::read_rds("Data/bench.rds") |> 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = date, y = bench_ret))) |> 
  plotly::ggplotly()

(readr::read_rds("Data/bench.rds") |> 
  mutate(cum_ret = cumprod(1+bench_ret)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = cum_ret))) |> 
  plotly::ggplotly()

# readr::read_rds("Data/factor_returns.rds") |> 
#   dplyr::left_join(readr::read_rds("Data/bench.rds") |> 
#                      )

##### Correlation Matrix #####
readr::read_rds("Data/portfolio_rets.rds") |> 
  rbind(readr::read_rds("Data/factor_returns.rds") |> 
                     dplyr::select(date:b_val_ret) |> 
                     tidyr::pivot_longer(cols      = -date,
                                         names_to  = "portfolio",
                                         values_to = "ret")) |> 
  rbind(readr::read_rds("Data/bench.rds") |> 
          dplyr::mutate(portfolio = "bench") |> 
          dplyr::filter(date >= "1992-04-30")) |>
  tidyr::pivot_wider(names_from  = portfolio,
                     values_from = ret) |>
  dplyr::select(-date) |> 
  as.matrix() |> 
  cor() |> 
  corrplot::corrplot()


  

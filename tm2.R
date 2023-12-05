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



##### Join Bonds and FX #####
fx <- readr::read_rds("Data/FX.rds") |> # Open rds
  dplyr::mutate(date = lubridate::ceiling_date(date, unit = "month") - 1) # Convert
# dates to the end of the month

source("R/collapse_panel.R")
source("R/fx_px.R")
source("R/lrets.R")

bonds <- readr::read_rds("Data/BondData.rds") |> # Open rds
  dplyr::filter(maturity == "10Y") |>  # Filter 10y bonds
  dplyr::mutate(symbol = sub(pattern = " Index", replacement = "", x = symbol), 
                # Get rid of the word Index in the symbol column 
                symbol = paste(symbol, country, sep = " ")) |> # Merge ticker and 
  # country in one column
  collapse_panel(symbol, date) |> # Convert to monthly data
  dplyr::mutate(PX_MID = 100 / (1 + PX_MID)) |> # Convert yields to a price series
  dplyr::left_join(fx, by = c("date","country")) |> # Left join with FX data
  fx_px(PX_MID, fx.rates, symbol, date) |>  # Convert PX to USD
  lrets(symbol, px_usd)

rm(fx, fx_px) # Clear environment

inds <- c("^AXJO", "^GSPTSE", "^GDAXI", "^N225", "^NZ50", "^OSEAX", 
          "^OMX", "^SSMI", "^FTSE", "^GSPC") # Stock index tickers for YF

# Download and save stock indexes data
# tidyquant::tq_get(x = inds, 
#                   get = "stock.prices",
#                   from = "1994-12-28",
#                   to = "2020-01-31") |> 
#   readr::write_rds("Data/yf_data.rds")

pxs_ind <- readr::read_rds("Data/yf_data.rds") |> # Open rds
  dplyr::select(symbol, date, adjusted) |> # Drop unused columns
  collapse_panel(symbol, date) |> # Convert to monthly data
  lrets(symbol, adjusted) # Compute log returns

bond_ind <- rbind(bonds, pxs_ind) |> # Combine bonds and stocks data sets
  dplyr::group_by(symbol) |> # Group by symbol
  dplyr::mutate(ret = cumsum(tidyr::replace_na(ret, 0))) |> # Compute cumulative 
  # log returns
  dplyr::ungroup() # Ungroup

rm(bonds, inds, lrets, collapse_panel, pxs_ind) # Clear environment

bond_ind |> 
  readr::write_rds("Data/bond_ind.rds") # Save combined data set



##### Plot for all Cumulative Returns #####
bond_ind <- readr::read_rds("Data/bond_ind.rds")

bond_ind |> 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = ret, col = symbol))



RData_rds <- function(.rdata, .rds, .path = "Data/"){
    # The first and second argument must be character vectors with the names of the files
  # (with the file extensions) that are to be converted, and the third, a string
  
  # Save the name of the object inside the RData file
  .object <- load(paste0(.path, .rdata))
  
  # Retrieve object and save it as .rds
  get(.object) |> 
    tibble::as_tibble() |> 
    readr::write_rds(paste0(.path, .rds))
  
  # Delete the RData file
  file.remove(paste0(.path, .rdata))
}

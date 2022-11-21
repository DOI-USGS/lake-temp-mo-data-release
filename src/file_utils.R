

sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  
  setwd(dsn)
  zip::zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

rds_to_csv <- function(in_file, out_file) {
  x <-readRDS(in_file)
  readr::write_csv(x, out_file)
}

feather_to_csv <- function(dir_in, dir_out) {
  # browser()
  # id files and prep file names
  filepath_in <- list.files(dir_in, full.names = TRUE)
  files_csv <- sub('.feather', '.csv', basename(filepath_in))
  filepath_out <- file.path(dir_out, files_csv)
  
  # check for ourt directory
  if(!dir.exists(dir_out)) {
    dir.create((dir_out))
  }
  
  # read data in
  data <- purrr::map(filepath_in, ~ arrow::read_feather(.x))
  
  # save as csv
  purrr::map2(data, filepath_out, 
              ~ readr::write_csv(.x, .y))
  
  csv_out <- list.files(dir_out, full.names = TRUE)
  return(csv_out)

}

rename_nml_files <- function(dir_in, dir_out) {
  # browser()
  # list model files and assign output directory
  model_files_orig <- paste0(list.files(dir_in, full.names = TRUE), '/glm3.nml')

  rename <- str_extract(model_files_orig, '(nhdhr.*)(?=\\/glm3)')

  model_files_rename <- file.path(dir_out, paste0(rename, '.nml'))

  if(!dir.exists(dir_out)) {
    dir.create((dir_out))
  }

  map2(.x = model_files_orig, .y = model_files_rename,
       ~ file.copy(from = .x, to = .y))

  # verify hashes
  sum_check <- map2(.x = model_files_orig, .y = model_files_rename,
       ~ check_sums(x = .x, y = .y))

  if(!all(unlist(sum_check)))
    stop("hash mismatch detected")
  # browser()

  return(model_files_rename)
}

find_temp_obs <- function(dir_in) {
  files <- list.files(dir_in, full.names = TRUE) %>% 
    .[grep('(in_data\\/field.*)', .)]
  
  return(files)
}

check_sums <- function(x, y){
  x <- md5sum(x) == md5sum(y)
  unname(x)
}
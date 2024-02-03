test_that("download data", {

  # Location of downloaded data
  folder      <- 'data'
  data_name   <- 'fire_department_census'
  folder_path <- paste0(folder, '/', data_name)

  # Test params
  total_of_rows    <- 500
  batch_size       <- 100
  expected_n_files <- total_of_rows/batch_size

  # Download data
  download_iowa_data(data_id   = 'hv43-6ksq',
                     folder    = folder,
                     data_name = data_name,
                     total_of_rows = total_of_rows,
                     batch_size    = batch_size)

  # Get downloaded files names
  filenames <- list.files(folder_path,
                          pattern    = paste0(data_name, ".*\\.csv$"), # Regex to match data_name_XXX.csv
                          full.names = TRUE)

  # It should be 5 files in the download folder
  expect_equal(length(filenames), expected_n_files)

  # Cleanup the download folder to not interfere with further tests
  file.remove(dir(
    folder_path,
    pattern = "*\\.csv$",
    full.names = TRUE
  ))
})

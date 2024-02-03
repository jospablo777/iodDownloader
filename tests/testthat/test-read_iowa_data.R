test_that("read data", {

  # Location of downloaded data
  folder        <- 'data'
  data_name     <- 'fire_department_census'
  folder_path   <- paste0(folder, '/', data_name)

  # Test params
  total_of_rows    <- 500
  batch_size       <- 100

  # read_iowa_data depends on the output of download_iowa_data
  download_iowa_data(data_id   = 'hv43-6ksq',
                     folder    = folder,
                     data_name = data_name,
                     total_of_rows = total_of_rows,
                     batch_size    = batch_size)

  # Read downloaded data
  df_read_test <- read_iowa_data(folder_path = folder,
                                 data_name   = data_name)

  # Eval result
  expect_equal(nrow(df_read_test), total_of_rows)

  # Cleanup the download folder to not interfere with further tests
  file.remove(dir(
    folder_path,
    pattern = "*\\.csv$",
    full.names = TRUE
  ))
})

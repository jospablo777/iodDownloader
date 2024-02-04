# ----------------------------------------------------------------
# Download data in batches

#' Data set batch downloader
#'
#' @description `download_iowa_data()` downloads specified data from the Iowa Open Data portal (https://data.iowa.gov/).
#'
#' @param data_id A string. You get it from the data set page in. This can be found on the url of your data set of interest; for example the url for Iowa Fire Department Census data set is "https://data.iowa.gov/Emergency-Management/Iowa-Fire-Department-Census/hv43-6ksq/about_data" here the id is the value nex to the data set name on the right (i.e., "hv43-6ksq").
#' @param folder A string. The path to the folder you will save the data.
#' @param data_name A string. How your files will be called in your file system.
#' @param total_of_rows An integer. The size of the dataset, or the amount of rows you want to pull from the API.
#' @param batch_size An integer. The size of the batch (in rows) you want to pull, the default is 1M rows.
#'
#' @return Nothing. The function save the batches in .csv files in the folder you've chosen for.
#' @export
#'
#' @examples
#' # Here we download the Iowa Fire Department Census data set.
#' # If you go to the dataset web page
#' # (https://data.iowa.gov/Emergency-Management/Iowa-Fire-Department-Census/hv43-6ksq/about_data),
#' # you will find that the total number of rows of this data set
#' # (to the date, 26-Jan-2024) is 738, so if we want the whole data
#' # set it is okay if we set `total_of_rows = 1000`, and `batch_size = 1000`.
#' download_iowa_data(data_id   = 'hv43-6ksq',
#'                    folder    = 'data',
#'                    data_name = 'fire_department_census',
#'                    total_of_rows = 1000,
#'                    batch_size    = 1000)
download_iowa_data <- function(data_id, folder, data_name, total_of_rows=10000, batch_size=5000) {
  options(readr.show_col_types = FALSE, # Hide readr messages
          readr.show_progress = FALSE)

  batch_size_str <- format(batch_size, scientific = F)
  n_iterations   <- ceiling(total_of_rows/batch_size)

  # Add a progress bar to track the advance in the data pulling.
  pb <- utils::txtProgressBar(min = 0,
                              max = n_iterations,
                              style = 3,
                              width = 50,
                              char = "=")

  # we will pull our data in batches of 1 million of rows
  offset <- 0

  # Loop to pull the data
  for (i in 1:n_iterations) {

    # The API does not like scientific notation in the requests
    offset_str <- format(offset, scientific = F)

    # URL
    url <- paste0('https://data.iowa.gov/resource/',
                  data_id,
                  '.csv?$order=:id&$limit=',
                  batch_size_str,
                  '&$offset=',
                  offset_str)

    # Request to the API
    request <- httr::GET(url = url)

    # Extract content
    df <- httr::content(request)

    file_final_folder <- paste0(folder, '/', data_name, '/')
    file_name <- paste0(file_final_folder, data_name, '_', i, '.csv')

    # If data folder doesn't exist, create it
    if (!file.exists(folder)){
      # Creates the directory
      dir.create(file.path(folder))
    }

    # We will save our data locally to save time when re-reading the data.
    # But first we must check if the directory for our data exist
    if (file.exists(file_final_folder)){

      readr::write_csv(df, file_name, num_threads = 1) # just saves the file if directory exists
    } else {
      # Creates the directory
      dir.create(file.path(file_final_folder))

      # Saves the file
      readr::write_csv(df, file_name, num_threads = 1)
    }

    # Update offset for next batch
    offset <- offset + batch_size

    # Progress bar update
    utils::setTxtProgressBar(pb, i)
  }
}

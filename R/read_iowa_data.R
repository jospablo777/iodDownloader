# ----------------------------------------------------------------
# General form to read all the downloaded files into a single df

#' Read the data pulled from Iowa Open Data portal (https://data.iowa.gov/).
#'
#' `read_iowa_data` reads the data pulled from the Iowa Open Data portal API with the function `download_iowa_data`
#'
#' @param folder_path A string. The path to the folder you saved your data.
#' @param data_name A string. How your data set common files are called in your file system.
#'
#' @return A data frame. It reads into the memory the chunks of your data set downloaded in batches from the Iowa Open Data portal (https://data.iowa.gov/).
#' @export
#'
#' @examples
#' # Read the fire_department_census data you downloaded with th download_iowa_data() function. The folder 'data' with the data is located in your R project working directory.
#' read_iowa_data(folder_path='data', data_name='fire_department_census')
read_iowa_data <- function(folder_path='data', data_name) {
  options(readr.show_col_types = FALSE, # hide readr messages
          readr.show_progress = FALSE)

  folder_path <- paste0(folder_path, '/', data_name, '/') # Ubicate the specific data set folder
  filenames <- list.files(folder_path,
                          pattern=paste0(data_name, ".*\\.csv$"), # Regex to match data_name_XXX.csv
                          full.names=TRUE)

  # Add a progress bar to track the advance in the data pulling.
  pb <- txtProgressBar(min = 0,
                       max = length(filenames),
                       style = 3,
                       width = 50,
                       char = "=")

  for(i in 1:length(filenames)){

    # First read file is used to instantiate the main df
    if (i == 1) {
      df <- readr::read_csv(filenames[i])
      col_specs <- readr::spec(df)
    } else {
      df_ <- readr::read_csv(filenames[i], col_types=col_specs)
      df <- dplyr::bind_rows(df, df_)
    }

    # Update progress bar
    setTxtProgressBar(pb, i)
  }

  # sort data by date
  # df <- df %>% arrange(date)

  return(df)

}

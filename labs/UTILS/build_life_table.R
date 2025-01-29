#!/usr/local/bin/Rscript --slave
#
argv <- commandArgs(TRUE)

dirpath <- as.character(argv[1])    # '~/Dropbox/EDA-U-PARIS/LIFE_TABLES'
outfile <- as.character(argv[2])    # 'full_life_table.Rds'

#
if (! dir.exists(dirpath)){
  cat(stringr::str_c("Directory does not exist: ", dirpath, "\n"))
  quit(status = 1)
}

#
if (file.exists(outfile)){
  cat(stringr::str_c("File already exists: ", outfile, "\n"))
  quit(status = 1)
}

#
require(magrittr, quietly = TRUE)
require(foreach, quietly = TRUE)

#
#
#' retype
#'
#' @param data a dataframe loaded from Human Mortality Database (hmd.org) using readr::read_delim
#'
#' @return a dataframe with trimmed column names and correctly typed columns
#'
#'
#' @examples
retype <- function(data){

  require(magrittr, quietly = TRUE)

  data %>%
    dplyr::rename_all(stringr::str_trim) ->
    data

  double_cols <-  c("mx", "qx", "ax", "ex")
  integer_cols <- setdiff(names(data), c(double_cols, "Country", "Gender"))

  data %>%
    dplyr::mutate_at(double_cols, as.numeric, ) %>%
    dplyr::mutate_at(integer_cols, as.integer) %>%
    tidyr::drop_na(Age) -> data

  data
}

#
country_code <- list(fr_t='FRATNP',
                     fr_c='FRACNP',
                     be='BEL',
                     gb_t='GBRTENW',
                     gb_c='GBRCENW',
                     nl='NLD',
                     it='ITA',
                     swe='SWE',
                     sp='ESP',
                     us='USA')

#
countries <- c('fr_t', 'gb_t', 'nl', 'it', 'sp', 'swe', 'us')

#
country_names <- list(fr_t='France',     # total population
                      fr_c='France',      # civilian population
                      be='Belgium',
                      gb_t='England & Wales',    # total population
                      gb_c='England & Wales',    # civilian population
                      nl='Netherlands',
                      it='Italy',
                      swe='Sweden',
                      sp='Spain',
                      us='USA')

#
gender_names <- list('b'='Both',
                     'f'='Female',
                     'm'='Male')

#
# naming the dataframes
df_names <- as.character(outer(c("b", "f", "m"),
                               countries,
                               FUN = function(s, c) stringr::str_c("data", s, c, sep="_")))

#  locating the files
suffix <- 'ltper_1x1.txt'
mid <- 'ltper_1x1/'

foreach::foreach(country=iterators::iter(countries), .combine = c) %do% {
  purrr::map2_chr(.x = c("both/b", "female/f", "male/m"),
                  .y = c("b", "f", "m"),
                  .f = ~ stringr::str_c(dirpath,
                                        '/lt_',
                                        .x ,
                                        mid,
                                        country_code[[country]],
                                        ".",
                                        .y,
                                        suffix))
}  -> file_paths

#
out <- purrr::map(file_paths,
                  ~ readr::read_delim(., delim = " ",
                                      skip = 2,
                                      col_types = readr::cols(),
                                      col_names = TRUE))

# naming the list elements
names(out) <-  df_names

#
out %>%
# Adding a Country column  to every dataframe
  purrr::map2(.x = .,
            .y= stringr::str_split(df_names,
                                   pattern = 'data_[m,f,b]_',
                                   simplify = TRUE)[,2],
            ~ dplyr::mutate(.x, Country=country_names[[.y]])) %>%
# Adding a Gender column to every dataframe
  purrr::map2(.x = .,
            .y = stringr::str_split(df_names, pattern = '_', simplify = TRUE)[,2],
            ~ dplyr::mutate(.x, Gender=gender_names[[.y]])) %>%
  purrr::map(retype) %>%
  dplyr::bind_rows() -> life_table

#
readr::write_rds(life_table, outfile)

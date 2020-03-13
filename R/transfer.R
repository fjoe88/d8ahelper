

# Transfer ------------------------------------------------------------------------------------

#'Copy unique values to clipboard.
#'
#'1) if input is a dataframe, do not return back a df \cr
#'2) by default will assume lotids being copied and format them unless set format_lotid = FALSE \cr
#'3) if format_step == TRUE, return a comma separated step name string instead \cr
#'4) if save_csv == TRUE, will save output to '1.lot_list.csv' \cr
#'
#'@param df a character string copied from clipboard, or a dataframe column
#'@param col column name if df is a data frame
#'@return a comma separated string w/ duplicates removed and a dataframe of unique values
#'
#'@example
#'copy_unique(df = "apple banana apple orange", format_lotid = FALSE)

copy_unique <-
  function(df = paste(readClipboard(), collapse = ","),
           col = "lotid",
           sep = ",",
           quotes = FALSE,
           format_lotid = TRUE,
           format_step = FALSE,
           save_csv = FALSE,
           ...) {

    if (!(is.character(df)|is.data.frame(df)))
      stop("only data.frame objects or character vectors are accepted")

    if (is.data.frame(df)) {
      if (tolower(col) == "lotid") {
        df <- wafer::format_lotid(df)
        df <- wafer::create_startlot(df)
      }
      unique.val <- unique(df[[col]])
      str.to.copy <- paste(unique.val, collapse = sep)
      utils::writeClipboard(str.to.copy)
      return(df)

    } else{

      # early return if to format step names
      if (format_step == TRUE){
        dt <- data.table::setDT(stringr::str_split(df, ","))
        dt <- unique(dt)

        col = "step"
        names(dt) <- col

        if (quotes == TRUE) {
          str <- paste(paste0("'", dt[[col]], "'"), collapse = ",")
        }

        utils::writeClipboard(str)
        return(dt)
      }

      if (length(df) > 1) {
        df = paste(df, collapse = ",")
      }

      # early return if nothing copied
      if (!grepl("[[:alnum:]]", df)) {return()}

      df <-
        stringr::str_replace_all(df, pattern = "\\s+", replacement = ",")
      df <-
        stringr::str_replace_all(df, pattern = "\n", replacement = ",")
      df <-
        stringr::str_replace_all(df, pattern = "^,+|,+$", replacement = "")
      df <-
        stringr::str_replace_all(df, pattern = ",+", replacement = ",")

      dt <- data.table::setDT(stringr::str_split(df, ","))

      names(dt) <- col

      if (format_lotid == TRUE) {
        names(dt) <- "lotid"
        dt <- wafer::format_lotid(dt)
        dt <- wafer::create_startlot(dt)
      }

      if (quotes == TRUE) {
        dt[[col]] <- paste0("'", dt[[col]], "'")
      }

      data.table::setkeyv(dt, col)
      dt <- unique(dt)

      utils::writeClipboard(paste(dt[[col]], collapse = sep))

      if (save_csv == TRUE) {
        d8ahelper::save_csv(dt, file.name = "lot_list.csv")
      }

      return(dt)
    }
  }


#' Fast way to output CSV files
#'
#' Will copy file path to clipboard for convinient file locate or remove (via unlink())
#'
#' @param df a data frame
#' @param time_as_chr specify if to convert datetime columns to character before export
#' @param path ouput folder, by default use 'r_output' folder under root directory
#' @param file.name character name for output file name
#' @param folder specify output folder under 'path'

save_csv <- function(df,
                     file.name = "r_output.csv",
                     path = here::here("r_output"),
                     time_as_chr = FALSE,
                     folder = "",
                     overwrite = FALSE,
                     use_readr = FALSE,
                     ...) {

  if (!is.data.frame(df)) return(NA)

  dir.create(file.path(path),
             showWarnings = FALSE)

  if (folder != "") {
    dir.create(file.path(path, folder),
               showWarnings = FALSE)
  }

  file = file.path(path,
                   folder,
                   file.name)

  if (file.exists(file) && overwrite == FALSE) stop(glue::glue("{file} already exists!"))

  if (file.exists(file) && overwrite == TRUE) {
    unlink(file)
  }

  if (time_as_chr == TRUE) {
    df <- d8ahelper::convert_time_to_chr(df)
  }

  if (use_readr == FALSE){
    data.table::fwrite(df,
                       file = file,
                       ...)
  }

  if (use_readr == TRUE){
    readr::write_csv(df,
                     path = file,
                     ...)
  }

}

#' a wrapper function of save_csv that applies to a list of data frames

save_csv_from_a_list <- function(lst, overwrite = FALSE, ...)
{
  if (!is.list(lst) || is.data.frame(lst)) {
    obj <- deparse(match.call(expand.dots = TRUE)[-1][[1]])
    stop(glue::glue("invalid argument: '{obj}' is not a list"))
  }
  i <- 1
  for (i in seq_along(lst)) {
    if (is.null(names(lst)[i])) {
      named_as <- i
      i <<- i + 1
    }
    if (!is.null(names(lst)[i])) {
      named_as <- names(lst)[i]
      i <<- i + 1
    }

    #guard clause for empty df
    if (!is.data.frame(lst[[i]])) {return(NA)}

    d8ahelper::save_csv(lst[[i]], file.name = glue::glue("{named_as}.csv"),
                        overwrite = overwrite, ...)
  }
}



#' Fast way to load CSV files
#'
#' @param file.name character name for output file name
#' @param path target folder, by default use 'r_output' folder under root directory
#' @param folder specify output folder under 'path'
#' @param load a boolean value, if == "FALSE" then only print to console the existing files

load_csv <- function(file = 'r_output.csv',
                     path = here::here("r_output"),
                     folder = "",
                     full_path = FALSE,
                     load = TRUE,
                     ...) {
  if (load == FALSE) {
    print(list.files(path, pattern = ".csv"))
  }

  if (load == TRUE) {
    if (full_path == TRUE) {
      file = file
    } else {
      dir.create(file.path(path,
                           folder),
                 showWarnings = FALSE)

      file = file.path(path,
                       folder,
                       file)
    }

    data.table::fread(file = file, ...)
  }
}

#' Wrapper function for convinient copy from Excel into a data frame ('Trick' via @SuzanBaert on twitter)

from_excel <- function(header = TRUE) {
  read.table(file = "clipboard", sep = "\t", header = header)
}

#'  via @haozhu233 (on GitHub)
#'  write fixed width format text file

write_fwf = function(dt, file, width,
                     justify = "l", replace_na = "NA") {
  fct_col = which(sapply(dt, is.factor))
  if (length(fct_col) > 0) {
    for (i in fct_col) {
      dt[,i] <- as.character(dt[,i])
    }
  }
  dt[is.na(dt)] = replace_na
  n_col = ncol(dt)
  justify = unlist(strsplit(justify, ""))
  justify = as.character(factor(justify, c("l", "r"), c("-", "")))
  if (n_col != 1) {
    if (length(width) == 1) width = rep(width, n_col)
    if (length(justify) == 1) justify = rep(justify, n_col)
  }
  sptf_fmt = paste0(
    paste0("%", justify, width, "s"), collapse = ""
  )
  tbl_content = do.call(sprintf, c(fmt = sptf_fmt, dt))
  tbl_header = do.call(sprintf, c(list(sptf_fmt), names(dt)))
  out = c(tbl_header, tbl_content)
  writeLines(out, file)
}

#' wrapper function to data.table::fread to convert blank cells to NA at reading
#' for some reason data.table::fread do not convert blank cell to NA even with na.string = ""
#' see https://stackoverflow.com/questions/51019041/blank-space-not-recognised-as-na-in-fread

fread2 <- function(file, ...) {
  dt <- data.table::fread(file = file, ...)
  dt[dt == ""] <- NA
  dt
}

#' load files paths in specified location

load_files <- function(load_from,
                       pattern = ".csv",
                       read_as_df = TRUE,
                       avoid = "^_|archive",
                       full.names = TRUE,
                       load = FALSE,
                       ...) {
  #'
  #' @param load_from a path for where files are load from
  #' @param load a logical for if actually read the files; if pattern is not ".csv", then read as text
  #' @param read_as_df a logical, if to read as data frame, else read as text

  files <-
    list.files(load_from, full.names = full.names, pattern = pattern, ...)
  names(files) <-
    str_extract(list.files(load_from, full.names = FALSE, , pattern = pattern),
                "^[^\\.]*")
  files <- files[!grepl(avoid, names(files))]

  if (load == TRUE) {
    if (read_as_df == TRUE) {
      file.list <-
        lapply(as.list(files), function(x)
          readr::read_csv(x, col_types = cols()))
      return(file.list)
    }

    if (read_as_df == FALSE) {
      file.list <-
        lapply(as.list(files), function(x)
          readtext::readtext(file = x, verbosity = FALSE)$text)
      return(file.list)
    }
  }

  return(files)
}


# Encryption ----------------------------------------------------------------------------------

#' A wrapper function for generating, store, and read key
#' @example
#' key <- gen_key(folder = "folder_where_key_is_stored")

gen_key <- function(file = ".key",
                    key.wrap = TRUE) {


  if (!file.exists(here::here(file))){
    key <- sodium::keygen()
    saveRDS(key, file = here::here(file), compress = FALSE)
    return(key)
  } else {
    key <- readRDS(file = here::here(file))
  }

  if (key.wrap == TRUE) {
    key <- cyphr::key_sodium(key)
  } else {key}
}

#' A wrapper function for encrypting/delete file using key
#' @example
#' d8ahelper::save_csv(iris, file.name = "iris.csv")
#' d8ahelper::encrypt("iris.csv", "iris_locked.csv")
#' d8ahelper::decrypt("iris_locked.csv", "iris_unlocked.csv")

encrypt <- function(file,
                    dest,
                    key = gen_key(),
                    keep.original = FALSE) {

  file.path = here::here(file)
  dest.path = here::here(dest)

  cyphr::encrypt_file(path = file.path,
                      key = key,
                      dest = dest.path)

  if (keep.original == FALSE && file.exists(file.path)) {
    unlink(file.path)
  }
}

#' A wrapper function for decrypting/delete file using key
#' @example
#' d8ahelper::save_csv(iris, file.name = "iris.csv")
#' d8ahelper::encrypt("iris.csv", "iris_locked.csv")
#' d8ahelper::decrypt("iris_locked.csv", "iris_unlocked.csv")

decrypt <- function(file,
                    dest,
                    key = gen_key(),
                    keep.original = FALSE) {

  file.path = here::here(file)
  dest.path = here::here(dest)

  cyphr::decrypt_file(path = file.path,
                      key = key,
                      dest = dest.path)

  if (keep.original == FALSE && file.exists(file.path)) {
    unlink(file.path)
  }
}

#' A wrapper function for the workflow of decryption, source and encryption of a '.R' file

open_encrypted <- function(file_actual,
                           file_encrypted,
                           encrypt = FALSE) {

  if (file.exists(file_actual) && encrypt == FALSE) {
    stop("decrypted file already exists")
  } else if (file.exists(file_actual) && encrypt == TRUE) {
    d8ahelper::encrypt(file_actual, file_encrypted)
  }

  d8ahelper::decrypt(file = file_encrypted,
                     dest = file_actual)

  source(here::here(file_actual))

  d8ahelper::encrypt(file = file_actual,
                     dest = file_encrypted)
}

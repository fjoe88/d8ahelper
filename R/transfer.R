
#'Copy unique values to clipboard.
#'
#'1) if input is a data.frame, do not return back a df
#'2) by default will assume lotids being copied and format them unless set format_lotid = FALSE
#'3) if format_step == TRUE, return a comma separated step name string instead
#'4) if save_csv == TRUE, will save output to '1.lot_list.csv'
#'
#'@param df a character string copied from clipboard, or a dataframe column
#'@param col column name if df is a data frame
#'@return a comma separated string w/ duplicates removed and a dataframe of unique values
#'@export
#'
#'@example
#'copy_unique(df = "apple banana apple orange", format_lotid = FALSE)

copy_unique <-
  function (df = paste(utils::readClipboard(), collapse = ","),
            col = "value",
            sep = ",",
            reduce_only = FALSE,
            fix_len = FALSE,
            quotes = FALSE,
            save_csv = FALSE,
            ...)
  {
    if (!(is.character(df) | is.data.frame(df)))
      stop("only data.frame objects or character vectors are accepted")
    if (is.data.frame(df)) {
      unique.val <- unique(df[[col]])
      str.to.copy <- paste(unique.val, collapse = sep)
      if (!(Sys.info()[['sysname']] == "Linux")) {
        utils::writeClipboard(str.to.copy)
      }
      return(df)
    }
    else {
      if (reduce_only == TRUE) {
        reduced_clip <- unique(readClipboard())
        if (fix_len == TRUE) {
          max_len <- max(sapply(reduced_clip, function(x) {
            nchar(as.character(x))
          }))
          reduced_clip <- stringr::str_pad(reduced_clip,
                                           7, pad = "0")
        }
        if (!(Sys.info()[['sysname']] == "Linux")) {
          writeClipboard(reduced_clip)
        }
        return(reduced_clip)
      }
      if (length(df) > 1) {
        df = paste(df, collapse = ",")
      }
      if (!grepl("[[:alnum:]]", df)) {
        return()
      }
      df <- stringr::str_replace_all(df, pattern = "\\s+",
                                     replacement = ",")
      df <-
        stringr::str_replace_all(df, pattern = "\n", replacement = ",")
      df <- stringr::str_replace_all(df, pattern = "^,+|,+$",
                                     replacement = "")
      df <-
        stringr::str_replace_all(df, pattern = ",+", replacement = ",")
      dt <- data.table::setDT(stringr::str_split(df, ","))
      names(dt) <- col
      if (quotes == TRUE) {
        dt[[col]] <- paste0("'", dt[[col]], "'")
      }
      data.table::setkeyv(dt, col)
      dt <- unique(dt)
      if (!(Sys.info()[['sysname']] == "Linux")) {
        utils::writeClipboard(paste(dt[[col]], collapse = sep))
      }
      if (save_csv == TRUE) {
        d8ahelper::save_csv(dt, file.name = "lot_list.csv")
      }
      return(dt)
    }
  }


#' A custom way to output CSV files
#'
#' @param df a data frame
#' @param time_as_chr specify if to convert datetime columns to character before export
#' @param path ouput folder, by default use 'r_output' folder under root directory
#' @param file.name character name for output file name
#' @param folder specify output folder under 'path'
#' @export

save_csv <- function(df,
                     file.name = "r_output.csv",
                     path = here::here("r_output"),
                     folder = "",
                     overwrite = FALSE,
                     use_readr = FALSE,
                     as_chr = TRUE,
                     ...) {

  if (!is.data.frame(df)) return(NA)

  dir.create(file.path(path),
             showWarnings = FALSE)

  if (folder != "") {
    dir.create(file.path(path, folder),
               showWarnings = FALSE)
  }

  datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

  if (file.name == "r_output.csv") {
    file.name <- glue::glue("r_output_{datetime}.csv")
  }

  file = file.path(path,
                   folder,
                   file.name)

  if (file.exists(file) && overwrite == FALSE) stop(glue::glue("{file} already exists!"))

  if (file.exists(file) && overwrite == TRUE) {
    unlink(file)
  }

  if (as_chr == TRUE) {
    df <- df %>% dplyr::mutate_all(as.character)
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


#' A wrapper function of save_csv that applies to a list of data frames
#'
#' @param lst
#' @param overwrite
#' @param use_name
#' @param use_clean_names
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
save_csv_from_a_list <- function(lst,
                                 overwrite = FALSE,
                                 use_name = FALSE,
                                 use_clean_names = TRUE,
                                 ...)
{
  lst[is.na(lst)] <- NULL
  lst[!sapply(lst, function(x) is.data.frame(x)
  )] <- NULL

  if (use_clean_names == TRUE) {
    names(lst) <- janitor::make_clean_names(names(lst), case = "snake")
  }


  if (!is.list(lst) || is.data.frame(lst)) {
    obj <- deparse(match.call(expand.dots = TRUE)[-1][[1]])
    stop(glue::glue("invalid argument: '{obj}' is not a list"))
  }

  i <- 1

  for (i in seq_along(lst)) {
    if (is.null(names(lst)[i]) || use_name == FALSE) {
      named_as <- i
      i <<- i + 1
    } else {
      named_as <- names(lst)[i]
      i <<- i + 1
    }

    d8ahelper::save_csv(lst[[i]],
                        file.name = glue::glue("{named_as}.csv"),
                        overwrite = overwrite,
                        ...)
  }
}


#' A custom way to load CSV files
#'
#' @param file.name character name for output file name
#' @param path target folder, by default use 'r_output' folder under root directory
#' @param folder specify output folder under 'path'
#' @param load a boolean value, if == "FALSE" then only print to console the existing files
#' @export

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
    if(!grepl("\\.csv$", file)){
      file <- paste0(file, ".csv")
    }

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

    data.table::fread(file = file, na.strings = c("", "NA"), ...)
  }
}


#' Wrapper function for convenient copy from Excel into a data frame ('Trick' via SuzanBaert on twitter)
#'
#' @param header
#'
#' @return
#' @export
#'
#' @examples
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
#' https://stackoverflow.com/questions/51019041/blank-space-not-recognised-as-na-in-fread
#'
#' @param file
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fread2 <- function(file, ...) {
  dt <- data.table::fread(file = file, ...)
  dt[dt == ""] <- NA
  dt
}

#' load files paths in specified location
#' @param load_from a path for where files are load from
#' @param load a logical for if actually read the files; if pattern is not ".csv", then read as text
#' @param read_as_df a logical, if to read as data frame, else read as text
#' @export
#'
load_files <- function(load_from,
                       pattern = "\\.csv$",
                       read_as_df = TRUE,
                       use_fread = TRUE,
                       avoid = "^_|archive",
                       full.names = TRUE,
                       load = FALSE,
                       ...) {

  files <-
    list.files(load_from, full.names = full.names, pattern = pattern)
  names(files) <-
    stringr::str_extract(list.files(load_from, full.names = FALSE, pattern = pattern),
                         "^[^\\.]*")
  files <- files[!grepl(avoid, names(files))]

  if (load == TRUE) {
    if (read_as_df == TRUE) {
      file.list <-
        lapply(as.list(files), function(x){

          if(use_fread == TRUE){
            df <- data.table::fread(x, na.strings = c("", "NA"))
            df <- tibble::as_tibble(df)
          }

          if (use_fread == FALSE){
            read.csv(x, ...)
            # df <- tibble::as_tibble(df)
          }

        })

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



#' copy files to another directory
#'
#' @param from
#' @param to
#' @param pattern
#' @param overwrite
#' @param copy.mode
#' @param copy.date
#'
#' @return
#' @export
#'
#' @examples
file.copy.content.only <- function(from,
                                   to,
                                   pattern = ".*",
                                   overwrite = FALSE,
                                   copy.mode = TRUE,
                                   copy.date = FALSE){

  list_of_files <- list.files(from, pattern)
  file.copy(file.path(from,list_of_files),
            to,
            overwrite = overwrite,
            copy.mode = copy.mode,
            copy.date = copy.date)
}



#' Perform a non intrusive inspection of a given directory and return a dataframe with essential file information sorted by file size
#'
#' @param dir
#' @param recursive
#' @param pattern
#' @param silence
#'
#' @return
#' @export
#'
#' @examples
insp_dir <- function(dir,
                     recursive = TRUE,
                     pattern = ".*",
                     silence = TRUE) {
  r_names <-
    list.files(dir,
               pattern = pattern,
               recursive = recursive,
               full.names = F)

  if (length(r_names) == 0) {
    print(glue::glue("all dirs and sub-dirs are empty within {dir}"))
    return(NA)
  }

  f_names <-
    list.files(dir,
               pattern = pattern,
               recursive = recursive,
               full.names = T)

  df <- data.frame(
    "names" = stringr::str_remove_all(r_names, ".*/"),
    "rel_file_name" = r_names,
    "full_file_name" = f_names,
    "size_mb" = sapply(f_names, function(x) {
      round(file.info(x)$size / (1024 * 1024), digits = 2)
    })
  )

  rownames(df) <- NULL

  df <- df %>% arrange(-size_mb)

  df <- df %>% mutate(
    size = case_when(
      size_mb > 2000 ~ "****",
      size_mb > 1000 ~ "***",
      size_mb > 100  ~ "**",
      size_mb > 10   ~ "*",
      TRUE ~ ""
    )
  )

  df_p <- df %>%
    select(rel_file_name, size_mb, size) %>%
    filter(size_mb > 100)

  if (silence == FALSE) {
    print(df_p)
  }

  return(df)
}

#' given a directory, delete files given the regex pattern excluding files matching to 'avoid' arg
#' @param pattern a regex string, used to match files to be deleted
#' @param avoid a regular expression string, matched files will not be deleted
#' @export

clear_dir <- function(dir,
                      pattern = ".csv",
                      recursive = F,
                      avoid = "^_") {

  df <- insp_dir(dir,
                 pattern = pattern,
                 recursive = recursive)

  if (!is.data.frame(df)) {
    return(NA)
  }

  to_excl <- sapply(df$names, function(x){
    grepl(x, pattern = avoid)
  })

  file_to_remove <- df$full_file_name[!to_excl]

  #delete
  sapply(file_to_remove, unlink)

  return(file_to_remove)
}


# Encryption ----------------------------------------------------------------------------------



#' A wrapper function for sodium::keygen to generate, convert keys
#'
#' @param key_file
#' @param convert_to_sodium_key
#'
#' @return
#' @export
#'
#' @examples
gen_key <- function(key_file,
                    convert_to_sodium_key = FALSE) {

  key <- sodium::keygen()
  saveRDS(key, file = key_file, compress = FALSE)

  if (convert_to_sodium_key == TRUE) {
    key <- cyphr::key_sodium(key)
  }

  return(key)
}


#' A wrapper function to encrypt file using a key
#'
#' @param file_orig
#' @param file_enc
#' @param key_file
#'
#' @return
#' @export
#'
#' @examples
encrypt <- function(file_orig,
                    file_enc,
                    key_file) {

  if (file.exists(key_file)){
    key <- readRDS(file = key_file)
    key <- cyphr::key_sodium(key)
  }

  if (!file.exists(key_file)){
    key <- gen_key(key_file,
                   convert_to_sodium_key = TRUE)
  }

  cyphr::encrypt_file(path = file_orig,
                      key = key,
                      dest = file_enc)

  if (file.exists(file_orig)) {
    unlink(file_orig)
  } else {
    stop("Exit: file to encrypt does not exist.")
  }
}


#' A wrapper function to decrypt file using a key
#'
#' @param file_enc
#' @param file_orig
#' @param key_file
#'
#' @return
#' @export
#'
#' @examples
decrypt <- function(file_enc,
                    file_orig,
                    key_file) {

  if (file.exists(key_file)){
    key <- readRDS(file = key_file)
  }

  if (!file.exists(key_file)){
    stop(glue::glue("did not find key in {key_file}"))
  }

  key <- cyphr::key_sodium(key)

  cyphr::decrypt_file(path = file_enc,
                      key = key,
                      dest = file_orig)

  if (file.exists(file_enc)) {
    unlink(file_enc)
  } else {
    stop("Exit: file to decrypt does not exist.")
  }
}



#' A wrapper function for the workflow of decrypt, source and encrypt back files
#' @param encrypt a bool, if TRUE then encrypt the file with key_file
#' @export

open_encrypted <- function(file_actual,
                           file_encrypted,
                           encrypt = FALSE,
                           key_file,
                           file_type = "r") {

  if (file.exists(file_actual) && encrypt == FALSE) {
    stop("decrypted file already exists, try use encrypt = TRUE if to encrypt the file first")
  } else if (file.exists(file_actual) && encrypt == TRUE) {
    encrypt(file_orig = file_actual,
            file_enc = file_encrypted,
            key_file = key_file)
  }

  decrypt(file_orig = file_actual,
          file_enc = file_encrypted,
          key_file = key_file)

  if (file_type == "r") {
    source(file_actual)
  }

  if (file_type == "csv") {
    df <- readr::read_csv(file = file_actual)
  }

  encrypt(file_enc = file_encrypted,
          file_orig = file_actual,
          key_file = key_file)

  if (file_type == "csv") {
    return(df)
  }

}

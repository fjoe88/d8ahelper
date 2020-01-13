

# Transfer ------------------------------------------------------------------------------------

copy_unique <-
  function(df = paste(readClipboard(), collapse = ","),
           col = "lotid",
           sep = ",",
           quotes = FALSE,
           format_lotid = TRUE,
           format_step = FALSE,
           save_csv = FALSE,
           ...) {
    #'copy unique values to clipboard.
    #'
    #'input: chr string copied from clipboard, or a dataframe (column).
    #'
    #'output:
    #'(by default) comma separated string w/ duplicates removed.
    #'a dataframe of unique values in each row.
    #'
    #'note:
    #'1) if input is a dataframe, do not return back a df.
    #'2) by default will assume lotids being copied and format them unless set format_lotid = FALSE
    #'3) if format_step == TRUE, return a comma separated step name string instead
    #'4) if save_csv == TRUE, will save output to '1.lot_list.csv'

    if (is.data.frame(df)) {
      if (tolower(col) == "lotid") {
        df <- d8ahelper::format_lotid(df)
        df <- d8ahelper::create_startlot(df)
      }
      unique.val <- unique(df[[col]])
      str.to.copy <- paste(unique.val, collapse = sep)
      utils::writeClipboard(str.to.copy)
      return(df)

    } else{
      if (!is.character(df))
        stop("only data.frame objects or character vectors are accepted")

      if (length(df) > 1) {
        df = paste(df, collapse = ",")
      }
      # early return
      if (format_step == TRUE){
        dt <- data.table::setDT(stringr::str_split(df, ","))
        names(dt) <- "step_name"
        dt <- unique(dt)
        utils::writeClipboard(paste(dt[["step_name"]], collapse = ","))
        return(dt)
      }

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
        dt <- d8ahelper::format_lotid(dt)
        dt <- d8ahelper::create_startlot(dt)
      }

      if (quotes == TRUE) {
        dt[[col]] <- paste0("'", dt[[col]], "'")
      }

      data.table::setkeyv(dt, col)
      dt <- unique(dt)

      utils::writeClipboard(paste(dt[[col]], collapse = sep))

      if (save_csv == TRUE) {
        d8ahelper::save_csv(dt, file.name = "1.lot_list.csv")
      }

      return(dt)
    }
  }

copy_as_sql_like <-
  function(x = paste(readClipboard(), collapse = ","),
           is_lotid = TRUE,
           col = "lot_id",
           add_and_prior = FALSE,
           add_and_post = FALSE) {
    #' provide quick sql-friendly format string to fuzzy match a list of (lotids)

    if (is_lotid == TRUE) {
      df <- d8ahelper::copy_unique(x)

      lotid.sql.like <- paste0(glue::glue("{col} like '"),
                               paste(df$StartLot,
                                     collapse = glue::glue("%'  or {col} like '")),
                               "%'")
    } else {
      df <- d8ahelper::copy_unique(x,
                                   format_lotid = FALSE)

      lotid.sql.like <- paste0(glue::glue("{col} like '"),
                               paste(df$lotid,
                                     collapse = glue::glue("%'  or {col} like '")),
                               "%'")
    }

    if (add_and_prior == TRUE) {
      lotid.sql.like <- paste("AND ", lotid.sql.like)
    }

    if (add_and_post == TRUE) {
      lotid.sql.like <- paste(lotid.sql.like, " AND ")
    }

    return(lotid.sql.like)
  }

save_csv <- function(df,
                     time_as_chr = FALSE,
                     path = here::here("R_output"),
                     file.name = "R_output.csv",
                     folder = "") {
  #' faster way of saving csv to a target folder
  #' utilizes 'here' package to save output to 'R_output' folder under root directory by default

  if (time_as_chr == TRUE) {
    df <- d8ahelper::convert_time_to_chr(df)
  }

  dir.create(file.path(path,
                       folder),
             showWarnings = FALSE)

  data.table::fwrite(df,
                     file = file.path(path,
                                      folder,
                                      file.name))
}


load_csv <- function(file.name = 'R_output.csv',
                     path = here::here("R_output"),
                     folder = "",
                     load = TRUE) {
  #' load csv from a target folder
  #' utilizes 'here' package to load files from to 'R_output' folder under root directory by default
  #'
  #' note:
  #' if load == FALSE, print csv file names to console only

  print(list.files(path, pattern = ".csv"))

  if (load == TRUE) {
    dir.create(file.path(path,
                         folder),
               showWarnings = FALSE)

    data.table::fread(file = file.path(path,
                                       folder,
                                       file.name))
  }
}


# data prep workflow ------------------------------------------------------

#' data prep actions once data is downloaded

require(d8ahelper)
require(wafer)
require(tidyverse)
require(data.table)

source(here::here("R", "function.R"))

project <- pick_project()

.output_folder = here::here("data", project, "working_dir", "feature_importance")

file.list <-
  d8ahelper::load_files(here::here("data", project, 'downloads'),
                        full.names = FALSE)

merged.file.list <-
  d8ahelper::load_files(here::here("data", project, 'working_dir/feature_importance'),
                        full.names = FALSE)
merged.file.list <- stringr::str_remove(merged.file.list, "^lb_")

#if any file already exist in target folder, ask if to skip them
skip_files <- ifelse(
  any(file.list %in% merged.file.list),
  select.list(
    c(TRUE, FALSE),
    preselect = TRUE,
    multiple = FALSE,
    title = "skip previously merged files?"
  ),
  FALSE
)

if (skip_files == TRUE) {

  files_to_avoid <- file.list[file.list %in% merged.file.list]
  files_to_avoid <- stringr::str_remove(files_to_avoid, ".csv$")
  to_avoid <- paste(files_to_avoid, collapse = "|")

  load_files_clean_wrapper(project = project,
                           replace = TRUE,
                           avoid = to_avoid)
} else {
  load_files_clean_wrapper(project = project, replace = TRUE)
}


if (length(grep('split_lot', file.list)) > 0) {
  message('need to source lotid_motherchild R file')
}

if (skip_files == TRUE) {

  #include "trackout" file for merging purpose
  merged.file.list <- merged.file.list[!grepl("trackout_w", merged.file.list)]

  files_to_avoid <- file.list[file.list %in% merged.file.list]
  files_to_avoid <- stringr::str_remove(files_to_avoid, ".csv$")
  to_avoid <- paste(files_to_avoid, collapse = "|")

  join_label(
    project = project,
    label_file_name = 'gb.csv',
    output_folder = .output_folder,
    summarize = FALSE,
    #critical
    join_dt = TRUE,
    .re_for_dt = "trackout",
    avoid = to_avoid
  )
} else{
  join_label(
    project = project,
    label_file_name = 'gb.csv',
    output_folder = .output_folder,
    summarize = FALSE,
    #critical
    join_dt = TRUE,
    .re_for_dt = "trackout"
  )
}


bat_open_csv_in_jmp_wrapper(dir_full_path = .output_folder)



# feature selection: good/bad ---------------------------------------------

require(d8ahelper)
require(wafer)
require(tidyverse)
require(data.table)
require(broom)
require(rmarkdown)
require(tictoc)
library(progress)

source(here::here("R", "function.R"))
tictoc::tic("overall")

projs <- list.dirs(path=here::here("data"), recursive = FALSE, full.names = FALSE)

project <- select.list(projs,
                       preselect = NULL,
                       multiple = FALSE,
                       title = "Select a Project:")

label = readline("Label column name:")

# source folder where raw data are and joined with response
data_folder = c("working_dir/feature_importance")

#output a list containing numerical and categorical results for each file
#use avoid = '_p$|_d$' to filter point/die level files
file.list <-
  d8ahelper::load_files(here::here("data", project, data_folder))

# model

# define datetime here to maintain consistent datetime label per run
datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M")

pb <- progress::progress_bar$new(total = length(file.list))

result.gb <-
  d8ahelper::lapply_preserve_names(file.list, function(file) {
    message(names(file))

    # df <- readr::read_csv(file, col_types = cols())
    df <- data.table::fread(file)
    df <- tibble::as_tibble(df)

    #returns list of num and cat results
    p <- fs_gb_wrapper(df = df, .label = label)

    #add 'file name' column in result
    p <- lapply(p, function(x) {
      if (is.data.frame(x)) {
        x$file <- names(file)
        x <- d8ahelper::move_left(df = x, "file")
      }
      return(x)
    })

    pb$tick()

    return(p)
  })

names(result.gb) <- names(file.list)

# output

#filter and output result using summary table metrics
save_anova_result <- function(do_which = "numeric", # TODO: figure out a proper TH
                              pval = 0.2,
                              min_row_count = 5, # beaware of this treshold
                              save_data = FALSE,
                              save_joined_data = FALSE,
                              joint_by = c("startlot", "waferid"),
                              generate_plot = FALSE,
                              label = label) {

  # datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M")
  # output_folder = paste0(label, "_", datetime)
  output_folder = 'results'

  if (length(result.gb) <= 1) {
    dt <- result.gb[[1]][[do_which]]
  } else {
    dt <- switch(do_which,
                 "numeric" = combine_num(result.gb),
                 "categorical" = combine_cat(result.gb)
    )
  }

  if (is.null(dt) || is.na(dt)) return(NA)

  dt <- dt[!(is.na(anova.p)), ]

  #filter results by p.val and non missing row count
  #min row count threshold = 5 or 1% of rows, for nrow <100 essentially return all results
  dt <- dt[anova.p < pval &
             pt > min(min_row_count, max(dt$pt) * 0.01) ]

  if (nrow(dt) == 0) return(NA)

  # special filters
  dt <- dt[sapply(tolower(dt$name), function(x) !grepl("::lotid::", x)), ]
  dt <- dt[sapply(tolower(dt$name), function(x) !grepl("startlot", x)), ]

  dt <- add_tier(dt, 'file')

  .base <- nrow(dt)

  #rank the result
  rank.pval <- data.table::frank(dt[['anova.p']], ties.method = 'min')
  dt$rank.p <- d8ahelper::format_num(rank.pval, digits = 0)

  rank.miss <- as.numeric(stringr::str_remove(dt[['miss']], "%$"))/100 * .base
  dt$rank.miss <- d8ahelper::format_num(rank.miss, digits = 0)

  #use the minimum between good/bad for balance ratio
  #more balanced(1) the better
  .min.gb <- pmin(dt[['lot.g']], dt[['lot.b']])
  .min.gb.max <- max(.min.gb)
  rank.bal <- (1 - .min.gb/.min.gb.max) * .base #use max pair-min as base
  dt$rank.bal <- d8ahelper::format_num(rank.bal, digits = 0)

  if (do_which == 'categorical') {
    .worst.uniq <- 10 #use 10, or can be max(dt[['uniq']])
    rank.uniq <- dt[['uniq']]/.worst.uniq * .base
    dt$rank.uniq <- d8ahelper::format_num(rank.uniq, digits = 0)
  }

  if (do_which == 'numeric') {
    how.many <- stringr::str_count(dt[['comp']], "<|>")
    rank.delta <- (1 - how.many / 3) * .base # one '<' count towards 1/3 in wt of base
    dt$rank.delt <- d8ahelper::format_num(rank.delta, digits = 0)
  }

  .wt <- c(
    'pval'   = 3,   #most important
    'miss'   = 0.5, #missings becomes less important as long as balance is good (min btw g/b)
    'uniq'   = 1,
    'delta'  = 0.2,
    'bal'    = 1
  )

  dt$rank.wt <-
    rank.pval   * .wt['pval'] +
    rank.miss   * .wt['miss'] +
    rank.bal    * .wt['bal'] +
    switch(do_which,
           'categorical' = rank.uniq * .wt['uniq'],
           'numeric' = rank.delta * .wt['delta'])

  dt$rank.wt <- d8ahelper::format_num(dt$rank.wt / min(dt$rank.wt))

  data.table::setorder(dt, rank.wt)

  dt$rn   <- NULL

  d8ahelper::save_csv(dt,
                      file.name = glue::glue("{label}_{do_which}_{datetime}.csv"),
                      path = here::here("data", project, data_folder),
                      folder = output_folder,
                      overwrite = TRUE)

  if (save_data == TRUE) {

    dt$dt.y3 <-
      paste0(dt$step, "::WaferData::RunCompleteDateTime")

    dt$dt.f2dd <-
      paste0("LOTDATA::trackedout::", dt$step)

    files.to.load <- paste(unique(dt$file), collapse = "|")

    loc <- here::here("data",
                      project,
                      data_folder)

    files <-
      d8ahelper::load_files(loc, load = TRUE, pattern = files.to.load, read_as_df = TRUE)


    files.skinned <- lapply_preserve_names(files, function(file) {
      file.name <- names(file)
      df.list <- d8ahelper::encode_col(file[[1]])

      col.id <-
        d8ahelper::get_id(df.list, detect_all(file[[1]], return_col_names = TRUE))
      col.label <- d8ahelper::get_id(df.list, label)

      #preserve original columns that came within response file
      col.orig <- d8ahelper::get_id(df.list, grep("_orig$", df.list$tag, value = TRUE))
      col.feat <- d8ahelper::get_id(df.list, dt$name[dt$file == file.name])
      col.dt.y3 <- d8ahelper::get_id(df.list, dt$dt.y3[dt$file == file.name])
      col.dt.f2dd <- d8ahelper::get_id(df.list, dt$dt.f2dd[dt$file == file.name])

      col.all <- unique(c(col.id, col.label, col.orig, col.feat, col.dt.y3, col.dt.f2dd))
      indx <- which(names(df.list$df) %in% col.all)

      df <- d8ahelper::decode_col(df.list)
      df <- df[, indx]

      d8ahelper::save_csv(
        df,
        file.name = glue("{do_which}_{file.name}.csv"),
        path = here::here("data", project, "downloads", "joined_w_response"),
        folder = output_folder,
        overwrite = TRUE,
        as_chr = TRUE)

      return(df)
    })
  }

  if (save_joined_data == TRUE){
    # be aware of which column used for joining
    files.skinned.combined <- d8ahelper::multi_join(files.skinned, by = join_by)
    d8ahelper::save_csv(files.skinned.combined,
                        file.name = "raw_combined.csv",
                        path = here::here("data", project, data_folder),
                        folder = output_folder,
                        as_chr = TRUE,
                        overwrite = TRUE)
  }


  return(dt)

}

result <- lapply(c("numeric",
                   "categorical"), function(x) {
                     tictoc::tic(x)

                     result <- save_anova_result(do_which = x,
                                                 generate_plot = FALSE,
                                                 label = label)

                     tictoc::toc()
                     return(result)
                   })

tictoc::toc()

# generate plot through rmd

# plot_gb <- function(result.type = 'gb_numeric',
#                     howmany = 50,
#                     rmd = here::here("R", "notebooks", "plot_fs.Rmd"),
#                     output_dir = here::here('r_output'),
#                     ...) {
#
#   tictoc::tic(glue::glue('render plot - {result.type}_{howmany}'))
#
#   rmarkdown::render(
#     input = rmd,
#     output_dir = output_dir,
#     output_file = glue::glue("{result.type}_plot_{datetime}.html")
#   )
#
#   tictoc::toc()
# }
#
# plot_gb(howmany = 50,
#         result.type = 'gb_numeric',
#         output_dir = here::here("data", project, data_folder,glue::glue("results")))
#
# plot_gb(howmany = 20,
#         result.type = 'gb_categorical',
#         output_dir = here::here("data", project, data_folder,glue::glue("results")))



# feature selection: numerical --------------------------------------------

#' project: use linear regression (and other models) to find a list of numerical ind.var that captures variation in the dep.var
#'
#' remove rows where metric is empty
#' separate numeric columns
#' find correlations for each ind.vars
#' output the result with necessary statistics
#' output data with datetime columns included
#'

require(d8ahelper)
require(wafer)
require(tidyverse)
require(data.table)
require(broom)
require(glmnet)
require(caret)

source(here::here("R", "function.R"))
tictoc::tic("overall")

project <- pick_project()

label = readline("Label column name:")

#setup filter
filter_using_col <- select.list(
  c(TRUE, FALSE),
  preselect = FALSE,
  multiple = FALSE,
  title = "Use column filters?"
)

if (filter_using_col == TRUE){
  filter_col <- readline("Column name to be used for filter:")
  filter_criteria <- readline("Filter rows equal to:")
}

# source folder where raw data are and joined with response
data_folder = c("working_dir/feature_importance")

file.list <-
  d8ahelper::load_files(here::here("data", project, data_folder))

# model
pb <- progress::progress_bar$new(total = length(file.list))

result <-
  d8ahelper::lapply_preserve_names(file.list, function(file) {
    message(names(file))

    # df <- readr::read_csv(file, col_types = cols())
    df <- data.table::fread(file)

    if (filter_using_col == TRUE) {
      which_row <- df[[filter_col]]==filter_criteria
      df <- df[which_row, ]
    }

    df <- tibble::as_tibble(df)

    #returns list of num and cat results
    p <- fs_num_wrapper(df = df,
                        .label = label,
                        .rm_outlier = TRUE,
                        .fct_lump_prop = 1 / 50,
                        .levels_at_most = 15,
                        fit_poly = FALSE,
                        cross_v = TRUE)

    p <- lapply(p, function(x) {
      if (is.data.frame(x)) {
        x$file <- names(file)
        x <- d8ahelper::move_left(df = x, "file")
      }
      return(x)
    })

    pb$tick()

    return(p)
  })

names(result) <- names(file.list)

# result ranking

dt.num <- combine_num(result)
dt.cat <- combine_cat(result)

# dt.num <- add_tier(dt.num, 'file')
# dt.cat <- add_tier(dt.cat, 'file')

#rank results
# data.table::setorderv(dt.num, "rsq.adj", order = -1)
# data.table::setorderv(dt.cat, "p.val", order = 1)


#cat
.base.cat <- nrow(dt.cat)

rank.p <- data.table::frank(dt.cat[['p.val']], ties.method = 'min')
dt.cat$rank.p <- d8ahelper::format_num(rank.p, digits = 0)

rank.p.cv <- data.table::frank(dt.cat[['p.cv']], ties.method = 'min')
dt.cat$rank.p.cv <- d8ahelper::format_num(rank.p.cv, digits = 0)

rank.miss <- as.numeric(stringr::str_remove(dt.cat[['miss']], "%$"))/100 * .base.cat
dt.cat$rank.miss <- d8ahelper::format_num(rank.miss, digits = 0)

rank.uniq <- dt.cat[['uniq']]/10 * .base.cat #use 10 as a bar
dt.cat$rank.uniq <- d8ahelper::format_num(rank.uniq, digits = 0)

.wt.cat <- c(
  'p'      = 2,   #most important
  'p.cv'   = 1,
  'miss'   = 0.5,
  'uniq'   = 0.5
)

dt.cat$rank.wt.cat <-
  rank.p       * .wt.cat['p'] +
  rank.p.cv    * .wt.cat['p.cv'] +
  rank.miss    * .wt.cat['miss'] +
  rank.uniq    * .wt.cat['uniq']

dt.cat$rank.wt <- d8ahelper::format_num(dt.cat$rank.wt / min(dt.cat$rank.wt))

data.table::setorder(dt.cat, rank.wt)

#num
.base.num <- nrow(dt.num)

rank.p <- case_when(dt.num[['p.val']] < 0.05 ~ dt.num[['p.val']]/0.05,
                    dt.num[['p.val']] < 0.2  ~ dt.num[['p.val']]/0.2 + 1,
                    TRUE                     ~ dt.num[['p.val']]/0.5 + 2
) * .base.num
dt.num$rank.p <- d8ahelper::format_num(rank.p, digits = 1)

rank.rsq <- data.table::frankv(dt.num[['rsq.adj']], ties.method = 'min', order = -1)
dt.num$rank.rsq <- d8ahelper::format_num(rank.rsq, digits = 1)

rank.rmse <- data.table::frank(dt.num[['rmse']], ties.method = 'min')
dt.num$rank.rmse <- d8ahelper::format_num(rank.rmse, digits = 0)

#sd of the slope from repeats/folds, can be seen as a quality metric (trustworthiness)
rank.slsd <- data.table::frank(dt.num[['sl.sd']], ties.method = 'min')
dt.num$rank.slsd <- d8ahelper::format_num(rank.slsd, digits = 2)

#want to penalize against <50pt esp <5pt scenarios
rank.pt <- case_when(dt.num[['pt']] < 5   ~ 1,
                     dt.num[['pt']] < 50  ~ (50 - dt.num[['pt']])/50,
                     TRUE                 ~ 0) * .base.num
dt.num$rank.pt <- d8ahelper::format_num(rank.pt, digits = 0)

rank.miss <- as.numeric(stringr::str_remove(dt.num[['miss']], "%$"))/100 * .base.num
dt.num$rank.miss <- d8ahelper::format_num(rank.miss, digits = 0)

#possible special method to use (pt-uniq)/pt.max as a measure for if data is not numerical?

.wt.num <- c(
  'rsq'    = 10,   #most important
  'pt'     = 1,
  'p'      = 0.5,
  'rmse'   = 0.5,
  'slsd'   = 0.5,
  'miss'   = 0.5
)

dt.num$rank.wt <-
  rank.rsq     * .wt.num['rsq'] +
  rank.rmse    * .wt.num['rmse'] +
  rank.pt      * .wt.num['pt'] +
  rank.p       * .wt.num['p'] +
  rank.slsd    * .wt.num['slsd'] +
  rank.miss    * .wt.num['miss']

dt.num$rank.wt <- d8ahelper::format_num(dt.num$rank.wt / min(dt.num$rank.wt))

data.table::setorder(dt.num, rank.rsq) #eventually want to use rank.wt

dt.num$rsq.adj <- format_num(dt.num$rsq.adj, digit = 2)
dt.num$rmse    <- format_num(dt.num$rmse, digit = 2)
dt.num$mae     <- format_num(dt.num$mae, digit = 2)
dt.num$p.val   <- format_num(dt.num$p.val, digit = 2)
dt.num$slope   <- format_num(dt.num$slope, digit = 2)
dt.num$sl.sd   <- format_num(dt.num$sl.sd, digit = 2)

dt.cat$rn <- NULL

dt.num$rn <- NULL

# output

datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M")
# output_folder = paste0(label, "_", datetime)
output_folder = 'results'

#add identifier for filters
if (filter_using_col == TRUE){
  filter_col2 <- gsub("_orig$", "", filter_col)
  file_info <- glue::glue("{datetime}_{filter_col2}={filter_criteria}")
} else {
  file_info <- datetime
}


d8ahelper::save_csv(dt.num,
                    file.name = glue::glue("num_numeric_{file_info}.csv"),
                    path = here::here("data", project, data_folder),
                    folder = output_folder,
                    overwrite = TRUE)

d8ahelper::save_csv(dt.cat,
                    file.name = glue::glue("num_categorical_{file_info}.csv"),
                    path = here::here("data", project, data_folder),
                    folder = output_folder,
                    overwrite = TRUE)

tictoc::toc()


# feature selection: good/bad, decision trees -----------------------------

require(d8ahelper)
require(wafer)
require(tidyverse)
require(data.table)
require(broom)
require(rmarkdown)
require(tictoc)
require(progress)
require(tidymodels)
require(vip)


source(here::here("R", "function.R"))

project = pick_project()
label = "metric"

# source folder where raw data are and joined with response
data_folder = c("working_dir/feature_importance")

#output a list containing numerical and categorical results for each file
#use avoid = '_p$|_d$' to filter point/die level files
file.list <-
  d8ahelper::load_files(here::here("data", project, data_folder))

# model

pb <- progress::progress_bar$new(total = length(file.list))

result <-
  d8ahelper::lapply_preserve_names(file.list, function(file) {
    message(names(file))

    # df <- readr::read_csv(file, col_types = cols())
    df <- data.table::fread(file)
    df <- tibble::as_tibble(df)
    #run pre-proc and return combined result
    p <- pre_proc_wrapper(df = df, .label = label)

    if (is.na(p)) return(NA)

    df.enc <- d8ahelper::encode_col(df)

    col.label.enc <- d8ahelper::get_id(df.enc, label)

    #default to 3:1 split
    split <- initial_split(df.enc$df, strata = !!col.label.enc)

    df_train <- training(split)
    df_test  <- testing(split)

    seq.col <- wafer::detect_all(df, return_loc = TRUE)
    seq.keep <- grep("_orig$|_excl$|^metric|^gb", names(df))
    seq.label <- which(names(df) == label)
    seq.id <- setdiff(union(seq.col, seq.keep), seq.label)
    col.id.enc <- names(df.enc$df)[seq.id]

    formula <- as.formula(glue::glue("{col.label.enc} ~ ."))

    tune_spec <-
      decision_tree(
        cost_complexity = tune(),
        tree_depth = tune()
      ) %>%
      set_engine("rpart") %>%
      set_mode("classification")

    tree_grid <- grid_regular(cost_complexity(),
                              tree_depth(),
                              levels = 5)

    set.seed(234)
    df_folds <- vfold_cv(df_train, v = 5)

    gb_dt_rcp <-
      recipe(formula, data = df_train) %>%
      update_role(!!col.id.enc, new_role = "ID")

    tree_wf <- workflow() %>%
      add_model(tune_spec) %>%
      add_recipe(gb_dt_rcp)

    tree_res <-
      tree_wf %>%
      tune_grid(
        resamples = df_folds,
        grid = tree_grid
      )

    tree_res %>%
      collect_metrics()

    tree_res %>%
      collect_metrics() %>%
      mutate(tree_depth = factor(tree_depth)) %>%
      ggplot(aes(cost_complexity, mean, color = tree_depth)) +
      geom_line(size = 1.5, alpha = 0.6) +
      geom_point(size = 2) +
      facet_wrap(~ .metric, scales = "free", nrow = 2) +
      scale_x_log10() +
      scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

    gb_rf_fit <-
      gb_rf_wflow %>%
      fit(data = df_train)

    fit <- pull_workflow_fit(gb_rf_fit)

    best_tree <- tree_res %>%
      select_best("roc_auc")

    final_wf <-
      tree_wf %>%
      finalize_workflow(best_tree)

    final_wf

    final_tree <-
      final_wf %>%
      fit(data = df_train)

    p <- final_tree %>%
      pull_workflow_fit() %>%
      vip::vi() %>%
      mutate(Col.Name = d8ahelper::get_name(df.enc, Variable))

    p <- vip::vi(fit) %>%
      mutate(Col.Name = d8ahelper::get_name(df.enc, Variable))

    #test
    final_fit <-
      final_wf %>%
      last_fit(split)

    final_fit %>%
      collect_metrics()

    final_fit %>%
      collect_predictions() %>%
      roc_curve(!!col.label.enc, .pred_bad) %>%
      autoplot()

    args(decision_tree)

    #add 'file name' column in result
    if (is.data.frame(p)) {
      p$file <- names(file)
      p <- d8ahelper::move_left(df = p, "file")
    }

    pb$tick()

    return(p)
  })

names(result) <- names(file.list)

result[is.na(result)] <- NULL

# output
#filter and output result using summary table metrics
save_anova_result <- function(do_which = "numeric", # TODO: figure out a proper TH
                              pval = 0.2,
                              min_row_count = 5, # beaware of this treshold
                              save_data = FALSE,
                              save_joined_data = FALSE,
                              joint_by = c("startlot", "waferid"),
                              generate_plot = FALSE,
                              label = label) {

  datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M")
  # output_folder = paste0(label, "_", datetime)
  output_folder = 'results'

  if (length(result.gb) <= 1) {
    dt <- result.gb[[1]][[do_which]]
  } else {
    dt <- switch(do_which,
                 "numeric" = combine_num(result.gb),
                 "categorical" = combine_cat(result.gb)
    )
  }

  if (is.null(dt) || is.na(dt)) return(NA)

  dt <- dt[!(is.na(anova.p)), ]

  #filter results by p.val and non missing row count
  #min row count threshold = 5 or 1% of rows, for nrow <100 essentially return all results
  dt <- dt[anova.p < pval &
             pt > min(min_row_count, max(dt$pt) * 0.01) ]

  # special filters
  dt <- dt[sapply(tolower(dt$name), function(x) !grepl("::lotid::", x)), ]
  dt <- dt[sapply(tolower(dt$name), function(x) !grepl("startlot", x)), ]

  dt <- add_tier(dt, 'file')

  .base <- nrow(dt)

  #rank the result
  rank.pval <- data.table::frank(dt[['anova.p']], ties.method = 'min')
  dt$rank.p <- d8ahelper::format_num(rank.pval, digits = 0)

  rank.miss <- as.numeric(stringr::str_remove(dt[['miss']], "%$"))/100 * .base
  dt$rank.miss <- d8ahelper::format_num(rank.miss, digits = 0)

  #use the minimum between good/bad for balance ratio
  #more balanced(1) the better
  .min.gb <- pmin(dt[['lot.g']], dt[['lot.b']])
  .min.gb.max <- max(.min.gb)
  rank.bal <- (1 - .min.gb/.min.gb.max) * .base #use max pair-min as base
  dt$rank.bal <- d8ahelper::format_num(rank.bal, digits = 0)

  if (do_which == 'categorical') {
    .worst.uniq <- 10 #use 10, or can be max(dt[['uniq']])
    rank.uniq <- dt[['uniq']]/.worst.uniq * .base
    dt$rank.uniq <- d8ahelper::format_num(rank.uniq, digits = 0)
  }

  if (do_which == 'numeric') {
    how.many <- stringr::str_count(dt[['comp']], "<|>")
    rank.delta <- (how.many / 3) * .base # one '<' count towards 1/3 in wt of base
    dt$rank.delt <- d8ahelper::format_num(rank.delta, digits = 0)
  }

  .wt <- c(
    'pval'   = 3,   #most important
    'miss'   = 0.5, #missings becomes less important as long as balance is good (min btw g/b)
    'uniq'   = 1,
    'delta'  = 0.5,
    'bal'    = 1
  )

  dt$rank.wt <-
    rank.pval   * .wt['pval'] +
    rank.miss   * .wt['miss'] +
    rank.bal    * .wt['bal'] +
    switch(do_which,
           'categorical' = rank.uniq * .wt['uniq'],
           'numeric' = rank.delta * .wt['delta'])

  dt$rank.wt <- d8ahelper::format_num(dt$rank.wt / min(dt$rank.wt))

  data.table::setorder(dt, rank.wt)

  dt$rn   <- NULL

  d8ahelper::save_csv(dt,
                      file.name = glue::glue("{label}_{do_which}_{datetime}.csv"),
                      path = here::here("data", project, data_folder),
                      folder = output_folder,
                      overwrite = TRUE)

  if (save_data == TRUE) {

    dt$dt.y3 <-
      paste0(dt$step, "::WaferData::RunCompleteDateTime")

    dt$dt.f2dd <-
      paste0("LOTDATA::trackedout::", dt$step)

    files.to.load <- paste(unique(dt$file), collapse = "|")

    loc <- here::here("data",
                      project,
                      data_folder)

    files <-
      d8ahelper::load_files(loc, load = TRUE, pattern = files.to.load, read_as_df = TRUE)


    files.skinned <- lapply_preserve_names(files, function(file) {
      file.name <- names(file)
      df.list <- d8ahelper::encode_col(file[[1]])

      col.id <-
        d8ahelper::get_id(df.list, detect_all(file[[1]], return_col_names = TRUE))
      col.label <- d8ahelper::get_id(df.list, label)

      #preserve original columns that came within response file
      col.orig <- d8ahelper::get_id(df.list, grep("_orig$", df.list$tag, value = TRUE))
      col.feat <- d8ahelper::get_id(df.list, dt$name[dt$file == file.name])
      col.dt.y3 <- d8ahelper::get_id(df.list, dt$dt.y3[dt$file == file.name])
      col.dt.f2dd <- d8ahelper::get_id(df.list, dt$dt.f2dd[dt$file == file.name])

      col.all <- unique(c(col.id, col.label, col.orig, col.feat, col.dt.y3, col.dt.f2dd))
      indx <- which(names(df.list$df) %in% col.all)

      df <- d8ahelper::decode_col(df.list)
      df <- df[, indx]

      d8ahelper::save_csv(
        df,
        file.name = glue("{do_which}_{file.name}.csv"),
        path = here::here("data", project, "downloads", "joined_w_response"),
        folder = output_folder,
        overwrite = TRUE,
        as_chr = TRUE)

      return(df)
    })
  }

  if (save_joined_data == TRUE){
    # be aware of which column used for joining
    files.skinned.combined <- d8ahelper::multi_join(files.skinned, by = join_by)
    d8ahelper::save_csv(files.skinned.combined,
                        file.name = "raw_combined.csv",
                        path = here::here("data", project, data_folder),
                        folder = output_folder,
                        as_chr = TRUE,
                        overwrite = TRUE)
  }


  return(dt)

}

result <- lapply(c("numeric",
                   "categorical"), function(x) {
                     tictoc::tic(x)

                     result <- save_anova_result(do_which = x,
                                                 generate_plot = FALSE,
                                                 label = label)

                     tictoc::toc()
                     return(result)
                   })

tictoc::toc()

# generate plot through rmd
plot_gb <- function(result.type = 'gb_categorical',
                    howmany = 50,
                    rmd = here::here("R", "notebooks", "plot_fs.Rmd"),
                    output_dir = here::here('r_output'),
                    ...) {

  tictoc::tic(glue::glue('render plot - {result.type}_{howmany}'))

  rmarkdown::render(
    input = rmd,
    output_dir = output_dir,
    output_file = glue::glue("{result.type}_plot.html")
  )

  tictoc::toc()
}

plot_gb(howmany = 50,
        result.type = 'gb_numeric',
        output_dir = here::here("data", project, data_folder,"results"))

# plot_gb(result.type = 'gb_categorical')


# feature selection: random forest ----------------------------------------

require(d8ahelper)
require(wafer)
require(tidyverse)
require(data.table)
require(broom)
require(rmarkdown)
require(tictoc)
require(progress)
require(tidymodels)
require(vip)


source(here::here("R", "function.R"))

project = pick_project()
label = "gb"

# source folder where raw data are and joined with response
data_folder = c("working_dir/feature_importance")

#output a list containing numerical and categorical results for each file
#use avoid = '_p$|_d$' to filter point/die level files
file.list <-
  d8ahelper::load_files(here::here("data", project, data_folder))

# model
pb <- progress::progress_bar$new(total = length(file.list))

result <-
  d8ahelper::lapply_preserve_names(file.list, function(file) {
    message(names(file))

    # df <- readr::read_csv(file, col_types = cols())
    df <- data.table::fread(file)
    df <- tibble::as_tibble(df)
    #run pre-proc and return combined result
    p <- pre_proc_wrapper(df = df, .label = label)

    if (is.na(p)) return(NA)

    df.enc <- d8ahelper::encode_col(df)

    col.label.enc <- d8ahelper::get_id(df.enc, label)

    # default to 3:1 split
    # split <- initial_split(df.enc$df, strata = !!col.label.enc)
    # df_train <- training(split)
    # df_test  <- testing(split)

    df_train <- df.enc$df

    seq.col <- wafer::detect_all(df, return_loc = TRUE)
    seq.keep <- grep("_orig$|_excl$|^metric|^gb", names(df))
    seq.label <- which(names(df) == label)
    seq.id <- setdiff(union(seq.col, seq.keep), seq.label)
    col.id.enc <- names(df.enc$df)[seq.id]

    formula <- as.formula(glue::glue("{col.label.enc} ~ ."))

    gb_rf_rcp <-
      recipe(formula, data = df_train) %>%
      update_role(!!col.id.enc, new_role = "ID") %>%
      step_medianimpute(all_numeric(), -all_outcomes())

    rf_mod <-
      rand_forest(trees = 100) %>%
      set_engine("ranger", importance = 'impurity') %>%
      set_mode("classification")

    gb_rf_wflow <-
      workflow() %>%
      add_model(rf_mod) %>%
      add_recipe(gb_rf_rcp)

    gb_rf_fit <-
      gb_rf_wflow %>%
      fit(data = df_train)

    fit <- pull_workflow_fit(gb_rf_fit)

    p <- vip::vi(fit) %>%
      mutate(Col.Name = d8ahelper::get_name(df.enc, Variable))

    #add 'file name' column in result
    if (is.data.frame(p)) {
      p$file <- names(file)
      p <- d8ahelper::move_left(df = p, "file")
    }

    pb$tick()

    return(p)
  })

names(result) <- names(file.list)

result[is.na(result)] <- NULL

# output
#filter and output result using summary table metrics
save_anova_result <- function(do_which = "numeric", # TODO: figure out a proper TH
                              pval = 0.2,
                              min_row_count = 5, # beaware of this treshold
                              save_data = FALSE,
                              save_joined_data = FALSE,
                              joint_by = c("startlot", "waferid"),
                              generate_plot = FALSE,
                              label = label) {

  datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M")
  # output_folder = paste0(label, "_", datetime)
  output_folder = 'results'

  if (length(result.gb) <= 1) {
    dt <- result.gb[[1]][[do_which]]
  } else {
    dt <- switch(do_which,
                 "numeric" = combine_num(result.gb),
                 "categorical" = combine_cat(result.gb)
    )
  }

  if (is.null(dt) || is.na(dt)) return(NA)

  dt <- dt[!(is.na(anova.p)), ]

  #filter results by p.val and non missing row count
  #min row count threshold = 5 or 1% of rows, for nrow <100 essentially return all results
  dt <- dt[anova.p < pval &
             pt > min(min_row_count, max(dt$pt) * 0.01) ]

  # special filters
  dt <- dt[sapply(tolower(dt$name), function(x) !grepl("::lotid::", x)), ]
  dt <- dt[sapply(tolower(dt$name), function(x) !grepl("startlot", x)), ]

  dt <- add_tier(dt, 'file')

  .base <- nrow(dt)

  #rank the result
  rank.pval <- data.table::frank(dt[['anova.p']], ties.method = 'min')
  dt$rank.p <- d8ahelper::format_num(rank.pval, digits = 0)

  rank.miss <- as.numeric(stringr::str_remove(dt[['miss']], "%$"))/100 * .base
  dt$rank.miss <- d8ahelper::format_num(rank.miss, digits = 0)

  #use the minimum between good/bad for balance ratio
  #more balanced(1) the better
  .min.gb <- pmin(dt[['lot.g']], dt[['lot.b']])
  .min.gb.max <- max(.min.gb)
  rank.bal <- (1 - .min.gb/.min.gb.max) * .base #use max pair-min as base
  dt$rank.bal <- d8ahelper::format_num(rank.bal, digits = 0)

  if (do_which == 'categorical') {
    .worst.uniq <- 10 #use 10, or can be max(dt[['uniq']])
    rank.uniq <- dt[['uniq']]/.worst.uniq * .base
    dt$rank.uniq <- d8ahelper::format_num(rank.uniq, digits = 0)
  }

  if (do_which == 'numeric') {
    how.many <- stringr::str_count(dt[['comp']], "<|>")
    rank.delta <- (how.many / 3) * .base # one '<' count towards 1/3 in wt of base
    dt$rank.delt <- d8ahelper::format_num(rank.delta, digits = 0)
  }

  .wt <- c(
    'pval'   = 3,   #most important
    'miss'   = 0.5, #missings becomes less important as long as balance is good (min btw g/b)
    'uniq'   = 1,
    'delta'  = 0.5,
    'bal'    = 1
  )

  dt$rank.wt <-
    rank.pval   * .wt['pval'] +
    rank.miss   * .wt['miss'] +
    rank.bal    * .wt['bal'] +
    switch(do_which,
           'categorical' = rank.uniq * .wt['uniq'],
           'numeric' = rank.delta * .wt['delta'])

  dt$rank.wt <- d8ahelper::format_num(dt$rank.wt / min(dt$rank.wt))

  data.table::setorder(dt, rank.wt)

  dt$rn   <- NULL

  d8ahelper::save_csv(dt,
                      file.name = glue::glue("{label}_{do_which}_{datetime}.csv"),
                      path = here::here("data", project, data_folder),
                      folder = output_folder,
                      overwrite = TRUE)

  if (save_data == TRUE) {

    dt$dt.y3 <-
      paste0(dt$step, "::WaferData::RunCompleteDateTime")

    dt$dt.f2dd <-
      paste0("LOTDATA::trackedout::", dt$step)

    files.to.load <- paste(unique(dt$file), collapse = "|")

    loc <- here::here("data",
                      project,
                      data_folder)

    files <-
      d8ahelper::load_files(loc, load = TRUE, pattern = files.to.load, read_as_df = TRUE)


    files.skinned <- lapply_preserve_names(files, function(file) {
      file.name <- names(file)
      df.list <- d8ahelper::encode_col(file[[1]])

      col.id <-
        d8ahelper::get_id(df.list, detect_all(file[[1]], return_col_names = TRUE))
      col.label <- d8ahelper::get_id(df.list, label)

      #preserve original columns that came within response file
      col.orig <- d8ahelper::get_id(df.list, grep("_orig$", df.list$tag, value = TRUE))
      col.feat <- d8ahelper::get_id(df.list, dt$name[dt$file == file.name])
      col.dt.y3 <- d8ahelper::get_id(df.list, dt$dt.y3[dt$file == file.name])
      col.dt.f2dd <- d8ahelper::get_id(df.list, dt$dt.f2dd[dt$file == file.name])

      col.all <- unique(c(col.id, col.label, col.orig, col.feat, col.dt.y3, col.dt.f2dd))
      indx <- which(names(df.list$df) %in% col.all)

      df <- d8ahelper::decode_col(df.list)
      df <- df[, indx]

      d8ahelper::save_csv(
        df,
        file.name = glue("{do_which}_{file.name}.csv"),
        path = here::here("data", project, "downloads", "joined_w_response"),
        folder = output_folder,
        overwrite = TRUE,
        as_chr = TRUE)

      return(df)
    })
  }

  if (save_joined_data == TRUE){
    # be aware of which column used for joining
    files.skinned.combined <- d8ahelper::multi_join(files.skinned, by = join_by)
    d8ahelper::save_csv(files.skinned.combined,
                        file.name = "raw_combined.csv",
                        path = here::here("data", project, data_folder),
                        folder = output_folder,
                        as_chr = TRUE,
                        overwrite = TRUE)
  }


  return(dt)

}

result <- lapply(c("numeric",
                   "categorical"), function(x) {
                     tictoc::tic(x)

                     result <- save_anova_result(do_which = x,
                                                 generate_plot = FALSE,
                                                 label = label)

                     tictoc::toc()
                     return(result)
                   })

tictoc::toc()

# generate plot through rmd
plot_gb <- function(result.type = 'gb_categorical',
                    howmany = 50,
                    rmd = here::here("R", "notebooks", "plot_fs.Rmd"),
                    output_dir = here::here('r_output'),
                    ...) {

  tictoc::tic(glue::glue('render plot - {result.type}_{howmany}'))

  rmarkdown::render(
    input = rmd,
    output_dir = output_dir,
    output_file = glue::glue("{result.type}_plot.html")
  )

  tictoc::toc()
}

plot_gb(howmany = 50,
        result.type = 'gb_numeric',
        output_dir = here::here("data", project, data_folder,"results"))

# plot_gb(result.type = 'gb_categorical')


# workflow ----------------------------------------------------------------

#calling R scripts that are part of my data-mining workflow

source(here::here("R/function.R"))

init_proj()

save_lot_list_to_proj()

add_step_to_proj(pull_latest = F)

add_label_col_to_proj()

source(here::here("R/utility/dataExtractionWorkflow.R"))

create_bat_file_f2dd(file = "tw_nearest", pick_step = TRUE)
create_bat_file_f2dd(file = "fd_uva_w", pick_step = TRUE)
create_bat_file_f2dd(file = "eet_w", pick_step = TRUE)
create_bat_file_f2dd(file = "chamber_fd_w", pick_step = TRUE)
create_bat_file_f2dd(file = "transition_w", pick_step = TRUE)

# custom data transformations
add_time_deltas_to_proj(lag=1:6, file_re='trackout', th=0.8)

#impute missing wafer level values with lot level mean
impute_missing_num_to_proj(file_re='sem|thk|scatter|reg')

lotid_to_mc_by_proj(file_re='split_lot')

source(here::here("R/utility/dataPrepWorkflow.R"))

source(here::here("R/utility/fs_gb.R"))

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

source(here::here("R/utility/fs_num.R"))


# function ----------------------------------------------------------------


# 1.data quality -------------------------------------------------------------------------------

sum_df_wf <- function(df) {
  #' create a summary txt file for each data file, including column type, missing%, lot/wafer coverage%, etc

  df <- wafer::format_all(df)

  sum <- tibble(
    "row" = seq_len(ncol(df)),
    "col.name" = d8ahelper::headtail(names(df), 50),
    "class" = sapply(df, typeof),
    "l.c" = sapply(df, function(x) {
      if (any(grepl("^startlot", tolower(names(df))))){
        lot <- names(df)[grepl("^startlot", tolower(names(df)))]
        lcp <- length(unique(df[[lot]][!is.na(x)]))/length(unique(df[[lot]]))
        d8ahelper::format_to_percentage(lcp, digits = 0)
      } else {NA}
    }),
    "w.c" = sapply(df, function(x) {
      if (any(grepl("^waferid", tolower(names(df))))){
        wf <- names(df)[grepl("^waferid$", tolower(names(df)))]
        wcp <- length(unique(df[[wf]][!is.na(x)]))/length(unique(df[[wf]]))
        d8ahelper::format_to_percentage(wcp, digits = 0)
      } else {NA}
    }),
    "n.m.r" = sapply(df, function(x) {
      format(length(x) - sum(is.na(x)), nsmall = 1)
    }),
    "n.m" = sapply(df, function(x) {
      percent <- ((length(x) - sum(is.na(x))) / length(x))
      d8ahelper::format_to_percentage(percent, digits = 0)
    }),
    "uniq" = sapply(df, function(x) {
      format(length(unique(x)), nsmall = 1)
    }),
    "mean" = sapply(df, function(x) {
      if (!is.numeric(x)) {
        NA
      } else{
        format(round(mean(x, na.rm = TRUE), 1), nsmall = 1)
      }
    }),
    "std" = sapply(df, function(x) {
      if (!is.numeric(x)) {
        NA
      } else{
        format(round(sd(x, na.rm = TRUE), 1), nsmall = 1)
      }
    }),
    # "s.m.r" = sapply(df, function(x) {
    #   if (!is.numeric(x)) {
    #     NA
    #   } else{
    #     d8ahelper::format_to_percentage(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE), digits = 0)
    #   }
    # }),
  )

  names(sum) <- toupper(names(sum))

  return(sum)

}

add_summary <- function(project){
  #' wrapper function to sum_df_wf: summarize each file and save as txt

  loc <- here::here("data", project, "downloads")

  files <- list.files(loc, full.names = TRUE, pattern = ".csv")
  names(files) <- str_extract(list.files(loc, full.names = FALSE, , pattern = ".csv"), "^[^\\.]*")
  files <- files[!grepl("^_|archive", names(files))]

  lapply_preserve_names(files, function(file) {
    print(names(file))

    df <- readr::read_csv(file, na = c("", "NA", " "), col_types = cols())

    if (nrow(df) > 1) {
      sum <- sum_df_wf(df)
      d8ahelper::write_fwf(sum,
                           file = here::here("data",
                                             project = project,
                                             "downloads",
                                             glue::glue("{names(file)}_summary.txt")),
                           width = c(5, max(nchar(sum[[2]])) + 5, 12, rep(8, ncol(sum) - 3)))
      return(sum)
    }

    ## cut super wide data frames to 1000 rows or less chunks
    # } else if (nrow(df) > 10 & ncol(df) > 1000) {
    #
    #   c1 <- wafer::detect_lotid(df, return_col_names = TRUE)
    #   c2 <- wafer::detect_startlot(df, return_col_names = TRUE)
    #   c.lotids <- union(c1, c2)
    #
    #   breaks = seq(0, 10000, by = 1000)
    #   labels = seq_len(length(breaks)-1)
    #   col.label <- cut(1:ncol(df), breaks = breaks, labels = labels)
    #
    #   for (i in unique(col.label)){
    #
    #     indx1 <- which(names(df) %in% c.lotids)
    #     indx2 <- which(col.label == i)
    #
    #     if (i>1) {
    #       sum1 <- sum_df_wf(df[, indx1])
    #       sum2 <- sum_df_wf(df[, setdiff(indx2, indx1)])
    #       sum <- rbind(sum1, sum2)
    #     } else {
    #       sum <- sum_df_wf(df[, union(indx1, indx2)])
    #       }
    #
    #     d8ahelper::write_fwf(sum,
    #                          file = here::here("data",
    #                                            project = project,
    #                                            "downloads",
    #                                            glue::glue("{names(file)}_({i})_summary.txt")),
    #                          width = c(5, max(nchar(sum[[2]])) + 5, 12, rep(8, ncol(sum) - 3))
    #     )
    #
    #
    #   }
    # }
  })
}

load_files_clean <- function(load_from,
                             ...) {
  #'wraper function for load_files, with additional formatting and cleaning
  #'
  #'@description read csv files and recognize missing cells, archive and remove empty files, then run a set of functions for cleaning and formatting, return a list of data frames
  #'@param load_from a path where files are loaded from

  files <- d8ahelper::load_files(load_from = load_from, ...)

  result <- d8ahelper::lapply_preserve_names(files, function(file){
    message(names(file))

    df <- data.table::fread(file, na.strings = c("", "NA"))
    df <- tibble::as_tibble(df)

    # readr can give parse fails
    # df <- readr::read_csv(file, na = c("", "NA", " "), col_types = cols())

    # guard clause for empty data
    if (nrow(df) <= 1) {
      folder.archive <- file.path(load_from, "archive")
      dir.create(folder.archive, showWarnings = FALSE)
      file.copy(file,
                file.path(folder.archive, glue::glue("{names(file)}.csv")))
      file.remove(file)
      return(NA)
    }

    df <- df %>%
      wafer::format_all(.) %>%
      wafer::clean_ids(.) %>%
      d8ahelper::rm_single_unique_col(.) %>%
      d8ahelper::remove_empty_rows(.)

    #exclude datetime columns from filling in with chr str '[missing]'
    id.dt <- detect_datetime(df, return_loc = TRUE, silent=TRUE)

    id.chr <- which(sapply(df, is.character))

    id.of.int <- setdiff(id.chr, id.dt)

    if (length(id.of.int) >= 1) {
      df[, id.of.int] <- d8ahelper::fill_na_as_missing(df[, id.of.int])
    }

    return(df)

  })

  names(result) <- names(files)
  result <- result[!is.na(result)]
  return(result)
}


load_files_clean_wrapper <- function(project,
                                     replace = FALSE,
                                     return_df = FALSE,
                                     ...) {

  #' wraper function for load_files_clean with additional layer to specify a project and save back action

  loc <- here::here("data",
                    project,
                    "downloads")

  if (replace == TRUE) {
    save_path = loc
  } else {
    datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M")
    save_path = file.path(loc, datetime)
  }

  df.list <- load_files_clean(load_from = loc, ...)

  #either save csv or return dataframe
  if (return_df == TRUE){
    return(df.list)
  } else {
    d8ahelper::save_csv_from_a_list(df.list,
                                    path = save_path,
                                    use_name = TRUE,
                                    use_clean_names = FALSE,
                                    overwrite = TRUE,
                                    as_chr = TRUE
    )
  }


}

# 2.data transformation ------------------------------------------------------------------------------

detect_datetime <- function(df,
                            return_loc = FALSE,
                            return_col_names = FALSE,
                            reverse = FALSE,
                            re = 'datetime|date|trackedout|trackedin',
                            silent = FALSE) {
  if (!is.data.frame(df)) {
    if (silent==FALSE) {
      message('not a data.frame, NA returned')
    }
    return(NA)
  }

  if (is.data.frame(df)) {

    bool <- sapply(df, lubridate::is.POSIXt) | grepl(re, tolower(names(df)))

    if (!any(bool)) {
      if (silent==FALSE) {
        message('no datetime column detected')
      }
      return(NA)
    }

    idx1 <- which(sapply(df, lubridate::is.POSIXt))
    idx2 <- grep(re, tolower(names(df)))

    idx <- union(idx1, idx2)

    if (reverse == TRUE) {
      idx <- setdiff(seq_along(df), idx)
    }

    if (return_loc == TRUE) {
      return(idx)
    }

    if (return_col_names == TRUE) {
      return(names(df)[idx])
    }

    if (reverse == TRUE) {
      return(!bool)
    }

    return(bool)
  }
}


#Generage Date and Hour column based on Datetime column

add_datehour <- function(df, datecol = "trackedout", ...) {


  df$Date <- lubridate::as_date(df[[datecol]])
  df$Hour <- lubridate::hour(df[[datecol]])
  df <- df %>%
    mutate("DateHour" = paste0(Date, "-", Hour))

  df$Date <- as.factor(df$Date)
  df$DateHour <- as.factor(df$DateHour)
  return(df)
}

#' Convert time related columns to character
#'
#' background: POSIXct datetime format when output as csv will not be read corretly with JMP \cr
#' purpose: convert all time columns to character format befor exporting to csv \cr
#' note: accept regex if default rule of selecting columns containing "time" is not ideal \cr
#'
#' @param df a data frame
#' @param re a regular expression character vector to be used for matching to time column names
#' @example df <- datetime_to_chr(df, re = "time|datetime|trackedout")

datetime_to_chr <-
  function(df, re = "datetime|date|trackedout|trackedin") {

    idx <- detect_datetime(df, re = re, return_loc = TRUE)

    if (length(idx) >= 1) {
      df[, idx] <- as.character(df[, idx])
    }

    return(df)
  }

#' transform a all-numeric dataframe to delta between each lag number of columns

get_deltas <- function(df, lag, scale) {
  #'@param df a dataframe with no missing and purely numeric

  if (length(df) < lag) {
    stop("lag exceeds max column numbers")
  }

  dt_start <- df[, -((length(df) - lag + 1):length(df))]
  dt_end <- df[, -(1:lag)]

  deltas <- dt_end - dt_start

  names(deltas) <-
    paste(
      glue::glue("{lag}.steps - "),
      stringr::str_extract(names(dt_end), "[[:digit:]]{4}.[[:alnum:]]{2}.*"),
      "from.",
      stringr::str_extract(names(dt_start), "[[:digit:]]{4}.[[:alnum:]]{2}.*")
    )

  names(deltas) <- stringr::str_replace_all(names(deltas), "[\\.|\\s]+", " ")

  deltas <- deltas / as.numeric(scale)

  return(deltas)
}

#' wrapper function to get_deltas to return time deltas between given numbers of step lags

get_time_deltas <-
  function(df,
           col_re = 'trackedout',
           dt_fmt = "%Y-%m-%d %H:%M:%S",
           lag = 1:5,
           scale = "3600", #sec to min
           th=0.6) {

    dt <- wafer::clean_ids(df)

    id_cols <- wafer::detect_all(dt, return_loc = T)
    to_cols <- which(stringr::str_detect(names(dt), col_re))

    #remove missing rows/cols by id columns
    dt2 <-
      d8ahelper::clean_by_id(
        dt,
        id_col = id_cols,
        var_col = to_cols,
        filter_col = TRUE,
        th = th #no missings allowed
      )

    dt3 <- dt2[, -(id_cols), with = FALSE]

    dt3 <-
      as.data.frame(lapply(dt3, function(x) {
        as.numeric(as.POSIXct(x, format = dt_fmt))
      }))

    lag <- lag[lag != 1]

    to_combine <-
      lapply(lag, function(x) {
        get_deltas(dt3, x, scale = scale)
      })

    rhs <- do.call(cbind, to_combine)
    lhs <- dt2[, id_cols, with = FALSE]

    return(cbind(lhs, rhs))
  }


create_dummy <- function(df,
                         label,
                         simplify = FALSE,
                         filter_chr = TRUE,
                         ...) {
  #'wraper function to use caret::dummyVars to create dummy variables from character variables
  #'@param df a data frame
  #'@param response response columns, if any
  #'@param simplify a logical for if to simplify output to a dataframe, if FALSE returns a list
  #'@param filter_chr a logical for if calling filter_col_unique for removing columns based on near zero variance predictors

  df <- wafer::format_all(df)

  col_id <- wafer::detect_all(df, return_col_names = TRUE)
  col.c <- names(df)[sapply(df, is.character)]
  col.n <- names(df)[sapply(df, is.numeric)]
  col.f <- names(df)[sapply(df, is.factor)]

  df.id <- df[col_id]
  df.rsps <- df[label]
  df.n <- df[setdiff(col.n, c(col_id, label))]
  df.c <- df[setdiff(union(col.c, col.f), c(col_id, label))]


  if (filter_chr == TRUE){
    df.c <- filter_col_unique(df.c, simplify = TRUE)
  }

  # fullRank for total dependent vars (1 or the other scenario)
  dmy <- caret::dummyVars( ~., data = df.c, fullRank = TRUE)
  df.c.dummified <- as.data.frame(predict(dmy, newdata = df.c))
  names(df.c.dummified) <- str_remove(names(df.c.dummified), "^`")
  names(df.c.dummified) <- str_replace_all(names(df.c.dummified), "`", "...")

  if (simplify == TRUE){
    return(cbind(df.id, df.rsps, df.n, df.c.dummified))
  } else (
    return(list("df.id"  = df.id,
                "df.rsps" = df.rsps,
                "df.num" = df.n,
                "df.chr" = df.c,
                "df.chr.dummy" = df.c.dummified))
  )
}


filter_col_unique <- function(df,
                              percentUnique = 50, #TODO: figure out a proper ths
                              levelsAtMost = 20,
                              freqRatio = 1,
                              breaks = 50,
                              useDefault = FALSE,
                              quiet = TRUE,
                              simplify = FALSE) {
  #' wraper function to caret::nearZeroVar to filter chr columns for Zero- and Near Zero-Variance Predictors
  #'@param df a data frame
  #'@param percentUnique a numeric for percentUnique threshold, distinct values to total number of  samples, default to cut off if above 50percent.
  #'@param freqRatio a numeric for freqRatio threshold (most common value to the second most common value, default to cut off if below 1)
  #'@param useDefault a logical for if to use caret default for removing near zero variance variables
  #'@param plot a logical for if to include histograms of percentUnique and freqRatio
  #'@param simplify a logical for if to simplify output to a single data frame, else return a list

  nzv <- caret::nearZeroVar(df, saveMetrics = TRUE)

  if (quiet == FALSE) {
    hist(nzv$freqRatio, breaks = breaks)
    hist(nzv$percentUnique, breaks = breaks)
  }

  if (useDefault == TRUE) {
    # default
    nzv <- caret::nearZeroVar(df_data)
    return(df[, -nzv])
  }

  # setup threshold for categorical column levels

  nzv$levels <- sapply(df, function(x){
    length(unique(x))
  })

  #drop = FALSE to prevent convert to var if single col
  df1 <-
    df[, nzv$percentUnique < percentUnique &
         nzv$levels < levelsAtMost &
         nzv$freqRatio < freqRatio * nrow(df), drop = FALSE]

  df2 <- df[, !(nzv$percentUnique < percentUnique &
                  nzv$levels < levelsAtMost &
                  nzv$freqRatio < freqRatio * nrow(df)), drop = FALSE]


  if (simplify == TRUE) {return(df1)
  } else {
    list("df" = df1, "df.filtered" = df2)
  }
}

sum_to_lot <- function(df,
                       sum_if_chr = "most",
                       fill_na_as_missing = TRUE) {
  #' convert wafer level data to lot level data
  #' character: by default to use most frequent string value (include missing), or can use the use the first encountered value, neither are ideal and one should consider in the first place if reducing wafer level categorical value to lot level is a good idea
  #'
  #' @param sum_if_chr a character value of either "most" or "first"

  df <- wafer::clean_ids(df) #ensure startlot is unique
  df <- wafer::format_all(df)

  col.id.name <-
    wafer::detect_startlot(df, return_col_names = TRUE)

  df <- d8ahelper::move_left(df, str = col.id.name[[1]])#ensure startlot is on far-left

  col.id <-
    which(names(df) %in% wafer::detect_startlot(df, return_col_names = TRUE))
  col.ids <-
    which(names(df) %in% wafer::detect_all(df, return_col_names = TRUE))

  col.n <- setdiff(which(sapply(df, is.numeric)), col.ids)
  col.c <- setdiff(which(sapply(df, d8ahelper::is_fct_or_chr)), col.ids)

  df.c <- df[, c(col.id, col.c), with = FALSE]
  df.n <- df[, c(col.id, col.n), with = FALSE]

  df.c.list <- d8ahelper::encode_col(df.c)
  df.n.list <- d8ahelper::encode_col(df.n)

  df.n.list$df <- as_tibble(df.n.list$df) %>%
    group_by(c1) %>%
    summarise_if(is.numeric, list( ~ mean(., na.rm = TRUE))) %>%
    select(-c1)

  # if "most", for each group return most frequent value, if all NA: return "[missing]"
  if (sum_if_chr == "most") {

    df.c.list$df <- as_tibble(df.c.list$df) %>%
      d8ahelper::fill_missing_as_na() %>% #ignore missing
      group_by(c1) %>%
      summarise_if(d8ahelper::is_fct_or_chr, list(~ d8ahelper::get_mode(.))) %>%
      d8ahelper::fill_na_as_missing()

  } else if (sum_if_chr == "first") {

    #convert verb 'missing' to NA, then use first non NA value, if all NA, use NA.
    df.c.list$df <- as_tibble(df.c.list$df) %>%
      d8ahelper::fill_missing_as_na() %>%
      group_by(c1) %>%
      summarise_if(d8ahelper::is_fct_or_chr, list( ~ first(c(na.omit(.), NA)))) #too complicated

    # since 'first' is mostly used for datetime, making backfill str missing as option
    if (fill_na_as_missing == TRUE){
      df.c.list$df <- d8ahelper::fill_na_as_missing(df.c.list$df)
    }
  }



  df.c <- d8ahelper::decode_col(df.c.list)
  df.n <- d8ahelper::decode_col(df.n.list)

  cbind(df.c, df.n)

}


contain_value <- function(x) {

  .doesnt_contain_value <- function(x){
    is.null(x) | is.na(x) | grepl("^[[:space:]]?$", x)
  }

  if (length(x)<=1) {
    return(!.doesnt_contain_value(x))
  }

  sapply(x, function(y) {
    !.doesnt_contain_value(y)
  })
}

#' join response file to data file and save file
#' response file is searched in the working_dir folder
#'
#' @param summarize determine if to match wafer level data to lot level signals
#' @param .re_for_dt regular expression string used to identify 'datetime' data that is to be joined with each individual data files
#'
join_label <- function(project,
                       label_file_name,
                       data_folder = "downloads",
                       output_folder = here::here("data", project, "working_dir", "feature_importance"),
                       summarize = TRUE,
                       return_df = FALSE,
                       recode_gb = TRUE,
                       join_dt = FALSE,
                       .re_for_dt = "trackout",
                       rm_dups = FALSE,
                       dups_keys = c('scribe'),
                       ...) {

  # dir.wk <- here::here("data", project, "working_dir")
  dir.dl <- here::here("data", project, data_folder)

  files <- d8ahelper::load_files(dir.dl, full.names = FALSE, ...)

  if (!file.exists(here::here("data", project, label_file_name))){
    return(message(glue::glue("cannot find {label_file_name} in project main folder, did you name the file correctly (include '.csv') or did you put the file inside the project main directory?")))
  }

  df.gb <- d8ahelper::load_csv(label_file_name,
                               path = here::here("data", project))

  #recode 'good' as '_good' for better jmp visual
  if (recode_gb == TRUE) {

    gb_cols <- names(df.gb)[grepl("^gb", names(df.gb))]

    for (i in seq_along(gb_cols)) {
      .col <- gb_cols[[i]]
      df.gb[[.col]] <- sapply(df.gb[[.col]], function(x) ifelse(x == "good", "_good", x))
    }
  }

  #or to use wafer::clean_ids if unsure if metric csv has duplicated id columns
  df.gb <- wafer::format_all(df.gb, .rename_baselot = TRUE)
  df.gb <- wafer::clean_ids(df.gb)

  if (rm_dups == TRUE) {
    #remove duplicates based on 'scribe'
    df.gb <- rm_dups_w_less_data(df.gb, keys=dups_keys)
  }

  col.id <- wafer::detect_all(df.gb, return_loc = TRUE)
  col.gb <- grep("^[gb|metric]", names(df.gb))
  col.rest <- setdiff(seq_along(df.gb), c(col.id, col.gb))

  #only add _orig to cols that do not have it
  w_o_orig_ending <- col.rest[!grepl("_orig$", names(df.gb)[col.rest])]
  names(df.gb)[w_o_orig_ending] <- paste0(names(df.gb)[w_o_orig_ending], "_orig")

  level.gb <- wafer::detect_level(df.gb)

  #load datetime file for join
  if (join_dt == TRUE) {
    if (sum(stringr::str_detect(files, .re_for_dt)) != 1) {
      message(glue::glue('none or multile files match to {.re_for_dt}'))
    } else {
      file.dt <- files[stringr::str_detect(files, .re_for_dt)]
      df.dt <- d8ahelper::load_csv(file.dt, path = dir.dl)

      # prep a lot level datetime, in case summarize == TRUE and need to join lot leve dt
      level.dt <- wafer::detect_level(df.dt)

      if (summarize == TRUE) {
        if (level.dt == "wafer" && level.gb == "startlot") {
          df.dt <-
            sum_to_lot(df.dt,
                       sum_if_chr = "first",
                       #do not backfill missing values informatively for datetime csv
                       fill_na_as_missing = FALSE)
        }
      }

      #append _excl to datatime table so that they are excluded in feature selection (pre-proc)
      dt.id <- wafer::detect_all(df.dt, return_loc = TRUE)
      col_add_excl <- setdiff(seq_along(df.dt), dt.id)
      names(df.dt)[col_add_excl] <-
        paste0(names(df.dt)[col_add_excl], '_excl')

    }
  }

  pb <- progress::progress_bar$new(total = length(files))

  d8ahelper::lapply_preserve_names(files, function(file){

    message(names(file))

    pb$tick()

    df <- d8ahelper::load_csv(file, path = dir.dl)

    if (nrow(df) == 0) return(NULL,)

    df <- wafer::clean_ids(df)
    df <- wafer::format_all(df)

    level.data <- wafer::detect_level(df)

    #summarize data to only lot level
    #for datetime file, use first occurrence, else use most occurrence
    if (summarize == TRUE) {
      if (level.data == "wafer" && level.gb == "startlot") {
        if (grepl(.re_for_dt, file) == TRUE) {
          df <- sum_to_lot(df,
                           sum_if_chr = "first",
                           fill_na_as_missing = FALSE)
        } else {
          df <- sum_to_lot(df, sum_if_chr = "most")
        }
      }
    }

    join_level <- wafer::detect_join_level(level.data, level.gb)

    join_key <- wafer::detect_join_keys(df.gb, df, level = join_level)

    count <- data.table(df)[, list(Freq =.N), by=eval(join_key$rhs)]

    #if there are multiple rows per id(s), reduce to 1 row per id
    if (sum(count$Freq > 1) > 0) {
      if (join_level == "startlot") {
        df <- sum_to_lot(df,
                         sum_if_chr = "first",
                         fill_na_as_missing = FALSE)
      }

      if (join_level == "wafer") {
        #Rtip:use by=list(a,b) or by=eval(foo) where foo==c('a', 'b')

        #data.table::setDT(df)
        #df <- df[, .SD[first(contain_value(waferid))], by=startlot]

        #slice first row that contain waferid value
        df <- df %>%
          group_by(startlot, waferid) %>%
          slice(first(which(contain_value(waferid))))
      }

    }

    a <- data.table::as.data.table(df.gb)
    data.table::setkeyv(a, join_key[["lhs"]])

    b <- data.table::as.data.table(df)
    data.table::setkeyv(b, join_key[["rhs"]])

    #find out duplicated ID columns on rhs thats not used for joining and delete
    lhs.id <- wafer::detect_all(a, return_col_names = T)
    rhs.id <- wafer::detect_all(b, return_col_names = T)
    dup.rhs <- setdiff(intersect(lhs.id, rhs.id), join_key[['rhs']])

    #remove id columns that are not used for joining in the rhs data table
    if (length(dup.rhs) > 0){
      b[, which(names(b) %in% dup.rhs)] <- NULL
    }

    c <-
      merge(a,
            b,
            # by.x = join_key[["lhs"]],
            # by.y = join_key[["rhs"]],
            all.x = TRUE)  #a[b]

    #remove rows where gb/metric column is all NAs
    joey <- lapply(grep('^gb|^metric', names(c)), function(x) {
      !sapply(c[,..x], is.na)
    })

    rachel <- Reduce("|", joey)

    d <- c[which(rachel), ]

    if (join_dt == TRUE & !stringr::str_detect(file, .re_for_dt)) {
      if (!any(stringr::str_detect(files, .re_for_dt))) {
        message(glue('no file found matches to {.re_for_dt}'))
      } else {

        e <- data.table::as.data.table(df.dt)
        d <- data.table::as.data.table(d)

        level.e <- wafer::detect_level(e)
        level.d <- wafer::detect_level(d)
        join_level2 <- wafer::detect_join_level(level.e, level.d)
        join_key2   <- wafer::detect_join_keys(e, d, level = join_level2)

        e <- wafer::format_all(e)
        d <- wafer::format_all(d)

        data.table::setkeyv(d, join_key2[["lhs"]])
        data.table::setkeyv(e, join_key2[["rhs"]])

        #find out duplicated ID columns on rhs thats not used for joining and delete
        lhs.id <- wafer::detect_all(d, return_col_names = T)
        rhs.id <- wafer::detect_all(e, return_col_names = T)
        dup.rhs <- setdiff(intersect(lhs.id, rhs.id), join_key2[['rhs']])
        if (length(dup.rhs) > 0) {
          e[, which(names(e) %in% dup.rhs)] <- NULL
        }

        d <-
          merge(d,
                e,
                # by.x = join_key2[["lhs"]],
                # by.y = join_key2[["rhs"]],
                all.x = TRUE)
      }
    }

    if (return_df == TRUE) {
      return(d)
    }

    if (return_df == FALSE) {
      d8ahelper::save_csv(
        d,
        file.name = paste0("lb_", names(file), ".csv"),
        path = output_folder,
        overwrite = TRUE
      )
    }

  })

}

pval_to_sig <- function(x) {
  cut(
    x,
    breaks = c(0, 0.01, 0.05, 0.2, 0.5, 1),#TODO figure out proper levels
    labels = c("****", "***", "**", "*", "")
  )
}

sum_pt_to_wf <- function(df) {
  #' summarize numeric columns to a set number of statistics

  df <- wafer::clean_ids(df)
  col.id <- wafer::detect_all(df, return_col_names = TRUE)

  df %>%
    group_by_at(col.id) %>%
    summarize_if(is.numeric,
                 c(mean = mean,
                   med = median,
                   q10 = d8ahelper::q10,
                   q25 = d8ahelper::q25,
                   q75 = d8ahelper::q75,
                   q90 = d8ahelper::q90
                 ),
                 na.rm = TRUE)
}

add_deck_col <- function(df, col, name = "deck"){
  start <-
    sapply(df[[col]], function(x)
      stringr::str_locate(x, "_D[[:digit:]]{1}")[1] + 1)

  end <-
    sapply(df[[col]], function(x)
      stringr::str_locate(x, "_D[[:digit:]]{1}")[2])

  df[[name]] <- substr(df[[col]], start, end)

  return(df)
}

add_tier <- function(df, col = "file"){
  # since not all features are created equal
  t2 <- c("qdr", "rda", "n2purge", "carrier_id", "fd_uva", "tw_nearest", "transition", "time_deltas")
  t3 <- c("eet", "bulk_consumable", "foup_history")

  df[['tier']] <- ifelse(stringr::str_detect(df[[col]], paste0(t2, collapse = "|")), "2", "1")
  df[['tier']] <- ifelse(stringr::str_detect(df[[col]], paste0(t3, collapse = "|")), "3", df[['tier']])

  df <- d8ahelper::move_left(df, 'tier')
  return(df)
}

# 3.pre-processing ----------------------------------------------------------

pre_proc <- function(df,
                     label = "gb",
                     col_type = "numeric",
                     rm_outlier = FALSE,
                     thres = 3,
                     fct_lump_prop = 1 / 50,
                     levels_at_most = 15) {
  #' pre-processing to prepare for modeling and feature selection
  #' col_type: numeric, categorical, or 1-hot(convert categorical to 1/0)
  #' @thres a number, multiplier of sigma value to determine threshold for removing outliers
  #' @return a dataframe with columns ordered in id-response-feature

  # remove rows w/ no response
  df <- df[!is.na(df[[label]]), ]

  df <- d8ahelper::rm_single_unique_col(df)

  df[[label]] <- tolower(df[[label]])
  df <- wafer::format_all(df)
  df <- wafer::clean_ids(df)

  if (is.numeric(df[[label]]) && rm_outlier == TRUE) {
    low = mean(df[[label]], na.rm = TRUE) - thres * sd(df[[label]], na.rm = TRUE)
    high = mean(df[[label]], na.rm = TRUE) + thres * sd(df[[label]], na.rm = TRUE)

    df <- df[(df[[label]] < high),]
    df <- df[(df[[label]] > low),]
  }

  id0 <- wafer::detect_all(df, return_loc = TRUE) #lot/wafer ids
  id1 <-
    which(names(df) == label) #response (g/b, num metric)

  id2.1 <-
    grep("_orig$|^gb|^metric|^cv|_excl$|\\.y$", tolower(names(df))) #non-datetime col to exclude
  id2.2 <-
    detect_datetime(df, return_loc = TRUE) #datetime cols to exclude

  id2 <- union(id2.1, id2.2) #to exclude

  #idx3: numeric or categorical
  if (col_type == "numeric") {
    id3.1 <- setdiff(which(sapply(df, is.numeric)), id2)

    #checkpoint
    if (length(id3.1) == 0) {
      return(NULL)
    }

    #only include numerical with >= 5 unique values
    id3.2 <- which(sapply(df, function(x) {
      isTRUE(length(unique(x[!is.na(x)])) >= 5)
    }))

    id3 <- intersect(id3.1, id3.2)

  }

  if (col_type %in% c("categorical", "1-hot")) {
    id3.1 <- setdiff(which(sapply(df, is.character)), id2)

    #checkpoint
    if (length(id3.1) == 0) {
      return(NULL)
    }

    #numerical with < 5 unique values will be treated as categorical (exclude)
    id3.2 <- setdiff(which(sapply(df, function(x) {
      isTRUE(length(unique(x[!is.na(x)])) < 5)
    })), id2)

    id3 <- union(id3.1, id3.2)
  }

  df1 <- df[, Reduce(union, c(id0, id1, id3))] #id-response-features

  #pre-proc categorical columns
  if (col_type == "categorical") {
    col.ids <- wafer::detect_all(df1, return_col_names = TRUE)
    col.datetime <-
      grep("datetime", tolower(names(df1)), value = TRUE)
    col.extra.garbage  <-
      grep("lotid$|scribe$", tolower(names(df1)), value = TRUE)
    col.features <-
      setdiff(names(df1),
              c(label,
                col.ids,
                col.extra.garbage,
                col.datetime))

    #checkpoint
    if (length(col.features) == 0) {
      return(NULL)
    }

    df1.features <- df1[col.features]

    df1.features <- as.data.frame(sapply(df1.features, function(x) {
      as.factor(x)
    }))

    #filter categorical cols
    df1.features <-
      filter_col_unique(
        df1.features,
        percentUnique = 50,
        levelsAtMost = levels_at_most,
        freqRatio = 1,
        quiet = TRUE,
        simplify = TRUE
      )

    #lump low occurrence categories
    df1.features <- as.data.frame(sapply(df1.features, function(x) {
      forcats::fct_lump_prop(x, fct_lump_prop)
    }))


    if (nrow(df1.features) == nrow(df1)) {
      df1 <- cbind(df1[c(col.ids, label)], df1.features)
    } else {
      return(NULL)
    }

  }

  if (col_type == "1-hot") {

    dummy <- create_dummy(df1, label = label)
    df.dummy <- dummy$df.chr.dummy

    # df.dummy <-
    #   d8ahelper::remove_high_corr_features(df.dummy, level = 1)

    df1 <- cbind(dummy$df.id, dummy$df.rsps, df.dummy)
  }

  return(df1)

}


pre_proc_wrapper <- function(df,
                             .label = 'gb') {
  #wrapper function for preprocessing both cat/num columns

  #numeric
  df_num <- pre_proc(df,
                     label = .label,
                     col_type = "numeric")

  #categorical
  df_cat <- pre_proc(df,
                     label = .label,
                     col_type = "categorical")

  if (is.null(df_num) && is.null(df_cat)) return(NA)
  if (is.null(df_num)) return(df_cat)
  if (is.null(df_cat)) return(df_num)

  return(cbind(df_cat, df_num))

}

# 4.feature_selection ---------------------------------------------------------------------------

fs_gb <- function(df,
                  label = "gb",
                  col_type = "numeric") {
  #' use simple ANOVA and p-values for signal screening
  #' made the choice to read file individually to preserve file names
  #' col_type: numeric, categorical, or 1-hot(convert categorical to 1/0)
  #' flow include i.pre-process, ii.modeling, iii.summarize output
  #' @return a list of lists, one for each file and each contain numeric/categorical results

  #encode col names, or, checking in the columns
  df.e <- d8ahelper::encode_col(df)

  ids <-
    d8ahelper::get_id(df.e, wafer::detect_all(df, return_col_names = TRUE))
  response <- d8ahelper::get_id(df.e, label)

  vars <- setdiff(names(df.e$df), union(ids, response))

  #generate a list of p-values for each file
  anova.p <- sapply(vars, function(var) {

    df <- df.e$df[c(response, var)]
    df <- df[!is.na(df[[var]]), ]

    #avoid single unique outcome / too few unique input cases
    var.limit <- switch(
      col_type,
      "numeric" = 2,
      "categorical" = 1,
      "1-hot" = 1
    )

    if (length(unique(df[[response]])) <= 1 |
        length(unique(df[[var]])) <= var.limit) {
      return(NA)
    }

    #numeric features, use ANOVA, no transformation needed
    if (col_type %in% c("numeric", "1-hot")) {
      f <- as.formula(paste(var,
                            response,
                            sep = " ~ "))
    }

    #categorical features, 1-hot dummify response column then use ANOVA
    if (col_type == "categorical") {

      df.rsps <- df[response]
      df.var <- df[var]

      dmy <- caret::dummyVars( ~ ., data = df.rsps, fullRank = TRUE)
      df.rsps.dummified <- as.data.frame(predict(dmy, newdata = df.rsps))

      names(df.rsps.dummified) <- response
      df <- cbind(df.var, df.rsps.dummified)

      #in reverse
      f <- as.formula(paste(response,
                            var,
                            sep = " ~ "))
    }

    #modeling
    model <- aov(f, data = df)
    model.tidy <- broom::tidy(model)
    model.tidy$p.value[1]
  })

  if (all(is.na(anova.p)) == TRUE) {
    return(NA)
  }

  p <- data.table::as.data.table(anova.p, keep.rownames = TRUE)
  data.table::setorderv(p, "anova.p")

  #col name translation
  p$name <- sapply(p$rn, function(x)
    df.e$tag[[x]])

  p$miss <- sapply(p$rn, function(x) {
    chandler <- df.e$df[[x]]
    d8ahelper::format_to_percentage(length(union(
      which(is.na(chandler)),
      which(chandler == "[missing]")
    )) / length(chandler))
  })

  p$pt <- sapply(p$rn, function(x) {
    sum(!(is.na(df.e$df[[x]]) | df.e$df[[x]] == '[missing]'))
  })

  p$pt.g <-
    sapply(p$rn, function(x) {
      sum(grepl('good', df.e$df[[response]]) & (!(is.na(df.e$df[[x]]) | df.e$df[[x]] == '[missing]')))
    })

  p$pt.b <-
    sapply(p$rn, function(x) {
      sum(grepl('bad', df.e$df[[response]]) & (!(is.na(df.e$df[[x]]) | df.e$df[[x]] == '[missing]')))
    })

  p$sig <- pval_to_sig(p$anova.p)

  # special col for categorical: uniq

  if (col_type == "categorical") {

    p$col_type <- "categorical"

    p$uniq <-
      sapply(p$rn, function(x) {
        length(unique(df.e$df[[x]]))
      })
  }

  # special col for categorical: comp
  if (col_type == "numeric") {
    p$col_type <- "numeric"

    p$comp <- sapply(p$rn, function(x) {
      good.data <- df.e$df[[x]][grepl('good', df.e$df[[response]])]
      bad.data <- df.e$df[[x]][grepl('bad', df.e$df[[response]])]

      if (!(all(is.na(good.data)) || all(is.na(bad.data)))) {
        good.max <- max(good.data, na.rm = TRUE)
        good.min <- min(good.data, na.rm = TRUE)
        good.90p <- d8ahelper::q90(good.data, na.rm = TRUE)
        good.75p <- d8ahelper::q75(good.data, na.rm = TRUE)
        good.25p <- d8ahelper::q25(good.data, na.rm = TRUE)
        good.10p <- d8ahelper::q10(good.data, na.rm = TRUE)

        bad.max <- max(bad.data, na.rm = TRUE)
        bad.min <- min(bad.data, na.rm = TRUE)
        bad.90p <- d8ahelper::q90(bad.data, na.rm = TRUE)
        bad.75p <- d8ahelper::q75(bad.data, na.rm = TRUE)
        bad.25p <- d8ahelper::q25(bad.data, na.rm = TRUE)
        bad.10p <- d8ahelper::q10(bad.data, na.rm = TRUE)

        if (good.max < bad.min)
          return("g<<<b")
        if (good.90p < bad.10p)
          return("g<<b")
        if (good.75p < bad.25p)
          return("g<b")
        if (good.min > bad.max)
          return("g>>>b")
        if (good.10p > bad.90p)
          return("g>>b")
        if (good.25p > bad.75p)
          return("g>b")

      }

      return("")
    })
  }


  if (col_type == "1-hot") {
    p$name.dummy <- sapply(p$rn, function(x)
      df.e$tag[[x]])
    p$name =  stringr::str_split(p$name.dummy, pattern = "\\.\\.\\.")[[1]][1]
    p$item =  stringr::str_split(p$name.dummy, pattern = "\\.\\.\\.")[[1]][2]
    p$col_type <- "categorical.1-hot"

    p$name.dummy <- NULL
  }

  #lot/step info
  if (wafer::detect_startlot(df)) {
    col.startlot <-
      d8ahelper::get_id(df.e, wafer::detect_startlot(df, return_col_names = TRUE)[1])[1]

    p$lot.g <- sapply(p$rn, function(x) {
      tf.good <- grepl('good', df.e$df[[response]]) & (!(is.na(df.e$df[[x]]) | df.e$df[[x]] == '[missing]'))
      length(unique(df.e$df[[col.startlot]][tf.good]))
    })

    p$lot.b <- sapply(p$rn, function(x) {
      tf.bad <- grepl('bad', df.e$df[[response]]) & (!(is.na(df.e$df[[x]]) | df.e$df[[x]] == '[missing]'))
      length(unique(df.e$df[[col.startlot]][tf.bad]))
    })

    p$step <- sapply(p$name, function(str) {
      stringr::str_extract(str, pattern = "[[:digit:]]{4}-[[:alnum:]]{2}[^::]*")
    })

  }

  p <-
    d8ahelper::move_left(
      df = p,
      str = c("rn",
              "anova.p",
              "name",
              "sig",
              "pt",
              "miss",
              "pt.b",
              "pt.g",
              "comp",
              "uniq",
              "lot.g",
              "lot.b"
      )
    )

  return(p)

}


fs_num <- function(df,
                   col_type,
                   label = "metric",
                   cross_v = FALSE,
                   caret_rpt_cv = FALSE,
                   fit_poly = FALSE,
                   ...
) {

  df[[label]] <- as.numeric(df[[label]])
  df2 <- d8ahelper::encode_col(df)

  id <- d8ahelper::get_id(df2, wafer::detect_all(df, return_col_names = TRUE))

  response <- d8ahelper::get_id(df2, label)

  feats <- setdiff(names(df2$df), union(id, response))

  range_metric <- max(df2$df[response], na.rm = T) - min(df2$df[response], na.rm = T)

  # modeling, one feature at a time
  lr <- lapply(feats, function(x) {

    df <- df2$df[c(response, x)]
    df <- df[!is.na(df[[x]]), ]

    var.limit <- switch(
      col_type,
      "numeric" = 5,
      "categorical" = 2
    )

    if (nrow(df) <= 2 |
        length(unique(df[[response]])) <= 1 |
        length(unique(df[[x]])) <= var.limit) {
      return(NA)
    }

    range_metric_new <- max(df[response], na.rm = T) - min(df[response], na.rm = T)
    range_perc <- d8ahelper::format_to_percentage(range_metric_new / range_metric, digits = 0)

    if (fit_poly==TRUE){
      if(unique(x)<=2) return(NA)

      f <- as.formula(paste(response,
                            glue::glue("poly({x}, 2)"),
                            sep = " ~ "))
    } else {
      f <- as.formula(paste(response,
                            x,
                            sep = " ~ "))
    }




    if (col_type == "numeric") {
      # simple lm
      # model <- lm(f, data = df)
      # sm <- summary(model)
      # model.tidy <- broom::tidy(model)

      if (cross_v == FALSE) {
        #model
        model <- lm(f, data = df)
        sm <- summary(model)
        model.tidy <- broom::tidy(model)

        #record results
        final <-  list(
          "rn" = x,
          "rsq.adj" = sm$adj.r.squared,
          "p.val" = model.tidy$p.value[1],
          "slope" = model$coefficients[[2]],
          "r.c" = range_perc
        )
      }

      if (cross_v == TRUE){

        # cross-validation(manual)
        uniq <- length(unique(!is.na(df[[x]])))

        k <- dplyr::case_when(
          uniq < 50 ~ 2,
          uniq < 500 ~ 5,
          uniq >= 500 ~ 10
        )

        cv_kf_lm <- function(df,
                             k,
                             repeats = 2,
                             ycol) {

          resample <- list()

          for (i in seq_len(repeats)) {

            sampling <- caret::createFolds(df[[ycol]], k)

            resample[[i]] <- lapply(sampling, function(x) {

              #split
              train <- df[-x,]
              validation <- df[x,]

              #model
              model <- lm(f, data = train)
              sm <- summary(model)
              model.tidy <- broom::tidy(model)

              #get validation set model performance
              p <- predict(model, validation)
              error <- (p - validation[[ycol]])
              rmse <- sqrt(mean(error^2))
              mae <- mean(abs(error))

              #record results
              result <-  list("rsq.adj" = sm$adj.r.squared,
                              "p.val" = model.tidy$p.value[1],
                              "slope" = model$coefficients[[2]],
                              "rmse" = rmse,
                              "mae" = mae)

              return(result)
            })
          }

          do.call(dplyr::combine, resample)

        }

        resample.result <-
          cv_kf_lm(
            df = df,
            k = k,
            repeats = 5,
            ycol = response
          ) #k-fold, 5x repeats

        final <- list(
          "rn" = x,
          "rsq.adj" = abs(mean(
            sapply(resample.result, function(x)
              x$rsq.adj), na.rm = TRUE
          )),
          #rtn rsq if num vs num
          "p.val" = abs(mean(
            sapply(resample.result, function(x)
              x$p.val), na.rm = TRUE
          )),
          #use median value for slope across all folds and repeats
          "slope" = median(sapply(resample.result, function(x)
            x$slope), na.rm = TRUE),
          "sl.sd" = sd(sapply(resample.result, function(x)
            x$slope), na.rm = TRUE),
          "rmse" = abs(mean(
            sapply(resample.result, function(x)
              x$rmse), na.rm = TRUE
          )),
          "mae" = abs(mean(
            sapply(resample.result, function(x)
              x$mae), na.rm = TRUE
          )),
          "r.c" = range_perc #response range percentage to original after removing empty rows per each feature
        )
      }

      # cross-validation(caret)
      if (caret_rpt_cv == TRUE) {
        # cl <- parallel::makeCluster(10)
        # doParallel::registerDoParallel(cl)

        train.control <- caret::trainControl(method = "repeatedcv",
                                             number = k,
                                             repeats = 2)

        #train
        model.cv <- caret::train(f, data = df, method = "lm",
                                 trControl = train.control)

        #summarize
        rsq.cv <- mean(model.cv$resample$Rsquared, na.rm = TRUE)
        rmse.cv <- mean(model.cv$resample$RMSE, na.rm = TRUE)
        mae.cv <- mean(model.cv$resample$MAE, na.rm = TRUE)

        # parallel::stopCluster(cl)
      }


      if (caret_rpt_cv == TRUE) {
        final$rsq.cv <- rsq.cv
        final$rmse.cv <- rmse.cv
        final$mae.cv <- mae.cv
      }
    }

    if (col_type == "categorical") {
      # df[[x]] <- as.factor(df[[x]])
      model <- aov(f, data = df)
      model.tidy <- broom::tidy(model)

      #add cv for reference (do not use for ranking)
      cv_kf_aov <- function(df,
                            k,
                            repeats = 2,
                            ycol) {

        resample <- list()

        for (i in seq_len(repeats)) {
          sampling <- caret::createFolds(df[[ycol]], k)

          resample[[i]] <- lapply(sampling, function(fold) {
            #split
            train <- df[-fold,]
            validation <- df[fold,]

            if (length(unique(train[[x]])) <= 1 | length(unique(validation[[x]])) <= 1) {
              return(NA)
            }

            #model
            model.train <- broom::tidy(aov(f, data = train))
            p.train <- model.train$p.value[1]

            #validation not used yet
            model.val <- broom::tidy(aov(f, data = validation))
            p.val <- model.val$p.value[1]

            return(p.train)
          })
        }

        if (all(is.na(unlist(resample)))) {
          return(NA)
        } else {
          mean(unlist(resample), na.rm = TRUE)
        }
      }

      resample.result <- cv_kf_aov(df = df, k = 2, repeats = 5, ycol = response) #k-fold, 5x repeats

      final <- list("rn" = x,
                    "p.val" = unlist(model.tidy$p.value[1]),
                    "p.cv" = resample.result,
                    "r.c" = range_perc)
    } #end of 'categorical'

    return(final)
  })

  if (all(is.na(lr)) == TRUE) {
    return(NA)
  }

  lr <- lr[sapply(lr, is.list)]

  #summary
  sm <- data.table::data.table("rn" = sapply(lr, function(x) x$rn),
                               "p.val" = sapply(lr, function(x) x$p.val)
  )

  sm <- d8ahelper::remove_empty_rows(sm)

  # patch TODO: figure out why sometimes p.val is a list
  sm <- sm[!sapply(sm$p.val, is.null), ]

  sm$p.val <- unlist(sm$p.val)

  sm$name <- sapply(sm$rn, function(x)
    df2$tag[[x]])

  sm$pt <- sapply(sm$rn, function(x) {
    sum(!is.na(df2$df[[x]]))
  })

  sm$uniq <- sapply(sm$rn, function(x){length(unique(df2$df[[x]]))})

  sm$miss <- sapply(sm$rn, function(x) {
    monica <- df2$df[[x]]
    d8ahelper::format_to_percentage(length(union(
      which(is.na(monica)),
      which(monica == "[missing]")
    )) / length(monica), digits = 1)
  })

  sm$r.c <- sapply(lr, function(x) x$r.c)

  #metric unique to numeric
  if (col_type == "numeric") {

    sm$rsq.adj <- sapply(lr, function(x) abs(x$rsq.adj))
    sm$slope <- sapply(lr, function(x) x$slope)

    if (cross_v == TRUE){
      sm$rmse <- sapply(lr, function(x) abs(x$rmse))
      sm$mae <- sapply(lr, function(x) abs(x$mae))
      sm$sl.sd <- sapply(lr, function(x) x$sl.sd)
    }

    data.table::setorderv(sm, "rsq.adj")

    if (caret_rpt_cv == TRUE){
      sm$rsq.cv <- sapply(lr, function(x) abs(x$rsq.cv))
      sm$rmse.cv <- sapply(lr, function(x) x$rmse.cv)
      sm$mae.cv <- sapply(lr, function(x) x$mae.cv)
      data.table::setorderv(sm, "rsq.cv")
    }

  }

  #metric unique to categorical
  if (col_type == "categorical"){
    sm$p.cv = sapply(lr, function(x) x$p.cv)
  }

  #incl lot, step info in summary output
  if (wafer::detect_startlot(df)) {

    col.startlot <-
      d8ahelper::get_id(df2, wafer::detect_startlot(df, return_col_names = TRUE)[1])[1]

    sm$lot <- sapply(sm$rn, function(x) {
      not.na <- !is.na(df2$df[[x]])
      length(unique(df2$df[[col.startlot]][not.na]))
    })

    sm$step <- sapply(sm$name, function(str) {
      stringr::str_extract(str, pattern = "[[:digit:]]{4}-[[:alnum:]]{2}[^::]*")
    })

  }

  sm <-
    d8ahelper::move_left(
      df = sm,
      str = c(
        "rn",
        "name",
        "rsq.adj",
        "pt",
        "lot",
        "r.c",
        "miss",
        "rmse",
        "mae",
        "uniq",
        "slope",
        "sl.sd"
      )
    )

  return(sm)

}

fs_gb_wrapper <- function(df,
                          .label = 'gb') {
  #wrapper function for preprocessing and feature selection on data w/ good-bad response

  #numeric
  df_num <- pre_proc(df,
                     label = .label,
                     col_type = "numeric")

  if (is.null(df_num) == TRUE) {
    num <- NA
  } else {
    num <- fs_gb(df_num,
                 label = .label,
                 col_type = "numeric")
  }

  #categorical
  df_cat <- pre_proc(df,
                     label = .label,
                     col_type = "categorical")

  if (is.null(df_cat) == TRUE) {
    cat <- NA
  } else {
    cat <- fs_gb(df_cat,
                 label = .label,
                 col_type = "categorical")
  }

  list("numeric" = num,
       "categorical" = cat)
}

fs_num_wrapper <- function(df,
                           .label = 'metric',
                           .rm_outlier = TRUE,
                           .fct_lump_prop = 1 / 50,
                           .levels_at_most = 15,
                           ...
) {
  #wrapper function for preprocessing and feature selection on data w/ numeric response

  #numeric - preproc
  df_num <- pre_proc(df,
                     label = .label,
                     col_type = "numeric",
                     rm_outlier = .rm_outlier)

  #numeric - fs
  if (is.null(df_num) == TRUE) {
    num <- NA
  } else {
    num <- fs_num(df_num,
                  label = .label,
                  col_type = "numeric", ...)
  }

  #categorical - preproc
  df_cat <- pre_proc(df,
                     label = .label,
                     col_type = "categorical",
                     rm_outlier = .rm_outlier,
                     fct_lump_prop = .fct_lump_prop,
                     levels_at_most = .levels_at_most)

  #categorical - fs
  if (is.null(df_cat) == TRUE) {
    cat <- NA
  } else {
    cat <- fs_num(df_cat,
                  label = .label,
                  col_type = "categorical")
  }

  list("numeric" = num,
       "categorical" = cat)
}


# ff for combine same category results from different files ("numeric" vs "categorical")
combine_same_type_df <- function(name){
  function(list){
    filtered.list <- sapply(list, function(x){
      ifelse(is.data.frame(x[[name]]), x[name], NA)
    })
    filtered.list <- filtered.list[sapply(filtered.list, is.data.frame)]

    if (is.list(filtered.list)){
      do.call(rbind, filtered.list)
    } else {
      return(NA)
    }

  }
}

combine_num <- combine_same_type_df("numeric")
combine_cat <- combine_same_type_df("categorical")

# 5.workflow essentials ----------------------------------------------------------------------


#Perform non intrusive (no mod) inspection of a directory and return a dataframe with file info sorted by file size
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

# foo <- insp_dir(here::here("data", "_adhoc"))

#' given a directory, clear files given the pattern except for files matching to 'avoid' arg
#' @param pattern a regex string, used to match files to be deleted
#' @param avoid a regular expression string, matched files will not be deleted
#'
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



wide_to_tall <- function(df,
                         return_list = FALSE){
  #' convert wide point level data table to tall format based on column naming conventions
  #'
  #' @param return_list a bool, if TRUE then return list of data frames, one for tall format point level data, one for the residual columns (that weren't being considered as point level data)

  df <- data.table::as.data.table(df)
  df <- wafer::format_all(df)

  idx.id <- wafer::detect_all(df, return_loc = TRUE)
  idx.pt.candidates <- which(str_detect(names(df), "- P[[:digit:]]+"))

  df.id <- df[, idx.id, with = FALSE]

  dt <-
    data.table("name" = names(df)[idx.pt.candidates])

  dt$point <-
    sapply(dt$name, function(x)
      stringr::str_extract(x, " P[[:digit:]]+"))

  dt$prefix <-
    sapply(dt$name, function(x)
      stringr::str_split(x, " - P[[:digit:]]+")[[1]][1])

  dt$suffix <-
    sapply(dt$name, function(x)
      tolower(stringr::str_split(x, "P[[:digit:]]+ - ")[[1]][2]))

  # accept suffix of 'P97 - site, P98 - site', 'P97 - site97, P98 - site98'
  dt$suffix[stringr::str_detect(dt$suffix, "site[[:digit:]]*")] <- NA

  dt[, count := .N, by = c('prefix', 'suffix')]

  dt[count >= 5 & is.na(suffix), yn := "y"]

  idx.pt.real <- idx.pt.candidates[which(dt$yn == "y")]
  point_label <- dt$point[which(dt$yn == "y")]
  point_group <- dt$prefix[which(dt$yn == "y")]
  fct <- as.factor(point_group)

  df.point <- df[, idx.pt.real, with = FALSE]
  names(df.point) <- str_trim(point_label, side = "both")

  list.of.point.tall <- lapply(seq_along(levels(fct)), function(i){
    level <- levels(fct)[i]

    idx.each.level <- which(point_group == level)
    df.pt <- df.point[, idx.each.level, with = FALSE]

    df.id.pt <- cbind(df.id, df.pt)

    df.id.pt %>%
      pivot_longer(
        .,
        cols = names(df.pt),
        names_to = "point",
        values_to = level
      ) %>% drop_na(level) %>% wafer::clean_ids(.) # to save space
  })

  # df.pt.tall.joined <-
  #   d8ahelper::multi_join(list.of.point.tall, by = c("startlot", "waferid", "point"))

  multi_join2 <- function (list, by = NULL, join = dplyr::full_join, ...)
  {
    Reduce(function(x, y, ...) {
      dplyr::full_join(x, y, ...)
    }, list)
  }

  df.pt.tall.joined <- multi_join2(list.of.point.tall, by = c("startlot", 'waferid', 'point'))

  df.remain <- df[ , setdiff(seq_along(df), idx.pt.real), with = FALSE]

  if (return_list == TRUE){
    return(list("point.tall" = df.pt.tall.joined,
                "residual" = df.remain))
  }

  return(df.pt.tall.joined)

}

point_to_wafer <- function(project){
  #' wrapper funciton for wide_to_tall and sum_pt_to_wf, with file manipulations

  load_folder <- here::here("data", project, "downloads")

  file.list <-
    d8ahelper::load_files(load_folder,
                          # pattern = "_p.csv",
                          avoid = "_remove_pt",
                          load = TRUE)

  point.tall.list <-
    d8ahelper::lapply_preserve_names(file.list, function(file){

      df <- file[[1]]

      #set criteria for if it is point level data
      if (sum(stringr::str_detect(names(df), "- P[[:digit:]]+")) > 3) {

        print(names(file))

        #action
        result <- wide_to_tall(df, return_list = TRUE)
      } else {
        return(NA)
      }

      if (all(is.na(result))) return(NA)

      if (is.data.frame(result[["residual"]])){
        d8ahelper::save_csv(result[["point.tall"]],
                            file.name = glue::glue("{names(file)}.csv"),
                            path = file.path(load_folder, "point_tall"),
                            overwrite = TRUE)
      }

      if (is.data.frame(result[["residual"]])){
        d8ahelper::save_csv(result[["residual"]],
                            file.name = glue::glue("{names(file)}_remove_pt.csv"),
                            path = load_folder,
                            overwrite = TRUE)
      }

      return(result[["point.tall"]])
    })

  names(point.tall.list) <- names(file.list)
  point.tall.list <- point.tall.list[sapply(point.tall.list, is.list)]

  #action
  pt.sum.to.wafer.list <- lapply(point.tall.list, sum_pt_to_wf)

  #archive original
  archive_to <- file.path(load_folder, "archive")
  dir.create(archive_to, showWarnings = FALSE)

  for (i in seq_along(pt.sum.to.wafer.list)){
    file.copy(
      from = file.path(load_folder, glue::glue("{names(pt.sum.to.wafer.list[i])}.csv")),
      to = file.path(archive_to, glue::glue("{names(pt.sum.to.wafer.list[i])}.csv"))
    )

    file.remove(file.path(load_folder, glue::glue("{names(pt.sum.to.wafer.list[i])}.csv")))
  }

  d8ahelper::save_csv_from_a_list(pt.sum.to.wafer.list,
                                  path = file.path(load_folder),
                                  overwrite = TRUE)

  return(pt.sum.to.wafer.list)
}

sum_die_to_wf <- function(df) {

  # guard clause
  if (nrow(df) <= 1) return(NA)

  col.id <- wafer::detect_all(df, return_col_names = TRUE)
  row_per_wf_max <-
    max(df %>% group_by_at(col.id) %>% summarise(n = n()) %>% ungroup() %>% select(n))

  if ((any(str_detect(names(df), "Die[X|Y]")) && row_per_wf_max > 3) == TRUE) {

    df[, which(str_detect(names(df), "Die[X|Y]"))] <- NULL

    if (!detect_startlot(df)) {
      df <- wafer::create_startlot(df)
    }

    df <- sum_pt_to_wf(df)
    return(df)
  } else {
    return(NA)
  }
}

die_to_wafer <- function(project){
  #' wrapper function for sum_die_to_wf, with file manipulations
  #'
  load_folder <- here::here("data", project, "downloads")

  file.list <-
    d8ahelper::load_files(load_folder,
                          # pattern = "_p.csv",
                          avoid = "_remove_pt",
                          load = TRUE)

  #action
  die.summarized <- lapply_preserve_names(file.list, function(file) {
    df <- file[[1]]
    sum_die_to_wf(df)
  })
  names(die.summarized) <- names(file.list)
  die.summarized <- die.summarized[sapply(die.summarized, is.list)]


  #archive original
  archive_to <- file.path(load_folder, "point_tall")
  dir.create(archive_to, showWarnings = FALSE)

  for (i in seq_along(die.summarized)){
    file.copy(
      from = file.path(load_folder, glue::glue("{names(die.summarized[i])}.csv")),
      to = file.path(archive_to, glue::glue("{names(die.summarized[i])}.csv"))
    )

    file.remove(file.path(load_folder, glue::glue("{names(die.summarized[i])}.csv")))
  }


  d8ahelper::save_csv_from_a_list(die.summarized,
                                  path = file.path(load_folder),
                                  overwrite = TRUE)

  return(die.summarized)
}

#'if trackout file exist in project downloads folder, transform it to a lag step time deltas file
#'wrapper function of get_time_deltas
#'TODO:add option to sum up to lot level first?

add_time_deltas_to_proj <- function(project, file_re='trackout', lag = 1:5, ...){

  if(!hasArg(project)){
    project = pick_project()
  }

  load_from <- here::here("data",
                          project,
                          "downloads")

  file_list <- load_files(load_from, full.names = F)
  matches <- sapply(file_list, function(x)grepl(file_re, x))

  if (sum(matches) !=1) {stop("no matching datetime file or multiple exist")}

  file_name <- file_list[matches]

  df <- load_csv(file=file_name, path = load_from)

  time_delta <- get_time_deltas(df, ...)

  datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M")
  save_as <- glue::glue("cust_{datetime}_time_deltas_lag_{min(lag)}_{max(lag)}.csv")

  data.table::fwrite(time_delta, file = file.path(load_from, save_as))

}

#' impute missing values with lot level mean

impute_missing_num <- function(df){

  if (!wafer::detect_startlot(df)){
    stop("does not detect any startlot columns")
  }

  replace_na_w_mean <- function(x) {
    if_else(condition = is.na(x),
            true = mean(x, na.rm = T),
            false = as.numeric(x))
  }

  df1 <- df %>%
    group_by(startlot) %>%
    mutate(across(where(is.numeric), replace_na_w_mean))
}

#' project based workflow to impute missing values with lot level mean

impute_missing_num_to_proj <- function(project, file_re='sem|thk|scatter', ...){

  if(!hasArg(project)){
    project = pick_project()
  }

  load_from <- here::here("data",
                          project,
                          "downloads")

  file_list <- load_files(load_from, full.names = F)
  matches <- sapply(file_list, function(x)grepl(file_re, x))

  if (sum(matches) < 1) {stop("no matching file exist")}

  file_name <- file_list[matches]

  for (file in file_name){
    df <- load_csv(file=file, path = load_from)

    df <- wafer::clean_ids(df)

    df1 <- impute_missing_num(df, ...)

    str1 <- stringr::str_extract(file, glue::glue(".*({file_re})"))
    str2 <- stringr::str_extract(file, glue::glue("(?<=({file_re})).*"))

    save_as <- paste0(str1, "_ll_impute", str2)

    data.table::fwrite(df1, file = file.path(load_from, save_as))
  }
}

#' convert a lot id per step file to mother/child/none labels
lotid_to_mc_by_proj <- function(project, file_re='split_lot') {

  if(!hasArg(project)){
    project <- pick_project()
  }

  load_from <- here::here("data",
                          project,
                          "downloads")

  file_list <- load_files(load_from, full.names = F)
  matches <- sapply(file_list, function(x)grepl(file_re, x))

  if (sum(matches) !=1) {stop("no matching datetime file or multiple exist")}

  file_name <- file_list[matches]

  df <- load_csv(file=file_name, path = load_from)

  label.col <- wafer::detect_all(df, return_loc = TRUE)
  lotid.col <- setdiff(seq_along(df), label.col)

  df.label <- df[, label.col, with = FALSE]
  df.lotid <- df[, lotid.col, with = FALSE]

  df.convert <- dplyr::as_tibble(df.lotid) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~case_when(stringr::str_detect(.x, ".002")~"mother",
                                           stringr::str_detect(.x, "\\.")~"child",
                                           TRUE~"[missing]")))

  result <- cbind(as.data.frame(df.label), df.convert)

  datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M")
  save_as <- glue::glue("cust_{datetime}_split_mother_child.csv")
  d8ahelper::save_csv(df=result,file.name = save_as,path=load_from)
}


workflow_pre_proc <- function(project){

  load_from <- here::here("data",
                          project,
                          "downloads")

  # clean file
  dfl.1 <- load_files_clean(load_from = load_from)
  d8ahelper::save_csv_from_a_list(dfl.1,
                                  path = load_from,
                                  overwrite = TRUE)


  # die to wafer
  file.list <- dfl.1[!grepl("_remove_pt", names(dfl.1))]

  die.summarized <- lapply_preserve_names(file.list, function(file) {
    df <- file[[1]]
    sum_die_to_wf(df)
  })
  names(die.summarized) <- names(file.list)
  die.summarized <- die.summarized[sapply(die.summarized, is.list)]

  archive_to <- file.path(load_from, "point_tall")
  dir.create(archive_to, showWarnings = FALSE)

  for (i in seq_along(die.summarized)){
    file.copy(
      from = file.path(load_from, glue::glue("{names(die.summarized[i])}.csv")),
      to = file.path(archive_to, glue::glue("{names(die.summarized[i])}.csv"))
    )

    file.remove(file.path(load_from, glue::glue("{names(die.summarized[i])}.csv")))
  }


  d8ahelper::save_csv_from_a_list(die.summarized,
                                  path = file.path(load_from),
                                  overwrite = TRUE)


  # point to wafer
  point.tall.list <-
    d8ahelper::lapply_preserve_names(file.list, function(file){

      df <- file[[1]]

      #set criteria for if it is point level data
      if (sum(stringr::str_detect(names(df), "- P[[:digit:]]+")) > 3) {

        result <- wide_to_tall(df, return_list = TRUE)
      } else {
        return(NA)
      }

      if (all(is.na(result))) return(NA)

      if (is.data.frame(result[["residual"]])){
        d8ahelper::save_csv(result[["point.tall"]],
                            file.name = glue::glue("{names(file)}.csv"),
                            path = file.path(load_from, "point_tall"),
                            overwrite = TRUE)
      }

      if (is.data.frame(result[["residual"]])){
        d8ahelper::save_csv(result[["residual"]],
                            file.name = glue::glue("{names(file)}_remove_pt.csv"),
                            path = load_from,
                            overwrite = TRUE)
      }

      return(result[["point.tall"]])
    })

  names(point.tall.list) <- names(file.list)
  point.tall.list <- point.tall.list[sapply(point.tall.list, is.list)]

  pt.sum.to.wafer.list <- lapply(point.tall.list, sum_pt_to_wf)

  archive_to <- file.path(load_from, "archive")
  dir.create(archive_to, showWarnings = FALSE)

  for (i in seq_along(pt.sum.to.wafer.list)){
    file.copy(
      from = file.path(load_from, glue::glue("{names(pt.sum.to.wafer.list[i])}.csv")),
      to = file.path(archive_to, glue::glue("{names(pt.sum.to.wafer.list[i])}.csv"))
    )

    file.remove(file.path(load_from, glue::glue("{names(pt.sum.to.wafer.list[i])}.csv")))
  }

  d8ahelper::save_csv_from_a_list(pt.sum.to.wafer.list,
                                  path = file.path(load_from),
                                  overwrite = TRUE)


}

#' helper function for lookup terms and definitions
#'
#' @example
#' lookup("BL")
lookup <- function(term){

  term_cln <- d8ahelper::trim_spaces(term)

  #trying to not lowercase bins like "mS" and "MS"
  if (length(strsplit(term_cln, " ")[[1]]) > 1) {
    term_cln <- tolower(term_cln)
  }

  list_of_df <- d8ahelper::load_files(load_from = here::here("r_output/lookup_definitions"), load = T)
  df <- do.call(rbind, list_of_df)

  which_row <- sapply(df$term, function(x){
    x <- d8ahelper::trim_spaces(unlist(strsplit(x, ",")))

    which_one_has_more_than_one_word <-
      sapply(x, function(x) length(strsplit(x, " ")[[1]])) > 1

    x[which_one_has_more_than_one_word] <- tolower(x[which_one_has_more_than_one_word])

    !is.na(match(term_cln, x))
  })

  if (sum(which_row) > 1){

    all_results <- paste(df$term[which_row], collapse = "\n")
    return(glue::glue("multiple matches found:
                        {all_results}"))
  }

  if (sum(which_row) == 0){
    which_aka_row <- sapply(df$aka, function(x){
      x <- d8ahelper::trim_spaces(unlist(strsplit(x, ",")))

      which_one_has_more_than_one_word <-
        sapply(x, function(x) length(strsplit(x, " ")[[1]])) > 1

      x[which_one_has_more_than_one_word] <- tolower(x[which_one_has_more_than_one_word])

      !is.na(match(term_cln, x))
    })

    if (sum(which_aka_row) == 0){
      return(glue::glue("no definition for '{term}' found :("))
    }

    if (sum(which_aka_row) > 1){

      all_results <- paste(df$term[which_aka_row], collapse = "\n")
      return(glue::glue("multiple matches found:
                        {all_results}"))
    }

    which_row <- which(which_aka_row)
  }

  if (length(strsplit(term, " ")[[1]]) > 1){
    term_print <- d8ahelper::cap_str(df$term[which_row])
  } else {

    term_print <- term
  }

  content <- df$definition[which_row]

  final <- glue::glue(
    "Term: {term_print}
     AKA: {df$aka[which_row]}

    \"{content}\""
  )

  return(final)
}



# 6.project oriented workflows ----------------------------------------------


init_proj <- function(project = readline("Enter project name:")) {
  #'initiate a project folder with all necessary subfolders

  dirs <- list(
    dir.root        = here::here('data'),
    dir             = here::here('data', project),
    dir.bat         = here::here('data', project, 'batch'),
    dir.bat.y3      = here::here('data', project, 'batch', 'yield3'),
    dir.bat.f2dd    = here::here('data', project, 'batch', 'f2dd'),
    dir.bat.f2dd.r  = here::here('data', project, 'batch', 'f2dd', 'R'),
    dir.dl          = here::here('data', project, 'downloads'),
    dir.dl.a        = here::here('data', project, 'downloads', 'archive'),
    dir.gql         = here::here('data', project, 'gql'),
    dir.gql         = here::here('data', project, 'gql', 'archive'),
    dir.wd          = here::here('data', project, 'working_dir'),
    dir.wd.a        = here::here('data', project, 'working_dir', 'raw'),
    dir.wd.fs       = here::here('data', project, 'working_dir', 'feature_importance'),
    dir.wd.rst      = here::here('data', project, 'working_dir', 'feature_importance', 'results'),
    dir.wd.rst.a    = here::here('data', project, 'working_dir', 'feature_importance', 'results', 'archive'),
    dir.wd.fs.a     = here::here('data', project, 'working_dir', 'feature_importance', 'archive'),
    dir.wd.r        = here::here('data', project, 'working_dir', 'archive')
  )


  sapply(dirs,
         function(dir)
           dir.create(dir, showWarnings = FALSE))

  return(dirs)
}

#'a wrapper function to clear_dir
#'given a project, clear download and feature_importance folders of all ".csv" extension files except for exceptions
#'move folder into "_archive" directory
#'everything in the "archive" sub dirs will be deleted
clear_proj <- function(project) {
  if (!hasArg(project)){
    project <- pick_project()
  }


  answer <- askYesNo(glue::glue("You are about to clear project:

                                 {project}

                                 All csv files will get deleted permanently

                                 Are you sure that you want to continue?"))

  if (answer != TRUE|is.na(answer)) stop("Stopped.")

  contain.archive <- grepl("archive", project)
  contain.underscore <- grepl("^_", project)
  need_to_stop <- is.na(project)|is.null(project)|contain.archive|contain.underscore

  if(need_to_stop == TRUE) {
    stop("project selected is empty, or contain either '_' or 'archive'")
  }

  dir.root <- here::here("data")
  dir <- here::here("data", project)

  dir_dl <- here::here("data", project, "downloads")
  dir_fi <- here::here("data", project, "working_dir", "feature_importance")

  all.dirs <- list.dirs(here::here("data", project))
  dir.archive <- all.dirs[grepl("archive", list.dirs(here::here("data", project)))]

  clear_dir(dir_dl, recursive = T)
  clear_dir(dir_fi, recursive = F, pattern = ".csv|.bat")

  sapply(dir.archive, function(x){clear_dir(x, recursive = T, pattern = ".csv|.bat")})

  unlink(file.path(here::here("data", "_archive"), project), recursive = T)
  dir.create(path = file.path(here::here("data", "_archive"), project), showWarnings = F)

  file.copy(from = file.path(here::here("data", project)),
            to = file.path(here::here("data", "_archive")),
            recursive = T)

  unlink(file.path(here::here("data", project)), recursive = T)

}

#'read and format lot list from clipboard to save as .gql and .csv files within a project folder

pick_project <- function(){
  projs <- list.dirs(path=here::here("data"), recursive = FALSE, full.names = FALSE)

  project <- select.list(projs,
                         preselect = NULL,
                         multiple = FALSE,
                         title = "Select a Project:")

  return(project)
}

list_proj_downloads <- function(project,
                                pattern =  ".csv$",
                                return_df = FALSE,
                                recursive = FALSE){

  if(!hasArg(project)){
    project <- pick_project()
  }

  lf <- insp_dir(here::here("data", project, "downloads"),
                 recursive = recursive,
                 pattern = pattern)

  print(paste0(lf$names, " ~ ", lf$size_mb, "MB", sep = " "))

  if (return_df == TRUE){
    return(lf)
  }
}

list_proj_fs <- function(project,
                         pattern =  ".csv$",
                         return_df = FALSE,
                         recursive = FALSE){

  if(!hasArg(project)){
    project <- pick_project()
  }

  lf <- insp_dir(here::here("data", project, "working_dir", "feature_importance"),
                 recursive = recursive,
                 pattern = pattern)

  print(paste0(lf$names, " ~ ", lf$size_mb, "MB", sep = " "))

  if (return_df == TRUE){
    return(lf)
  }
}

load_proj_downloads <- function(project, file){
  if(!hasArg(project)){
    project <- pick_project()
  }

  if(!hasArg(file)){
    lf.short <- list_proj_downloads(project = project, return_df = T)$names

    if (length(lf.short) == 0) {
      message("no files found.")
      return(NA)
    }

    file <- select.list(lf.short)
  }

  message(glue::glue("project = '{project}',
                      file = '{file}'"))

  d8ahelper::load_csv(file = file, path = glue::glue("data/{project}/downloads"))
}

load_proj_fs <- function(project, file){
  if(!hasArg(project)){
    project <- pick_project()
  }

  if(!hasArg(file)){
    lf.short <- list_proj_fs(project = project, return_df = T)$names

    if (length(lf.short) == 0) {
      message("no files found.")
      return(NA)
    }

    file <- select.list(lf.short)
  }

  message(glue::glue("project = '{project}',
                      file = '{file}'"))

  d8ahelper::load_csv(file = file, path = glue::glue("data/{project}/working_dir/feature_importance"))
}

add_label_col_to_proj <- function(project, to_col, from_col, label_file_name= "gb.csv"){
  if (!hasArg(project)){
    project <- pick_project()
  }

  if (!hasArg(to_col)){
    to_col <- readline("Label column name:")
  }

  if (!hasArg(from_col)){
    df <- d8ahelper::load_csv(file=label_file_name,path = glue::glue("data/{project}"))

    id_col <- wafer::detect_all(df, return_loc = T)
    col_sel <- names(df)[setdiff(seq_along(df), id_col)]

    from_col <- select.list(col_sel,
                            preselect = NULL,
                            multiple = FALSE,
                            title = glue::glue("Select a column to set as '{to_col}' column:"))

    df[[to_col]] <- df[[from_col]]

    df <- d8ahelper::move_left(df, c(names(df)[id_col],to_col, from_col))
  }

  d8ahelper::save_csv(
    df,
    file.name = label_file_name,
    path = glue::glue("data/{project}"),
    overwrite = TRUE
  )
}



# 7.to_d8ahelper_eventually -------------------------------------------------

#' any_dups hitting error reference SO#27980835 https://stackoverflow.com/questions/27980835/r-data-table-works-in-direct-call-but-same-function-in-a-package-fails

#' Output summary of duplicated rows by given keys
#' @param keys a character vector contains column names to be used for grouping
#' @return a list, of which 'dups' is a subset dataframe contain only duplicated entries by group, 'dup_count' is a count summary by group, 'dup_row' is row index of duplicated rows by group, and 'dup_row_bool' is boolean of duplicated rows by group
any_dups <- function(df, keys) {

  df1 <- data.table::as.data.table(df)[, N := .N, by = keys]

  df2 <- df1[N > 1, .SD, .SDcols = keys]

  bool_list <- lapply(seq_along(df2), function(i) {
    col_name <- names(df2)[[i]]
    df[[col_name]] %in% df2[[i]]
  })

  dup_row_bool <- Reduce(`&`, bool_list)
  dup_row <- which(dup_row_bool)

  dups <- df[dup_row_bool,]

  dup_count <- df[dup_row_bool, keys, with = FALSE]
  dup_count <- dup_count[, N := .N, by = keys]

  dup_count <- dup_count[dup_count[, .I[max(N)], by=keys]$V1] #getting row index of group max in N

  list(
    key = keys,
    dups = dups,
    dup_count = dup_count,
    dup_row = dup_row,
    dup_row_bool = dup_row_bool
  )
}

#' Remove duplicated rows by leaving only 1 observation per each group of most non-missing data columns
#' if multiple observations sharing equal number of most non-missing data columns, choose the first one by row index
#' a wrapper function of d8ahelper::any_dups
#' @param keys a character vector contains column names, for grouping purposes
rm_dups_w_less_data <- function(df, keys){
  dups <- any_dups(df, keys = keys)

  dt <- data.table::as.data.table(df)

  dt.uniq <- dt[!dups$dup_row_bool, ]

  dt.dup <- dups$dups

  not_missing <- sapply(dt.dup[, -keys, with=FALSE], function(col){
    is.na(col)|col==""
  })
  sums <- apply(not_missing, 1, sum) #faster than using apply row-wise with test

  #if multiple max exist, return first one
  .first_max <- function(...){
    if (!is.numeric(c(...))){
      stop('only accept numerics')
    }

    bool <- c(...) == max(...)

    if (sum(bool) > 1){
      bool[which(bool)[-1]] <- FALSE
    }

    return(bool)
  }

  dt.dup$.sums <- sums

  dt.dup <- dt.dup[dt.dup[, .I[.first_max(.sums)], by=keys]$V1]

  dt.dup[, .sums:=NULL]

  message(glue::glue("{nrow(dups$dups)-nrow(dt.dup)} rows with duplicated keys({paste(dups$key, collapse=',')}) removed."))

  rbind(dt.uniq, dt.dup)

}


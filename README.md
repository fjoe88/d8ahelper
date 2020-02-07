# d8ahelpeR

###### A set of convenience functions around data manipulation (import/formatting/transform/summarize/visualize).

![d8ahelpeR](./IMAGES/logo.jpg)


### [Programming](https://github.com/fjoe88/d8ahelper/blob/master/R/programming.R)

- `lapply_preserve_names`

### [Formatting](https://github.com/fjoe88/d8ahelper/blob/master/R/formatting.R)

- `format_to_percentage`
- `format_num`
- `format_datetime`

### [Summarize](https://github.com/fjoe88/d8ahelper/blob/master/R/summarize.R)

- `headtail`
- `unique_row`
- `sum_table`
- `sum_col`
- `sum_missing`

### [Transform](https://github.com/fjoe88/d8ahelper/blob/master/R/transform.R)

- `trim_spaces`
- `insert_nas`
- `move_left`
- `add_datehour`
- `add_wmy`
- `subset_by_quantile`
- `add_empty_rows`
- `convert_time_to_chr`

#### join

- `coalesce_join`
- `multi_join`

#### redundant rows/columns

- `rm_single_unique_col`
- `remove_empty_rows`

#### NAs / missing values

- `remove_duplicates`
- `fill_na_as_missing`
- `fill_missing_as_na`

#### column names encode/decode
- `encode_col`
- `decode_col`
- `get_name`
- `get_id`

### [Transfer](https://github.com/fjoe88/d8ahelper/blob/master/R/transfer.R)

- `copy_unique`
- `copy_as_sql`
- `copy_as_sql_like`
- `save_csv`
- `load_csv`
- `from_excel`
- `write_fwf`
- `fread2`
- `load_files`
- `gen_key`
- `encrypt`
- `decrypt`
- `open_encrypt`

### [Visualize](https://github.com/fjoe88/d8ahelper/blob/master/R/visualize.R)

- `multiplot`
- `plot_boxplot`
  - boxplot with confidence intervals and summary statistics such as p-val and risk

### [Modelling](https://github.com/fjoe88/d8ahelper/blob/master/R/modelling.R)

- `filter_col_unique`
  - wraper function to caret::nearZeroVar to filter chr columns for Zero- and Near Zero-Variance Predictors
- `create_dummy`
  - wraper function to use caret::dummyVars to create dummy variables from character variables
- `corr_to_df`
  - filter high corr features and output in tidy format
- `remove_high_corr_features`
  - wraper function ofcaret::findCorrelation: find and remove highly correlated features
- `model_lm`
  - run lm model and print a curated summary output

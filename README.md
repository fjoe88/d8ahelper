# d8ahelpeR

*2019-12-26*

*zhoufang*

![d8ahelper](./IMAGES/d8ahelper.png)


*A set of convenience functions around data manipulation.*


## Modules

### [Programming](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/programming.R)

`lapply_preserve_names` base R `lapply` wrapper since it give access to only the element of the vector but not other attributes

`list_files_fwd_slash` wraper function list.files to allow coppied windows path format containing backward slashes

`is_fct_or_chr`

### [Formatting](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/formatting.R)

`format_to_percentage` convert integer/floating point number format to percentage format

`format_num` format numbers to limit digits after decimal point

`format_datetime` format datetime columns to a specified format

### [Summarize](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/summarize.R)

`headtail` a quick glance into a dataframe combining n head and tail rows

`get_mode` get the mode of a vector

`ff_quantile` function factory to create nth quantile function, such as ```q5 <- ff_quantile(0.05)```

`unique_row` extract unique rows combinations and show row counts

`sum_table` given 2 columns, summarize counts of each unique value-pair combinations
  
`sum_col` summarize a data frame with concise and useful summary statistics

`sum_row` generate row-wise summary for missing and unique values

`sum_df` sister function to sum_col and sum_row, returns a list of results from each

`sum_missing` summary statistics of top n missing value columns

### [Transform](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/transform.R)

`trim_spaces` remove leading and trailing white spaces

`move_left` move column or columns to far left hand side of a dataframe

`add_wmy` add week, weekday, month, year columns based on datetime column

`subset_by_quantile` filter column by removing points beyond top and(or) bottom percentage thresholds

`add_empty_rows` append empty rows to a dataframe to make row number to a target number

`insert_nas` insert(inject) NAs as replacement randomly to a data.frame

#### Join

`coalesce_join` coalescely join x, y data frames, for columns of same names, append y values to x if rows that are missing value

`multi_join` wrapper function to coalesce_join, join together a list of data frames

#### Redundant rows/columns

`rm_single_unique_col` remove column(s) if containing a single unique value

`remove_empty_rows` remove duplicated rows of the original data frame, or a subset of if column names being passed in

#### NAs / missing values

`rm_na` remove NAs given a vector

`fill_na_as_missing` fill NAs and empty cells with fillers such as a character

`fill_missing_as_na` sister function to fill_na_as_missing, replace cells with NAs if match to certain string

#### Column names encode/decode

`encode_col` replace column names with alpha-numeric sequences, returns a named vector (of name-value pairs) for column name look-up; Returns a list.

`decode_col` sister function to encode_col to revert encoded list of dataframes

`get_name` sister function to encode_col, retrieve column names based on id(s)

`get_id` sister function to encode_col, retrieve column id based on name(s)

#### Regular Expressions

`str_find` wrapper function for locate a string based on regex leading to and after it

### [Transfer](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/transfer.R)

`copy_unique` copy unique values to clipboard

`save_csv` a custom way to output CSV files

`save_csv_from_a_list` a wrapper function of save_csv that applies to a list of data frames

`load_csv` a custom way to load CSV files

`from_excel` wrapper function for convinient copy from Excel into a data frame ('Trick' via @SuzanBaert on twitter)

`write_fwf` 

`fread2` wrapper function to data.table::fread to convert blank cells to NA at reading

`load_files` load files paths in specified location

#### Encrypt/decrypt

`gen_key` a wrapper function for `sodium::keygen` to generate, convert keys

`encrypt` a wrapper function to encrypt file using a key

`decrypt` a wrapper function to decrypt file using a key

`open_encrypt` a wrapper function for the workflow of decrypt, source and encrypt back files

### [Visualize](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/visualize.R)

`multiplot`

`plot_boxplot` boxplot with confidence intervals and summary statistics such as p-val and risk

### [Modelling](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/modelling.R)

`corr_to_df` filter high corr features and output in tidy format

`remove_high_corr_features` wraper function to caret::findCorrelation: find and remove highly correlated features

`model_lm` run lm model and print a curated summary output


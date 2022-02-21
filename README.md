# d8ahelpeR

*2019-12-26*

*zhoufang*

![d8ahelper](./IMAGES/d8ahelper.png)


*A set of convenience functions around data manipulation.*


## Modules

### [Programming](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/programming.R)

**`lapply_preserve_names`** base R `lapply` wrapper since it give access to only the element of the vector but not other attributes
```R
> foo = list("fruit1"="apple", "fruit2" = "banana", "num" = c(1, 2))
> lapply(foo, function(x)print(names(x)))
NULL
NULL
NULL
...
> lapply_preserve_names(foo, function(x)print(names(x)))
[1] "fruit1"
[1] "fruit2"
[1] "num"
```
`list_files_fwd_slash` wraper function list.files to allow coppied windows path format containing backward slashes

`is_fct_or_chr` character type column identifier

### [Summarize](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/summarize.R)

`headtail` a quick glance into a dataframe combining n head and tail rows

`get_mode` get the mode of a vector

`ff_quantile` function factory to create nth quantile function, such as ```q5 <- ff_quantile(0.05)```

`unique_row` extract unique rows combinations and show row counts

`sum_table` given 2 columns, summarize counts of each unique value-pair combinations
  
**`sum_col`** summarize a data frame with concise and useful summary statistics

**`sum_row`** generate row-wise summary for missing and unique values

`sum_df` sister function to sum_col and sum_row, returns a list of results from each

**`sum_missing`** summary statistics of top n missing value columns

`contain_value` examine if a data holder contains 'value' in a pre-defined term

`any_dups` output summary of duplicated rows by given keys

### [Transform](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/transform.R)

#### General

`trim_ws` remove leading and trailing white spaces

`trim_ws_df` remove leading and trailing whitespaces that are within any character or factor type columns of a data.frame

**`move_left`** move column or columns to far left hand side of a dataframe

`add_wmy` add week, weekday, month, year columns based on datetime column

`subset_by_quantile` filter column by removing points beyond top and(or) bottom percentage thresholds

`add_empty_rows` append empty rows to a dataframe to make row number to a target number

**`puff_my_df`** artificially increase the number of rows to make up any missing combinations by label columns for every unique id, functions similarly to tidyr::crossing and tidyr::expand.grid.

```R
> foo <- data.frame(
+     "name"   = c("Rachel", "Chandler", "Rachel", "Monica", "Rachel", "Chandler"),
+     "year"   = c(2010, 2010, 2012, 2010, 2011, 2012),
+     "size"  =  c("XS", "S",  "M", "S", "S", "M"),
+     "results" = c(1,2,3,4,5,6)
+ )

#new rows introduced with 'weights' of NA
> puff_my_df(foo, "name", c("year", "size"))
       name year size results
1    Rachel 2010   XS       1
3    Rachel 2012    M       3
5    Rachel 2011    S       5
11   Rachel 2010    S      NA
2  Chandler 2010    S       2
6  Chandler 2012    M       6
12 Chandler 2010   XS      NA
21 Chandler 2011    S      NA
4    Monica 2010    S       4
13   Monica 2010   XS      NA
22   Monica 2012    M      NA
31   Monica 2011    S      NA
```

#### Join

**`coalesce_join`** coalescely join x, y data frames, for columns of same names, append y values to x if rows that are missing value

**`multi_join`** wrapper function to coalesce_join, join together a list of data frames

#### Redundant rows/columns

`rm_single_unique_col` remove column(s) that contain a single unique value

`remove_empty_rows` remove duplicated rows of the original data frame, or a subset of if column names being passed in

**`rm_dups_w_less_data`** remove duplicated rows by leaving only 1 observation per each group of most non-missing data columns, if multiple observations sharing equal number of most non-missing data columns, choose the first one by row index. A wraper function of `any_dups`.

#### NAs / missing values

`insert_nas` insert NAs as replacement randomly to a data

`rm_na` remove NAs given a vector

`remove_duplicates`

`fill_na_as_missing` fill NAs and empty cells with fillers such as a character

`fill_as_na` sister function to fill_na_as_missing, replace cells with NAs if match to given string

**`clean_by_id`** remove rows where id columns are all missing, and rows where all columns but the id columns are missing; (optional) Remove columns where all rows are of missing values.

#### Column names encode/decode

`encode_col` replace column names with alpha-numeric sequences, returns a named vector (of name-value pairs) for column name look-up; Returns a list.

`decode_col` sister function to encode_col to revert encoded list of dataframes

`get_name` sister function to encode_col, retrieve column names based on id(s)

`get_id` sister function to encode_col, retrieve column id based on name(s)

#### Regular Expressions

**`str_find`** wrapper function for locate a string based on regex leading to and after it

### [Transfer](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/transfer.R)

`copy_unique` copy unique values to clipboard

`save_csv` a custom way to output CSV files

`save_csv_from_a_list` a wrapper function of save_csv that applies to a list of data frames

`load_csv` a custom way to load CSV files

`from_excel` wrapper function for convenient copy from Excel into a data frame ('Trick' via @SuzanBaert on twitter)

`write_fwf` write fixed width format text file

`fread2` wrapper function to data.table::fread to convert blank cells to NA at reading

`load_files` load files paths in specified location

`file.copy.content.only` to enable selective file copy to another directory, base R's file.copy function can only copy a entire directory to another directory

`insp_dir` function to ferform a non intrusive inspection of a given directory and return a dataframe with essential file information sorted by file size

`clear_dir` given a directory, delete files given the regex pattern excluding files matching to 'avoid' arg

#### Encrypt/decrypt

`gen_key` a wrapper function for `sodium::keygen` to generate, convert keys

`encrypt` a wrapper function to encrypt file using a key

`decrypt` a wrapper function to decrypt file using a key

`open_encrypt` a wrapper function for the workflow of decrypt, source and encrypt back files


### [Formatting](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/formatting.R)

`format_to_percentage` convert integer/floating point number format to percentage format

`format_num` format numbers to limit digits after decimal point

`format_datetime` format datetime columns to a specified format

`cap_str` capitalize the first letter of each word in the string

`conv_fct_to_chr` convert all columns of factor type to character type

`conv_chr_to_fct` convert all columns of character type to factor type

#### Date and time

`secs_to_date` convert all columns of character type to factor type

`dats_to_date` convert numerically converted format POSIXct time back to POSIXct datetime format

### [Visualize](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/visualize.R)

`multiplot`

`plot_boxplot` boxplot with confidence intervals and summary statistics such as p-val and risk

### [Modelling](https://bitbucket.micron.com/bbdc/users/zhoufang/repos/d8ahelper/browse/R/modelling.R)

#### Pre-processing
`corr_to_df` filter high corr features and output in tidy format

`remove_high_corr_features` wraper function to caret::findCorrelation: find and remove highly correlated features

#### Modeling

`model_lm` run lm model and print a curated summary output

#### Unsupervised ML

`hclust_wss` hiearchical clustering with for within cluster SSs screeplot for easier cluster number selection, a wrapper function of 'fastcluster' package method hclust (for matrix) and hclust.vector(for vector)

`fill_map` a wrapper function to 'gstat::gstat' function to use a univariate or multivariate geostatistical model with input of x,y coordinate and fill z column with predictions, for each id group, `puff_my_df` also written to make all available entries available for each group

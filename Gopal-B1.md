STAT 545 B Assignment B1
================
Gopal Khanal
2023-11-03

# Introduction

This is the first assignment of UBC’s STAT 545 course Part B. This
assignment covers making a function in R, documenting it, and testing
it. This includes four specific exercises. 1. Making a reasonable
function. 2. Documenting the function. 3. Showing the application of the
function with example 4. Demonstrating if the function actually works
with tests.

# Excercise 1 & 2 : Create a function and its documentation

First, I will load the relevant libraries that I’ll need.

``` r
library(datateachr) # <- This  contains that the data flow sample I'm using
library(palmerpenguins) # <- This  contains that the data flow sample I'm using
library(tidyverse)
library(testthat)
library(roxygen2)
library(dplyr)
```

One of the main tasks in data analysis is that we often need to check
how observations of a particular variable vary across other variables.
For example, how blood pressure or hemoglobin content of patients vary
across different treatment groups, which could be age, sex, or other
treatment levels.

While this can be easily done in base R or using `dplyr` package,
creating a `function` could ease the task when one wants to do the
things repeatedly.

In my previous Mini Data Analysis, I had to regularly check and repeat
how **flow** (continuous variable) varied across extreme **flow type**
(categorical variable). This is often the case in other types of
analysis as well when one need to constantly check how observation
values vary across categories or treatment types.

My function `summary.stat`computes summary statistics for a numeric
variable within a data frame, grouped by another variable. The grouping
variable can be categorical, character variable or numeric. Ideally,
it’s better to do such summary for a grouping variable with categorical
or character class. But, users may also want to summarize one numeric
variable across other variable with numeric or integer class. This
function summarizes the value different summary statistics such as mean,
range, standard deviation, of the continuous variable across other
variables.

``` r
# Naming the function as summary.stat
summary.stat <- function(dataset, group, value, na.rm = TRUE) {
  # Make the column names as string so that they are  used checking error and getting for error messages later.
  
  group1 <- deparse(substitute(group))
  value1 <- deparse(substitute(value))
  
  #  Check if there are non-numeric values in the variable that we want to summarize
  
   if(!is.numeric(dataset[[value1]])) {
    stop('Sorry, numeric variable can only be a numeric vector')
   }
  
  # check if there are  NAs in the grouping variable. If yes, NAs will be considered and summarized as separate group.
    if((!is.null(group1) && anyNA(dataset[[group1]]))){
    warning("NAs are found in grouping variable. Consider NAs as a separate group")
  }
  
  # Implement the function
  
  dataset %>%
    group_by({{group}}) %>%
    summarize(
      n = n(), 
      mean = mean({{value}},na.rm = T),
      range= max({{value}},na.rm = T)-min({{value}},na.rm = T),
      stdev = sd({{value}},na.rm = T))
}
#'@Title Get summary statistics of a variable across other variable
#'
#' Description: This function returns a table with the summary statistics such as total number of observations, mean, range and standard deviation of continuous variable across other variable.
#' @param dataset A data frame that should have at least one continuous variable and other grouping variables. This is named to make better sense.
#' @param group A vector which could be of factor, character or numeric or integer. The function will group values of another numeric variable according to this variable to yield summary statistics with new function. This is named as group to make typical sense about the grouping variable.
#' @param value A numeric vector which should have numeric class. I named it value because it will be self evident that this variable is the one for which we have observation/values to summarize. 
#' @param na.rm Logical, indicating whether to remove missing values (default is TRUE).
#'
#' @return A data frame with summary statistics information given by this new function.
#' @export
#'
#' @examples
#' (summary.stat(flow_sample, flow, extreme_type))
#' 
```

# Exercise 3: Showing how function works with examples

## Flow sample data

The **flow_sample** data that I used for Mini Data Analysis had **flow**
as continuous variable and **extreme_type** as categorical variable two
two level. Below I show application of the new function **summary_stat**
to quickly access the summary information of different summary measures
(number of observations, mean, standard deviation and other)

``` r
flow_summary <- summary.stat(flow_sample, extreme_type, flow)

flow_summary
```

    ## # A tibble: 2 × 5
    ##   extreme_type     n   mean  range  stdev
    ##   <chr>        <int>  <dbl>  <dbl>  <dbl>
    ## 1 maximum        109 212.   359    61.7  
    ## 2 minimum        109   6.27   4.82  0.965

## Vancouver tree data

The **Vancouver tree** data has tree information for different
neighborhood. One important measure of tree is diameter, and the tree
**diameter** can vary based on categorical variable
**neighborhood_name**. The **summary.stat** function can quickly
summarise selected tree summary measures for different neighborhoods.

``` r
tree_summary <- summary.stat(vancouver_trees, neighbourhood_name, diameter)
tree_summary
```

    ## # A tibble: 22 × 5
    ##    neighbourhood_name           n  mean range stdev
    ##    <chr>                    <int> <dbl> <dbl> <dbl>
    ##  1 ARBUTUS-RIDGE             5169 11.9   62    8.67
    ##  2 DOWNTOWN                  5159  7.45 150    5.12
    ##  3 DUNBAR-SOUTHLANDS         9415 13.9  305   11.0 
    ##  4 FAIRVIEW                  4002 10.6   97.8  7.98
    ##  5 GRANDVIEW-WOODLAND        6703 11.4   63    9.12
    ##  6 HASTINGS-SUNRISE         10547 11.0  435    9.49
    ##  7 KENSINGTON-CEDAR COTTAGE 11042 11.7   99    8.93
    ##  8 KERRISDALE                6936 12.7  156    9.80
    ##  9 KILLARNEY                 6148 10.1   50    7.49
    ## 10 KITSILANO                 8115 14.4  317   10.8 
    ## # ℹ 12 more rows

## Penguins data set

The **penguins** data has body mass information of different species of
penguins. It is important to get summary information of **body_mass_g**
for different **species** (categorical variable) of penguins.

``` r
penguins_summary <- summary.stat(penguins, species, body_mass_g)
penguins_summary
```

    ## # A tibble: 3 × 5
    ##   species       n  mean range stdev
    ##   <fct>     <int> <dbl> <int> <dbl>
    ## 1 Adelie      152 3701.  1925  459.
    ## 2 Chinstrap    68 3733.  2100  384.
    ## 3 Gentoo      124 5076.  2350  504.

My function wouldn’t have worked if I had specified that my grouping
variable should be of character class because here the class of species
in penguins dataset is `factor`

``` r
# checking the class of species in penguins data set
class(penguins$species)
```

    ## [1] "factor"

### Use of numeric class variable as grouping variable

Not-specifying grouping variable as character or factor works for
numeric or integer class variable. For example, here I have summarized
**drat** across **cyl** which has numeric class.

``` r
class(mtcars$cyl)
```

    ## [1] "numeric"

``` r
mpg_summary <- summary.stat(mtcars, cyl, drat)
mpg_summary
```

    ## # A tibble: 3 × 5
    ##     cyl     n  mean range stdev
    ##   <dbl> <int> <dbl> <dbl> <dbl>
    ## 1     4    11  4.07  1.24 0.365
    ## 2     6     7  3.59  1.16 0.476
    ## 3     8    14  3.23  1.46 0.372

# Exercise 4: Testing the function

Write formal tests for your function. You should use at least three
non-redundant uses of an expect\_() function from the testthat package,
and they should be contained in a test_that() function (or more than
one). They should all pass.

The **function** should work in range of conditions. Below I use package
`testthat` to test if this function actually works in different set of
conditions.

``` r
library(testthat)
```

### Test 1: Check whether the function output matches with the normal calculation

``` r
# Get output from dplyr package and rename the dataframe

data1 <- penguins %>%
  group_by(species) %>%  
  summarize(n=n(),
    mean = mean(body_mass_g, na.rm = TRUE),
    range=max(body_mass_g, na.rm = TRUE)- min(body_mass_g, na.rm=TRUE),
    stdev = sd(body_mass_g, na.rm = TRUE))
  
test_that("Check if function gives correct calculation", {
  expect_equal(summary.stat(penguins, species, body_mass_g), data1) 
  # Check if it matches with the output from dplyr
})
```

    ## Test passed 🎉

### Test 2: Test of incorrect class of input variables in generated data

``` r
# Create sample data
value <- rnorm(100, mean = 0, sd = 1)
group <- sample(c('A', 'B'), 100, replace = TRUE)

# Create a dataframe with character and numeric variables
df <- data.frame(value = value, group = group)

# Display the dataframe
#print(df)

# check error with character class in place of numeric class

#expect_error(summary_stat(df, value, group))

test_that('Test other cases that should give an error', {
  expect_error(summary.stat(df, value, group))  
})
```

    ## Test passed 🥳

### Test 3: Use of incorrect class of input variables.

I’m using penguins dataset to get summary information.

The function will work in a dataset where my grouping variable is of any
class factor, character or numeric, and the summarizing variable should
be of numeric class.

My function `summary.stat` throws an error if my summarizing variable is
of class other than numeric.

In the **vancouver_tree** data street address is non-numeric so it gives
an error.

``` r
result1<-'Sorry, numeric variable can only be a numeric vector'
test_that('Sorry, numeric variable can only be a numeric vector', {
expect_error(summary.stat(vancouver_trees, neighbourhood_name, std_street), result1)
})
```

    ## Test passed 😀

### Test 4: Incorrect naming of input variables.

I used the incorrect name of the summarizing variable from other data
set.

The function should through an error for this.

``` r
# Test of incorrect variable name when variable from other data is used 
test_that('Test of incorrect variable name should give an error', {
  expect_error(summary.stat(flow_sample, 'diameter'))  
})
```

    ## Test passed 🥳

### Test 5: Test when the data contains NA’s. The function will work in that case if NAs are handled by function.

For example, some of the values in penguins dataset has NAs. The
function will handle the NA’s and won’t through error

``` r
# Create a sample dataframe with NAs
test_data <- data.frame(
  species = c("A", "B", "A", "B", "A", "C", "C", "C"),
  body_mass_g = c(50, 60, NA, 70, 80, 90, NA, 30)
)
result2 <- test_data %>%
  group_by(species) %>%  
  summarize(n=n(),
    mean = mean(body_mass_g, na.rm = TRUE),
    range=max(body_mass_g, na.rm = TRUE)- min(body_mass_g, na.rm=TRUE),
    stdev = sd(body_mass_g, na.rm = TRUE))

test_that("Check if function gives correct calculation when the data contains NA's",{
expect_equal(summary.stat(test_data, species, body_mass_g), result2) })
```

    ## Test passed 😀

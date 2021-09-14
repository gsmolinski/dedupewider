
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dedupewider

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dedupewider)](https://CRAN.R-project.org/package=dedupewider)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/dedupewider?color=blue)](https://r-pkg.org/pkg/dedupewider)
<!-- badges: end -->

Duplicated data can exist in different rows and columns and user may
need to treat observations (rows) connected by duplicated data as one
observation, e.g. companies can belong to one family (and thus: be one
company) by sharing some telephone numbers. This package provides a
function to find connected rows based on data on chosen columns and
collapse it into one row.

Function from this package was used in CATI surveys (especially on
businesses databases) to minimize the chance that interviewers will call
independently the same respondent and thus irritate her or him. It is a
chance that the same, suitable person to participate in the survey,
works in more than one company and that these companies exist as a
separate records in the database (sometimes just as a separate
companies, sometimes as a branches). When trying to find participant in
company X, interviewer can be switched to company C to speak with
employee E and the second interviewer, when calling company Y, can also
be switched to company C to speak with employee E. If some data in
database (like phone numbers) can be use to collapse companies X, Y and
C into one record, the chance for this inconvenience will be much lower.

## Installation

You can install the released version of dedupewider from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dedupewider")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gsmolinski/dedupewider")
```

## Usage

``` r
library(dedupewider)

initial_table <- data.frame(tel_1 = c(111, 222, 444, 555),
                            tel_2 = c(222, 666, 666, 555),
                            tel_3 = c(NA, NA, NA, 555),
                            tel_4 = c(NA, NA, NA, 555),
                            tel_5 = c(NA, NA, NA, 555),
                            name = paste0("name", 1:4),
                            nace = c("01.19", "01.64", "55.90", "09.10"))

table_deduplicated <- dedupe_wide(initial_table, cols_dedupe = paste0("tel_", 1:5),
                                  cols_expand = "name")
table_deduplicated
```

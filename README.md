<!-- README.md is generated from README.rmd. Please edit that file -->

This package is designed to help members of the [Kluane Red Squirrel
Project](http://redsquirrel.biology.ualberta.ca/) work with the squirrel
database in R. It provides functions encapsulating common database
queries and helper functions to aid users writing their own bespoke
queries. In all cases, the functions return database records in the form
of R data frames ready for use in further analyses. This package
requires access to the KRSP database and is therefore only of use to
members of the project.

# Installation

``` r
install.packages("devtools")
devtools::install_github("KluaneRedSquirrelProject/krsp")
```

# Use

For a full demonstation of the functions in this package consult the
vignette.

``` r
browseVignettes("krsp")
```

### Changelog

-   Aug 2022: Updates to all functions – connection is now a wrapper
    around `DBI::dbConnect` rather than `dbplyr::src_dbi` which has been
    deprecated since 2019. Some additional helper functions added and
    infrastructure was added for future expansion.

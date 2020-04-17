SEG Shiny Heatmap (version 0.3.3)
================
Martin Frigaard
2020-04-17

# Welcome to the SEG Shiny app project page\!

Document info:

  - **Created date:** 2020-04-17

  - **R version:** R version 3.6.3 (2020-02-29)

***This is not the code for the working app\!***

This page outlines the code used to develop the SEG application. The
working version is on the Diabetes Technology Society website
[here](https://www.diabetestechnology.org/seg/).

The preview version (not necessarily stable) is available
[here](https://mfrigaard.shinyapps.io/seg-shiny-033/).

For questions, issues, or feature requests, please email Martin at
<support@quesgen.com>.

## Required packages

``` r
library(dplyr) # Data wrangling, glimpse(50) and tbl_df().
library(ggplot2) # Visualise data.
library(lubridate) # Dates and time.
library(readr) # Efficient reading of CSV data.
library(stringr) # String operations.
library(tibble) # Convert row names into a column.
library(tidyr) # Prepare a tidy dataset, gather().
library(magrittr) # Pipes %>%, %T>% and equals(), extract().
library(tidyverse) # all tidyverse packages
library(mosaic) # favstats and other summary functions
library(fs) # file management functions
library(shiny) # apps
library(datapasta) # for pasting tibbles
library(styler) # cleaner code
```

## The Research

This application incorporates two lines of research: the use a
surveillance error grid to evaluate blood glucose measurements, and the
accuracy of marketed blood glucose monitors.

### The Surveillance Error Grid

1.  [The surveillance error grid. Klonoff DC, Lias C, Vigersky R, Clarke
    W, Parkes JL, Sacks DB, Kirkman MS, Kovatchev B; ErrorGrid Panel. J
    Diabetes Sci Technol. 2014
    Jul;8(4):658-72](http://journals.sagepub.com/doi/full/10.1177/1932296814539589).

2.  [Computing the surveillance error grid analysis: procedure and
    examples. Kovatchev BP, Wakeman CA, Breton MD, Kost GJ, Louie RF,
    Tran NK, Klonoff DC. J Diabetes Sci Technol. 2014
    Jul;8(4):673-84.](https://journals.sagepub.com/doi/full/10.1177/1932296814539590)

### Blood Glucose Monitor Surveillance:

1.  [Investigation of the Accuracy of 18 Marketed Blood Glucose
    Monitors. David C. Klonoff, Joan Lee Parkes, Boris P. Kovatchev,
    David Kerr, Wendy C. Bevier, Ronald L. Brazg, Mark Christiansen,
    Timothy S. Bailey, James H. Nichols and Michael A. Kohn. Diabetes
    Care 2018
    Aug; 41(8): 1681-1688.](http://care.diabetesjournals.org/content/41/8/1681.long)

### Feature requests, bugs, etc.

Please email `mjfrigaard@gmail.com` for any bugs or issues.

### CHANGELOG

Any updates and recent changes have been documented
[here](https://github.com/mjfrigaard/seg-shiny-0.3.3/blob/master/CHANGELOG.md).

### Software Requirement Specifications (SRS)

[This
document](https://github.com/mjfrigaard/seg-shiny-0.3.3/blob/master/SRS-documentation.md)
outlines the goals and objectives of the SEG shiny application, and
documents the code in great depth.

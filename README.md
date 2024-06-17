# Repository with codes for my Master's thesis "Vector generalized linear models and their selected applications"

## Setup

Install the development version of the necessary packages from CRAN:

``` r
install.packages(c(
  "VGAM", "modelsummary", "tidyverse", "sandwich", "lmtest", 
  "doSNOW", "progress", "gridExtra", "VennDiagram"
))
```

and the `goalmodel` package from GitHub:

```r
remotes::install_github("opisthokonta/goalmodel")
```


## Structure of the repo

-   `codes/`
    -   `heteroscedastic_error_comparison.R` - the code to run simulation from chapter 3 section 1 on hypothesis testing in factorial study designs, runs the code from:
        - `heteroscedastic_errors_functions.R` - with functions required for heteroscedastic consistent error matrixes
        - produces a very large data set (140.2 mb too large for GitHub) with finished results avaiable under: https://drive.google.com/file/d/1uvrizLF-UbyjmhXDfoQot0aOUHAMXQ4_/view?usp=sharing
    -   `football_data.R` - the code for models from chapter 3 section 2 on football data
    -   `apartments.R` - the code to get all models fitted in chapter 4 to capture-recapture data
-   `figures/`
    -   contains all figures used in the thesis
-   `raw_data/`
    -   `processed_data.Rdata`, `processed_data_averages_3.Rdata` raw data files from repository [LukaszChrostowski/Football_Results_Predictions](https://github.com/LukaszChrostowski/Football_Results_Predictions/tree/master/output) used in `football_data.R`
    -   `otodom.csv`, `olx.csv` raw (ish) data used in `apartments.R`
-   `output_data/`
    -   `football_analysis.csv` a cleaned dataset from chapter 3 section 2 on which analysis was performed
    -   `factorial_study_design_test_comparisson.csv` a much smaller csv from `heteroscedastic_error_comparison.R` containing only the essentials

## Grant informaiton

Work on this thesis was supported by the National Science Centre, OPUS 20 grant no. 2020/39/B/HS4/00941.

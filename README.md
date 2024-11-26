# SurvCens

**SurvCens** is an R package for censoring survival data and competing risks, simplifying data preparation for survival analysis.

## Description

The package provides functions for:

- **Censoring survival data** based on a specified cutoff time using the `os_cut` function.
- **Censoring data with competing risks** using the `cmprsk_cut` function.

This allows researchers to efficiently prepare data for survival modeling and competing risks analysis.

## Installation

Install the `SurvCens` package directly from GitHub using:

```R
# Install the devtools package if it's not already installed
install.packages("devtools")

# Install SurvCens from GitHub
devtools::install_github("DarkSynapse/SurvCens")

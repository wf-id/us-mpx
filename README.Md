[![DOI](https://zenodo.org/badge/515973085.svg)](https://zenodo.org/badge/latestdoi/515973085)

# Monkeypox Cases in the United States

This repository saves a copy of the posted reported monkeypox (MPX) cases for each state as recorded by the CDC on [their website](https://www.cdc.gov/poxvirus/monkeypox/response/2022/us-map.html).

## Key Functions

* [scrape.R](https://github.com/wf-id/us-mpx/blob/main/scrape.R) saves a local snapshot of the aggregated cases as a json file in the [data](https://github.com/wf-id/us-mpx/tree/main/data) directory
* [aggregate.R](https://github.com/wf-id/us-mpx/blob/main/aggregate.R) reads the latest snapshots by date and then aggregated the output into the [output/mpx.csv](https://github.com/wf-id/us-mpx/blob/main/output/mpx.csv) directory.

## Key Output

* Cumulative and new daily cases will be available in the [mpx.csv](https://github.com/wf-id/us-mpx/blob/main/output/mpx.csv) file.
The date reflects the last available update. 

## Terms and Conditions

The information on this website is for general informational purposes only and SHOULD NOT be relied upon as a substitute for sound professional medical advice, evaluation or care from your physician or other qualified health care provider.

The Website relies upon publicly available data that do not always agree. 
Authors hereby disclaims any and all representations and warranties with respect to the Website, including accuracy, fitness for use, reliability, completeness, and non-infringement of third party rights.

These terms and conditions are subject to change. 
Your use of the Website constitutes your acceptance of these terms and conditions and any future modifications thereof.

# Human Rights Watch Analysis of Child Welfare Systems in the US
## Quantitative analyses from the report ““If I Wasn’t Poor, I Wouldn’t Be Unfit” The Family Separation Crisis Within the US Child Welfare System"

Code by Brian Root; report authored by Hina Naveed

## Data Sources and Methodology
This repository includes the analytical code that was used in Human Rights Watch's investigation. The analysis is based on three data files:
*The [National Child Abuse and Neglect Data System (NCANDS) Child File](https://www.ndacan.acf.hhs.gov/datasets/datasets-list-ncands-child-file.cfm)
*The [Adoption and Foster Care Analysis and Reporting System (AFCARS) Foster Care File](https://www.ndacan.acf.hhs.gov/datasets/datasets-list-afcars-foster-care.cfm)
*US Census Bureau [American Community Survey](https://www.census.gov/programs-surveys/acs/data.html) 5-year file

The raw data is not provided because the NCANDS data is a restricted use dataset and the AFCARS file requires terms of use to be signed. Both files can be ordered from the [National Archive on Child Abuse and Neglect](https://www.ndacan.acf.hhs.gov/datasets/datasets-list.cfm). For both datasets, we analyzed the fiscal year 2019 file because it was the most recent file with a full year of pre-Covid-19 pandemic data, however the analysis can be replicated using other years of data. Fiscal year 2019 covered the period from October 2018 through September 2019.

The NCANDS data file includes information on the over 4.2 million reports of maltreatment that were investigated by child welfare agencies during the year and the AFCARS file includes data on over 672,000 children who were under the responsibility of state child welfare agencies at some point during the year.  Data from Puerto Rico was not included in the analysis. When discussing our analysis of this data in the report, we do not include the term “fiscal” for clarity and readability, and instead refer to the time period as “year” or “2019”

Depending on the variable analyzed, there can be wide variation in the completeness of data. For certain data points, states that provided incomplete data or outlier data were removed when computing totals or rates.  

All rates were computed using data from the US Census Bureau 2019 5-year American Community Survey (ACS). All child rates used population estimates of the under 18 population. Race-specific rates for the white population used the non-Hispanic/Latino white population. Rates for Latinx population used the Census Bureau’s Hispanic/Latino ethnicity categorization, regardless of race. This methodology matches with the methodology used in the AFCARS and NCANDS datasets on the derived “RaceEthn” variable.  Data on income and poverty levels is from the same ACS survey. 

## Core scripts
This repository provides the R scripts used to process and analyze the data.
*The file 01_childfile_processing.R is used to create new variables that are used in the analysis.
*02_census_download_process.R queries the Census api to download population and other economic/demographic data from the ACS and generate additional percentage variables. In order for the census script to run, you must have a census api key saved in your .Renviron.
*The 03_descriptive_analysis.R script contains all of the analyses used to generate the statistics, tables, and figures used in the report. The actual visualizations were not developed in R but the script generates the dataframes that were used to create the visualizations.


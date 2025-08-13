# Does the type of clause register indicate to readers whether a text is fake or true news?


### A project that handles text processing while filtering for specific clauses and provides visulizations and room for further analysis.

## Intro

Handling text data from two csv files containing true news and the other fake news.

### Script performs the following actions:

Loading csv files

Cleaning out unnecessary metadata

Capturing desired clauses

## Prerequisites:

R version 4.5.1 (2025-06-13 ucrt) -- "Great Square Root"

"tidyr"

"ggplot2"

"dplyr"

"stringr"

"tibble"

"udpipe"

```{r}
install.packages(c("tidyr", "ggplot2", "dplyr", "stringr", "tibble", "udpipe"))
```

# Data source and file format

Access to the data used in this analysis can be fonud here;
https://www.kaggle.com/datasets/clmentbisaillon/fake-and-real-news-dataset 

File structure is as following and should be run in this order;

"00_" indicates a R script for loading libraries and checking version

"01_" indicates a R script that builds the functions used in following scripts

"02_" indicates a R script for processing data

"03_" indicates a R script for filtering clauses

"05_" indicates a RMD file for visualization 


library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(udpipe)

R.version.string
file_path <- ("C:\\Users\\Ethan\\OneDrive\\Cloud_Desktop\\00_UC\\00_2025_S2\\00_LING\\00_analysis\\data_sets")

################################################################################
# Loading model
################################################################################

udmodel <- udpipe_download_model(language = "english-ewt")
udmodel <- udpipe_load_model(file = udmodel$file_model)

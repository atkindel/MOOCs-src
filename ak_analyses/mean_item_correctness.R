## Script to calculate and visualize mean item correctness
## Author: Alex Kindel
## Date: 26 May 2016

library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)

# Load data
home_wd <- "/Users/vpoluser/Code/irt/data/"
load(str_c(home_wd, "course_tables.Rdata"))

# TODO #

## Script to read IRT matrices into memory and tidy
## Author: Alex Kindel
## Date: 26 May 2016

library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)

# List of courses to read in
courses <- list(ResGeo="EarthSciences_ResGeo202_Spring2015",
                Compilers="Engineering_Compilers_Fall2014",
                Stocks="GSB_StocksBonds_SelfPaced",
                ChemE="Engineering_IntroChE_SelfStudy",
                Nano="Engineering_Nano_Summer2014",
                # Networking="Engineering_Networking-SP_SelfPaced",  # Bad
                OpenKnowl="Education_OpenKnowledge_Fall2014",
                StatLearn="HumanitiesandScience_StatLearning_Winter2015",
                EP="HumanitiesSciences_EP-101_Spring2015",
                MolFound="Medicine_MolFoundations_SelfPaced",
                Haptics="SelfPaced_Haptics_2014",
                MathEdu.v1="Education_115SP_2015",
                MathEdu.v2="Education_EDUC115-S_Spring2014",
                Econ1.v1="HumanitiesSciences_Econ-1_Summer2014",
                Econ1.v2="HumanitiesSciences_Econ_1_Summer2015",
                Anes.v1="Medicine_ANES204_Fall2014",
                Anes.v2="Medicine_ANES205_Fall2014",
                MedStat.v1="Medicine_MedStats_Summer2014",
                MedStat.v2="Medicine_MedStats._Summer2015",
                DigDeeper.v1="English_DiggingDeeper1_Winter2015",
                DigDeeper.v2="English_diggingdeeper2_Spring2015",
                SciWrite.v1="Medicine_Sci-Write_Fall2014",
                SciWrite.v2="Medicine_SciWrite._Fall2015",
                QMSE.v1="Engineering_QMSE01._Autumn2015",
                QMSE.v2="Engineering_QMSE-01_Fall2014",
                QMSE.v3="Engineering_QMSE-02_Winter2015",
                WomensHealth.v1="GlobalHealth_IWHHR_Summer2014",
                WomensHealth.v2="GlobalHealth_IntWomensHealth_Jan2015",
                WomensHealth.v3="GlobalHealth_INT.WomensHealth_July2015"
                )

# Cutoff constants
PROPORTION_ATTEMPTS = 0.5
ELIGIBLE_SIZE = 300

# Directories where data lives
home_wd <- "/Users/vpoluser/Code/MOOCs-src/"
exports_dir <- "data/exports/"
raws_dir <- "data/raws/"

course_tbls <- list()
course_meta <- list()
for (course in unlist(courses)) {
  matrices <- list()

  # Read in problem_metadata
  setwd(str_c(home_wd, raws_dir))
  problem_metadata <- read.csv(str_c(course, "_ProblemMetadata.csv"))

  # Get list of matrices
  setwd(str_c(home_wd, exports_dir, course))
  csvs <- list.files(pattern="csv")

  for (mtx_file in csvs) {

    # Read in matrix
    mtx <- read.csv(mtx_file, header=TRUE, row.names="learner")

    # Reduce problem ID colnames to form pid-part
    colnames(mtx) %<>%
      strsplit(".problem.", fixed=TRUE) %>%
      sapply(magrittr::extract, 2) %>%
      sapply(sub, pattern="_", replacement="-", fixed=TRUE) %>%
      strsplit("_", fixed=TRUE) %>%
      sapply(magrittr::extract, 1)

    # Order mtx columns with subparts in order
    mtx <- mtx[,order(colnames(mtx))]
    colnames(mtx) %<>%
      strsplit("-", fixed=TRUE) %>%
      sapply(magrittr::extract, 1)

    # Rank items by course order
    items_names <-
      colnames(mtx)
    items_idx <-
      colnames(mtx) %>%
      match(problem_metadata[,1]) %>%
      rank(ties.method="first")
    items <-
      data.frame(items_idx, items_names) %>%
      arrange(items_idx)

    # Order matrix columns by item total order
    colnames(mtx) <- items_idx
    mtx <- mtx[,order(as.integer(colnames(mtx)))]
    # colnames(mtx) <- items$items_names  # Set column names to platform ID

    # Order matrix rows alphabetically
    mtx <- mtx[order(row.names(mtx)),]

    # Dedupe columns (effectively collapses multi-part questions into one column)
    mtx <- mtx[!duplicated(as.list(mtx))]

    # Add to list of matrices for this course
    matrices[[sub(".csv", "", mtx_file)]] <- mtx
  }

  # Drop learner rows below 0.5 item completion rate
  last_grade <-
    matrices$last_grade %>%
    as.matrix() %>%
    ifelse(equals(., "none"), NA, .)  # Mark 'none' grade as missing
  item_prop <- rowSums(!is.na(last_grade)) / ncol(last_grade)
  eligible <- item_prop >= PROPORTION_ATTEMPTS
  if (sum(eligible) < ELIGIBLE_SIZE) next  # Skip this course if not enough learners in sample
  for (mtx_i in 1:length(matrices)) matrices[[mtx_i]][eligible,] -> matrices[[mtx_i]]

  # Add this course to data list
  course_tbls[[course]] <- matrices
  course_meta[[course]] <- c(course, nrow(last_grade), sum(eligible), ncol(last_grade))
}

# Restructure course-level metadata a bit
course_meta <- do.call("rbind", course_meta)
colnames(course_meta) <- c("course", "total", "cmpl > 0.5", "items")

# Finally, save data and clean up environment
save(course_tbls, course_meta, file=str_c(home_wd, "data/", "course_tables.Rdata"))
rm(list=ls())

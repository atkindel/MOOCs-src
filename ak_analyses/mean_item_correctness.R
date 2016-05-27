## Script to calculate and visualize mean item correctness
## Author: Alex Kindel
## Date: 26 May 2016

library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(scales)

# Load data
home_wd <- "/Users/vpoluser/Code/irt/data/"
load(str_c(home_wd, "course_tables.Rdata"))

# Summary function for item grade attempts
grade_agglom <- function(tbl, tbl_name) {

  # Tabulate correct/incorrect totals per item
  data <- data.frame(t(sapply(tbl, table)))
  colnames(data) <- c(str_c(tbl_name, "_correct"), str_c(tbl_name, "_incorrect"))

  # Proportion of correct attempts
  data[,str_c(tbl_name, "_prop")] <- data[,1] / (data[,1] + data[,2])

  return(data)
}

# Calculate change in #correct/incorrect per attempt
delta <- function(grades, att_n) {
  grades[,str_c("g", att_n, "_correct_delta")] <- grades[,str_c("g", att_n, "_correct")] - grades[,1]
  return(grades)
}

for course in names(course_tbls) {
  tbls <- course_tbls[[course]]

  # Tabulate grade counts for n=5 attempts per item
  grades <- data.frame(grade_agglom(tbls[["first_grade"]], "g1"),
                       grade_agglom(tbls[["second_grade"]], "g2"),
                       grade_agglom(tbls[["third_grade"]], "g3"),
                       grade_agglom(tbls[["fourth_grade"]], "g4"),
                       grade_agglom(tbls[["fifth_grade"]], "g5"))

  # Tabulate differences between successive item attempts
  grades[,"g1_correct_delta"] <- grades$g1_correct
  grades <- delta(grades, 2)
  grades <- delta(grades, 3)
  grades <- delta(grades, 4)
  grades <- delta(grades, 5)
  grades[,"g5_incorrect_delta"] <- grades$g5_incorrect

  # Visualize
  grades %>%

    # Prep data
    select(contains("correct_delta")) %>%
    as.matrix() %>%
    melt() %>%

    # Plot as stacked bar
    ggplot(aes(x=Var1, y=value, fill=Var2)) +
      geom_bar(position="fill", stat="identity") +
      coord_flip() +

      # Label axes and legend
      scale_y_continuous(labels=scales::percent) +
      scale_x_continuous(trans="reverse", breaks=c(1:61)) +
      scale_fill_discrete(name="attempt", labels=c("1", "2", "3", "4", "5", "Inc.")) +
      xlab("item N") +
      ylab("Proportion correct") +

      # Draw guidelines at 25% interval
      geom_hline(linetype=2, yintercept=seq(0.0, 1.0, by=0.25))

}

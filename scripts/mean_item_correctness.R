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

# Define plot palette-- using ggplot defaults mostly
palette <- c('#F8766D', '#B79F00', '#00BA38', '#00BFC4', '#619CFF', '#999999')

# Transform course metadata to data.frame if needed
course_meta <- data.frame(course_meta)


## Helper functions

# Summary function for item grade attempts
grade_agglom <- function(tbl, tbl_name) {

  # Tabulate correct/incorrect totals per item
  tbl <- data.frame(lapply(tbl, as.character), stringsAsFactors=FALSE)
  tbl <- rbind(tbl, rep("correct", times=length(tbl[1,])))
  tbl <- rbind(tbl, rep("incorrect", times=length(tbl[1,])))
  data <- data.frame(t(sapply(tbl, table))) - 1  # prevents missing levels
  colnames(data) <- c(str_c(tbl_name, "_correct"), str_c(tbl_name, "_incorrect"))
  rownames(data) <- c(1:length(data[,1]))

  # Proportion of correct attempts
  data[,str_c(tbl_name, "_prop")] <- data[,1] / (data[,1] + data[,2])

  return(data)
}

# Calculate change in #correct/incorrect per attempt
delta <- function(grades, att_n) {
  grades[,str_c("g", att_n, "_correct_delta")] <- grades[,str_c("g", att_n, "_correct")] - grades[,str_c("g", att_n-1, "_correct")]
  return(grades)
}


## Main

plots <- list()
for (course in names(course_tbls)) {
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
  grades[,"n_learners"] <- grades$g5_correct + grades$g5_incorrect

  # Visualize
  grades %>%

    # Prep data
    select(contains("correct_delta")) %>%
    as.matrix() %>%
    melt() %>%

    # Plot as stacked bar
    ggplot(aes(x=Var1, y=value, fill=Var2)) +
      #geom_bar(position="fill", stat="identity") +
      geom_bar(stat="identity") +
      coord_flip() +

      # Format/label axes and legend
      #scale_y_continuous(breaks=seq(0.0, 1.0, by=0.1), labels=scales::percent, expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_x_continuous(trans="reverse", breaks=c(1:as.numeric(as.character(course_meta$items[course]))), expand=c(0,0)) +
      scale_fill_manual(values=palette, name="attempt", labels=c("1", "2", "3", "4", "5", "Inc.")) +
      xlab("Item") +
      ylab("N correct") +
      theme(axis.ticks=element_blank(), legend.position="bottom", axis.text=element_text(size=8)) +
      labs(title=course) +

      # Draw visual guidelines at 10% interval
      geom_hline(linetype="dotted", yintercept=seq(0.0, 1.0, by=0.1)) ->

    # Save
    plots[[course]]
}

# Draw plots to outfile
ggsave("~/Downloads/totals.pdf", marrangeGrob(grobs=plots, nrow=1, ncol=1, top=NULL, right=" "), width=8.5, height=11, units="in")

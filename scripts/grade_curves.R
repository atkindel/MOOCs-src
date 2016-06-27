## Script to visualize mean item correctness density
## Author: Alex Kindel
## Date: 27 June 2016

# Load data
home_wd <- "/Users/vpoluser/Code/MOOCs-src/data/"
load(str_c(home_wd, "course_tables.Rdata"))
course_meta <- data.frame(course_meta)  # Just in case...

# Set graphics palette and grade table names
# palette <- c('#F8766D', '#B79F00', '#00BA38', '#00BFC4', '#619CFF', '#999999')
palette <- c('red', 'orange', 'gold', 'green', 'blue', 'purple')
grade_tables <- c('first_grade', 'second_grade', 'third_grade', 'fourth_grade', 'fifth_grade', 'last_grade')


## Summary statistic functions

means <- function(grades) {
  ifelse(as.matrix(grades) == "correct", 1, 0) -> grades
  colMeans(grades, na.rm=TRUE) -> means
  sapply(means, is.nan) -> invalid
  return(means[!invalid])  # Filter out items with no valid responses
}

densities <- function(course, tables) {
  calcs <- list()
  tables[[course]] -> data
  for (tbl in grade_tables) {
    means(data[[tbl]]) -> gn
    density(gn) -> d_gn
    range(d_gn$y) -> rn_gn
    calcs[[tbl]] <- list(gn, d_gn, rn_gn)
  }
  return(calcs)
}

last <- function(arr) {
  return(arr[length(arr)])
}


## Main

# Set up graphics device
png(str_c(home_wd, "density.png"),units="in",height=24,width=14,res=100)
par(mfrow=c(7,3),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))

# Run calculations over each course
calcs <- list()
for (course in names(course_tbls)) {
  calcs[[course]] <- densities(course, course_tbls)
}

# Plot mean item correctness density per attempt
for (course in names(calcs)) {
  plot(calcs[[course]][[last(grade_tables)]][[2]], col=last(palette), xlim=c(0,1), xlab=" ", ylab=" ", sub=" ", main="", lwd=1)
  for (idx in seq_along(grade_tables)) lines(calcs[[course]][[grade_tables[idx]]][[2]], col=palette[idx], lwd=1)

  # Add label, legend, etc.
  legend("topleft",bty="n",c("first","second","third","fourth","fifth","last"),lty=1,lwd=2,col=c("red","orange","gold","green","blue","purple"))
  mtext(side=3,line=.2,course)
}

dev.off()

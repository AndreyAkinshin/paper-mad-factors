library(Hmisc)
library(dplyr)

source("reference-implementation.R")

simulation.efficiency <- function() {
  filename <- "data/efficiency.csv"
  ns <- c(2:10, 50, 100, 500, 1000)
  if (file.exists(filename)) {
    df.eff <- read.csv(filename)
  } else {
    set.seed(42)
    iterationCount <- 10000
    df.eff <- data.frame(matrix(NA, nrow = iterationCount * length(ns), ncol = 4))
    colnames(df.eff) <- c("n", "sm", "hd", "thd")
    index <- 0
    for (n in ns) {
      for (i in 1:iterationCount) {
        x <- rnorm(n)
        index <- index + 1
        df.eff[index,] <- data.frame(n, sm = mad.sm(x), hd = mad.hd(x), thd = mad.thd.sqrt(x))
      }
    }
    write.csv(df.eff, filename, row.names = F, quote = F)
  }
  df.eff
}

table.efficiency <- function() {
  df.eff <- simulation.efficiency()
  ns <- unique(df.eff$n)
  df.res <- data.frame(matrix(NA, ncol = 3, nrow = length(ns)))
  colnames(df.res) <- c("n", "HD", "THD-SQRT")
  for (i in 1:length(ns)) {
    n <- ns[i]
    df.n <- df.eff[df.eff$n == n,]
    df.res[i, "n"] <- n
    df.res[i, "HD"] <- var(df.n$sm)/var(df.n$hd)
    df.res[i, "THD-SQRT"] <- var(df.n$sm)/var(df.n$thd)
  }
  knitr::kable(df.res, digits = 3, caption = "Relative statistical efficiency of the median absolute deviation.") %>%
    kable_styling(latex_options = "hold_position")
}

inline.efficiency <- function(name, n) {
  df.eff <- simulation.efficiency()
  df.n <- df.eff[df.eff$n == n,]
  target <- if (name == "hd") df.n$hd else df.n$thd
  result <- var(df.n$sm) / var(target)
  paste0("+", round((result - 1) * 100, 1), "\\%")
}
inline.efficiency("hd", 3)

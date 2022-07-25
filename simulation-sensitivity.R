library(evd)
library(EnvStats)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)

options(dplyr.summarise.inform = FALSE)

source("reference-implementation.R")

simulation.sensitivity <- function() {
  d.unif <- list(title = "Uniform(a=0, b=1)", r = runif, q = qunif)
  d.tri_0_2_1 <- list(title = "Triangular(a=0, b=2, c=1)", r = function(n) rtri(n, 0, 2, 1), q = function(p) qtri(p, 0, 2, 1))
  d.tri_0_2_02 <- list(title = "Triangular(a=0, b=2, c=0.2)", r = function(n) rtri(n, 0, 2, 0.2), q = function(p) qtri(p, 0, 2, 0.2))
  d.beta2_4 <- list(title = "Beta(a=2, b=4)", r = function(n) rbeta(n, 2, 4), q = function(p) qbeta(p, 2, 4))
  d.beta2_10 <- list(title = "Beta(a=2, b=10)", r = function(n) rbeta(n, 2, 10), q = function(p) qbeta(p, 2, 10))
  
  d.norm <- list(title = "Normal(m=0, sd=1)", r = rnorm, q = qnorm)
  d.weibull1_2 <- list(title = "Weibull(scale=1, shape=2)", r = function(n) rweibull(n, 2), q = function(p) qweibull(p, 2))
  d.student3 <- list(title = "Student(df=3)", r = function(n) rt(n, 3), q = function(p) qt(p, 3))
  d.gumbel <- list(title = "Gumbel(loc=0, scale=1)", r = rgumbel, q = qgumbel)
  d.exp <- list(title = "Exp(rate=1)", r = rexp, q = qexp)
  
  d.cauchy <- list(title = "Cauchy(x0=0, gamma=1)", r = rcauchy, q = qcauchy)
  d.pareto1_05 <- list(title = "Pareto(loc=1, shape=0.5)", r = function(n) rpareto(n, 1, 0.5), q = function(p) qpareto(p, 1, 0.5))
  d.pareto1_2 <- list(title = "Pareto(loc=1, shape=2)", r = function(n) rpareto(n, 1, 2), q = function(p) qpareto(p, 1, 2))
  d.lnorm0_1 <- list(title = "LogNormal(mlog=0, sdlog=1)", r = function(n) rlnorm(n, 0, 1), q = function(p) qlnorm(p, 0, 1))
  d.lnorm0_2 <- list(title = "LogNormal(mlog=0, sdlog=2)", r = function(n) rlnorm(n, 0, 2), q = function(p) qlnorm(p, 0, 2))
  
  d.lnorm0_3 <- list(title = "LogNormal(mlog=0, sdlog=3)", r = function(n) rlnorm(n, 0, 3), q = function(p) qlnorm(p, 0, 3))
  d.weibull1_03 <- list(title = "Weibull(shape=0.3)", r = function(n) rweibull(n, 0.3), q = function(p) qweibull(p, 0.3))
  d.weibull1_05 <- list(title = "Weibull(shape=0.5)", r = function(n) rweibull(n, 0.5), q = function(p) qweibull(p, 0.5))
  d.frechet1 <- list(title = "Frechet(shape=1)", r = function(n) rfrechet(n, shape = 1), q = function(p) qfrechet(p, shape = 1))
  d.frechet3 <- list(title = "Frechet(shape=3)", r = function(n) rfrechet(n, shape = 3), q = function(p) qfrechet(p, shape = 3))
  
  ds <- list(
    d.unif, d.tri_0_2_1, d.tri_0_2_02, d.beta2_4, d.beta2_10,
    d.norm, d.weibull1_2, d.student3, d.gumbel, d.exp,
    d.cauchy, d.pareto1_05, d.pareto1_2, d.lnorm0_1, d.lnorm0_2,
    d.lnorm0_3, d.weibull1_03, d.weibull1_05, d.frechet1, d.frechet3
  )
  ds.names <- sapply(ds, function(d) d$title)
  ns <- 5:50
  input <- expand.grid(d = ds, n = ns)
  repetitions <- 1000
  
  calc <- function(distribution, n) {
    x <- distribution$r(n)
    data.frame(t(replicate(repetitions, {
      x <- distribution$r(n)
      c(distribution = distribution$title,
        n = n,
        sm = mad.sm(x),
        hd = mad.hd(x),
        thd.sqrt = mad.thd.sqrt(x))
    })))
  }
  
  filename.sd <- "data/sensitivity-sd.csv"
  filename.iqr <- "data/sensitivity-iqr.csv"
  filename.mad <- "data/sensitivity-mad.csv"
  
  if (!file.exists(filename.sd) || !file.exists(filename.iqr) || !file.exists(filename.mad)) {
    df.raw <- do.call("rbind", lapply(1:nrow(input), function(i) calc(input$d[i][[1]], input$n[i])))
    df.raw <- df.raw %>% gather("estimator", "mad", -names(df)[1:2])
    process <- function(f) {
      df.raw %>%
        group_by(distribution, estimator, n) %>%
        summarise(v = round(f(mad), 5)) %>%
        spread("estimator", "v") %>%
        data.frame()
    }
    df.sd <- process(sd)
    df.iqr <- process(iqr)
    df.mad <- process(mad.sm)
    
    write.csv(df.sd, filename.sd, row.names = F)
    write.csv(df.iqr, filename.iqr, row.names = F)
    write.csv(df.mad, filename.mad, row.names = F)
  } else {
    read <- function(filename) {
      df0 <- read.csv(filename)
      df0$distribution <- factor(df0$distribution, levels = ds.names)
      df0
    }
    df.sd <- read(filename.sd)
    df.iqr <- read(filename.iqr)
    df.mad <- read(filename.mad)
  }

  list(sd = df.sd, iqr = df.iqr, mad = df.mad)
}

table.sensitivity <- function(n) {
  s <- simulation.sensitivity()
  f <- function(df) {
    df <- df[df$n == n, c(1, 3, 4, 5)]
    df[,2:4] <- round(df[,2:4], 2)
    df
  }
  df.sd <- f(s$sd)
  df.iqr <- f(s$iqr)
  df.mad <- f(s$mad)
  df.res <- cbind(df.sd, df.iqr[2:4], df.mad[2:4])
  kbl(df.res,
      row.names = F,
      col.names = c("Distribution", rep(c("SM", "HD", "THD"), 3)),
      caption = paste0("Properties of MAD estimations for n=", n, ".")) %>% 
    add_header_above(c(" ", "SD" = 3, "IQR" = 3, "MAD" = 3)) %>% 
    kable_styling(latex_options = "hold_position")
}

figure.sensitivity <- function() {
  s <- simulation.sensitivity()
  df.mad <- s$mad
  df.plot <- df.mad %>% gather("estimator", "value", -distribution, -n)
  df.plot$estimator <- factor(df.plot$estimator, levels = c("sm", "hd", "thd.sqrt"))
  ggplot(df.plot, aes(n, value, col = estimator)) +
    facet_wrap(vars(distribution), scales = "free", ncol = 4) +
    scale_color_manual(values = cbPalette, labels = c("SM", "HD", "THD-SQRT")) +
    labs(y = "MAD", col = "") +
    geom_line() +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 8))
}
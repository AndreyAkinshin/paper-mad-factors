source("utils.R")

read.factors <- function() {
  list(
    park = read.csv("data/park.csv"),
    sm = read.csv("data/sm.csv"),
    hd = read.csv("data/hd.csv"),
    thd = read.csv("data/thd-sqrt.csv")
  )
}

figure.stddev1 <- function() {
  x <- seq(-4, 4, by = 0.01)
  df <- data.frame(
    x = x,
    U1 = dnorm(x),
    U2 = dnormMix(x, sd1 = 1, sd2 = 49, p.mix = 0.05),
    U3 = dnormMix(x, sd1 = 1, sd2 = 9, p.mix = 0.1)
  ) %>% gather("type", "y", -1)
  
  ggplot(df, aes(x, y, col = type, linetype = type)) +
    geom_line() +
    ylab("Density") +
    theme_bw() +
    theme(legend.title = element_blank()) +
    scale_color_manual(values = cbPalette)
}

figure.stddev2 <- function() {
  x <- seq(-25, 25, by = 0.1)
  df <- data.frame(
    x = x,
    D1 = dnorm(x, sd = 1),
    D11 = dnorm(x, sd = 11),
    D3 = dnorm(x, sd = 3)
  ) %>% gather("type", "y", -1)
  df$type <- factor(df$type, levels = c("D1", "D11", "D3"))
  labels <- c(TeX("(U1) \\sigma = 1"), TeX("(U2) \\sigma = 11"), TeX("(U3) \\sigma = 3"))
  ggplot(df, aes(x, y, col = type, linetype = type)) +
    geom_line() +
    ylab("Density") +
    theme_bw() +
    theme(legend.title = element_blank(), legend.text.align = 0) +
    scale_color_manual(values = cbPalette, labels = labels) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted"), labels = labels)
}

figure.park <- function() {
  df <- read.factors()$park
  df <- df[df$n <= 100,]
  df$type <- ifelse(df$n %% 2 == 1, "odd", "even")
  
  ggplot(df, aes(n, factor)) +
    geom_line(col = cbGrey, alpha = 0.5) +
    geom_point(aes(shape = type, col = type), size = 1.1) +
    scale_color_manual(values = cbPalette) +
    scale_y_continuous(breaks = c(round(1 / qnorm(0.75), 4), seq(1.6, 2.2, by = 0.2))) +
    theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
    ylab(TeX("$C_n$")) +
    geom_hline(yintercept = 1 / qnorm(0.75)) +
    theme_bw() +
    theme(legend.title = element_blank())
}

table.factors <- function(name, caption) {
  df <- read.factors()[[name]]
  df <- rbind(data.frame(n = 1, factor = NA), df)
  slice <- function(index, size = 20) df[((index - 1) * size + 1):(index * size),]
  df <- cbind(slice(1), slice(2), slice(3), slice(4), slice(5), slice(6), slice(7))
  header <- rep(c("n", "$C_n$"), 7)
  knitr::kable(df, caption = caption, col.names = header, escape = FALSE, digits = 4) %>%
    kable_styling(latex_options = "hold_position")
}
figure.factors <- function() {
  factors <- read.factors()
  df1 <- factors$sm
  df1$type <- "SM"
  df2 <- factors$hd
  df2$type <- "HD"
  df3 <- factors$thd
  df3$type <- "THD-SQRT"
  df <- rbind(df1, df2, df3)
  df$type <- factor(df$type, levels = c("SM", "HD", "THD-SQRT"))
  df <- df[df$n <= 100,]
  ggplot(df, aes(n, factor, col = type, group = type)) +
    geom_point(aes(shape = type), size = 1.1) +
    scale_color_manual(values = cbPalette) +
    scale_y_continuous(breaks = c(round(1 / qnorm(0.75), 4), seq(1.6, 2.2, by = 0.2))) +
    scale_shape_manual(values = c(1, 3, 4)) +
    ylab(TeX("$C_n$")) +
    geom_hline(yintercept = 1 / qnorm(0.75)) +
    theme_bw() +
    theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
}

get.ab <- function() {
  df <- data.frame()
  factors <- read.factors()
  names <- c("park", "sm", "hd", "thd")
  for (name in names) {
    df.data <- factors[[name]]
    df.data <- df.data[df.data$n >= 100 & df.data$n <= 1000,]
    df.data$bias <- 1 / qnorm(0.75) / df.data$factor - 1
    fit <- lm(bias ~ 0 + I(n^(-1)) + I(n^(-2)), data = df.data)
    df <- rbind(df, round(fit$coefficients, 4))
  }
  rownames(df) <- names
  colnames(df) <- c("a", "b")
  df
}

table.ab <- function() {
  df.af <- get.ab()
  rownames(df.af) <- c("$\\operatorname{MAD}_{\\operatorname{PARK}}$", "$\\operatorname{MAD}_{\\operatorname{SM}}$",
                    "$\\operatorname{MAD}_{\\operatorname{HD}}$", "$\\operatorname{MAD}_{\\operatorname{THD-SQRT}}$")
  colnames(df.af) <- c("$\\alpha$", "$\\beta$")
  knitr::kable(df.af, caption = "$A_n$ parameters for $n > 100$.", escape = FALSE, digits = 4) %>%
    kable_styling(latex_options = "hold_position")
}

get.n100 <- function() {
  p <- get.ab()
  factors <- read.factors()
  read <- function(name, title) {
    df0 <- factors[[name]]
    df0 <- df0[df0$n > 100,]
    df0$factor.predicted <- 1 / qnorm(0.75) / (1 + p[name, 1] / df0$n + p[name, 2] / df0$n^2)
    df0$type <- title
    df0
  }
  df <- rbind(read("sm", "SM"), read("hd", "HD"), read("thd", "THD-SQRT"))
  df
}

inline.n100Diff <- function() {
  df <- get.n100()
  df$diff <- abs(df$factor - df$factor.predicted)
  round(df[df$diff == max(df$diff),]$diff, 6)
}

figure.n100 <- function() {
  df <- get.n100()
  df$type <- factor(df$type, levels = c("SM", "HD", "THD-SQRT"))
  ggplot(df, aes(n)) +
    geom_line(aes(y = factor.predicted), col = cbBlue) +
    geom_point(aes(y = factor), col = cbRed, size = 0.8) +
    facet_wrap(vars(type), ncol = 3) +
    scale_y_continuous(breaks = c(1.4826, seq(1.484, 1.493, by = 0.001))) +
    geom_hline(yintercept = 1 / qnorm(0.75)) +
    ylab(TeX("$C_n$")) +
    theme_bw()
}

inline.maxParkSmDiff <- function() {
  factors <- read.factors()
  df.park <- factors$park
  df.sm <- factors$sm
  df.sm <- df.sm[df.sm$n <= 500,]
  df <- data.frame(
    n = df.park$n,
    park = df.park$factor,
    sm = df.sm$factor,
    diff = abs(df.park$factor - df.sm$factor)
  )
  round(max(df$diff), 5)
}
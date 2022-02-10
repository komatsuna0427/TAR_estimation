#' Estimate a threshold autoregressive model with constant thresholds.
#' @param y time series vector
#' @export
TAR_const <- function (y) {
  min.ta <- round(0.2 * length(y), digits = 0)
  max.ta <- round(0.8 * length(y), digits = 0)
  df <- tibble::tibble(y)
  df <- df %>%
    dplyr::mutate(L.y = dplyr::lag(y, k = 1)) %>%
    na.omit(L.y) %>%
    dplyr::mutate(D.y = y - L.y,
                  abs.Ly = abs(L.y)) %>%
    dplyr::mutate(
      o.Ly = order(abs.Ly),
      d = 0,
      rssv = NA,
      tauv = NA
    )
  rss <- Inf
  for (ta in min.ta:max.ta) {
    # potential threshold value
    tau <- df[df$o.Ly == ta, "abs.Ly"] %>%
      as.numeric()
    df <- df %>%
      dplyr::mutate(d = ifelse(abs.Ly <= tau, 0, 1)) %>%
      dplyr::mutate(i.t = d * L.y)
    m.u <-  lm(D.y ~ 0 + i.t, data = df)
    df$rssv[ta] <-  sum(resid(m.u)^2)
    df$tauv[ta] <-  tau
    {
      if (sum(resid(m.u)^2) < rss) {
        reg <- m.u
        rss <- sum(resid(m.u)^2)
        theta <- tau
      }
      else {
      }
    }
  }
  rssplot <- ggplot2::ggplot(df, ggplot2::aes(x = tauv, y = rssv)) +
         ggplot2::geom_point() +
         ggplot2::xlab(latex2exp::TeX("$\\theta$")) +
         ggplot2::ylab("Residual Sum of Squares")
  theta <- theta
  halflife <- - log(2) / log(abs(1 + summary(reg)$coefficients[1, 1]))
  output <- list(reg, theta, halflife, rssplot)
  names(output) <- c("regression", "theta", "halflife", "rssplot")
  return(output)
}

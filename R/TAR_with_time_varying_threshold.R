library(magrittr)
library(foreach)
library(doParallel)
library(ggplot2)
registerDoParallel()

TAR_threshold_varying <- function (y) {
  # Set the minimum and maximum threshold values to ensure that at least 20% of the observations are iehther within or outside the band.
  min.ta <- as.integer(0.2 * length(y))
  max.ta <- as.integer(0.8 * length(y))
  # Total number of observations
  T <- length(y) - 1
  # Create dataset for estimation
  df <- tibble::tibble(y) %>%
    dplyr::mutate(L.y = dplyr::lag(y, k = 1)) %>%
    na.omit(L.y) %>%
    dplyr::mutate(
      D.y = y - L.y,
      abs.Ly = abs(L.y),
      trend = 0:(T-1)
    )
  # Make a pair of candidates for threshold values
  pair_theta <- df %>%
    dplyr::mutate(o.Ly = order(abs.Ly)) %>%
    dplyr::filter(o.Ly >= min.ta & o.Ly <= max.ta) %>%
    dplyr::mutate(
      theta1 = abs.Ly,
      theta2 = abs.Ly
    ) %>%
    tidyr::expand(theta1, theta2)
  # Calculate OLS for each pair of threshold values
  residual <- foreach (i = 1:nrow(pair_theta), .combine = "rbind") %dopar% {
    # Candidates for threshold values
    theta1 <- pair_theta$theta1[i]
    theta2 <- pair_theta$theta2[i]
    df_ols <- df %>%
      dplyr::mutate(theta_trend = theta1 + (theta2 - theta1) * (trend / (T - 1))) %>%
      dplyr::mutate(d = ifelse(abs.Ly <= theta_trend, 0, 1)) %>%
      dplyr::mutate(i.t = d * L.y)
    # Estimate the parameter by OLS
    m.u <-  lm(D.y ~ 0 + i.t, data = df_ols)
    # Store the values
    rss <-  sum(resid(m.u)^2)
    count <- i
    data.frame(count, theta1, theta2, rss)
  }
  # Make the dataset of RSS as tibble
  residual %<>% 
    tibble::as_tibble()
  # Find the pair of thresholds that minimizes RSS
  minimum <- residual %>%
    dplyr::filter(rss == min(rss))
  # Store the minimizers
  theta_first <- as.numeric(minimum$theta1)
  theta_last <- as.numeric(minimum$theta2)
  # In case there are multiple minimizers, take the mean
  theta_first_mean <- mean(theta_first)
  theta_last_mean <- mean(theta_last)
  # Estimate regression functions with the pair of thredholds that minimizes RSS
  df <- df %>%
    dplyr::mutate(
      theta_trend = theta_first_mean + (theta_last_mean - theta_first_mean) * (trend / (T - 1)),
      d = ifelse(abs.Ly <= theta_trend, 0, 1),
      i.t = d * L.y
    )
  reg <- lm(D.y ~ 0 + i.t, data = df)
  # Store halflife
  halflife <- - log(2) / log(abs(1 + summary(reg)$coefficients[1, 1]))
  # Contor plot
  g_contour <- ggplot(residual, aes(theta1, theta2, z = rss)) +
    geom_contour(aes(color = stat(level)))
  # Check if theta_first minimizes RSS given the estimated theta_last
  g_theta_first <- residual %>%
    dplyr::filter(theta2 == theta_last) %>%
    ggplot(aes(x = theta1, y = rss)) +
    geom_point()
  # Check if theta_last minimizes RSS given the estimated theta_first
  g_theta_last <- residual %>%
    dplyr::filter(theta1 == theta_first) %>%
    ggplot(aes(x = theta2, y = rss)) +
    geom_point()
  # Make a list
  output <- list(reg, residual, theta_first, theta_last, halflife, g_contour, g_theta_first, g_theta_last)
  names(output) <- c("regression","residual", "theta_first", "theta_last", 
                     "halflife", "plot_contour", "plot_theta_first", "plot_theta_last")
  # Return the output
  return(output)
}

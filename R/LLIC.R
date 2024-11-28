#' LLIC for Lre Model
#'
#' This function carries out an Laplace LIC analysis utilizing the Lre model.
#'
#' @param X Design matrix
#' @param y Random response vector of observed values
#' @param alpha Significance level
#' @param K Number of subsets
#'
#' @return A list containing the following components:
#' \item{MUopt}{A vector of the means of the predictor variables in the optimal subset.}
#' \item{Bopt}{A vector of the estimated regression coefficients from the final model fitted to the optimal subset.}
#' \item{MAEMUopt}{The Mean Absolute Error (MAE) for the optimal subset.}
#' \item{MSEMUopt}{The Mean Squared Error (MSE) for the optimal subset.}
#' \item{opt}{Currently NULL, a placeholder for potential future use.}
#' \item{Yopt}{A vector of the predicted values from the final model fitted to the optimal subset.}
#' @export
#'
#' @examples
#' set.seed(12)
#' library(VGAM)
#' X <- matrix(data = sample(1:3, 1200 * 5, replace = TRUE), nrow = 1200, ncol = 5)
#' b <- sample(1:3, 5, replace = TRUE)
#' e <- rlaplace(1200, 0, 1)
#' Y <- X %*% b + e
#' alpha <- 0.05
#' K <- 10
#' result <- LLIC(X, Y, alpha, K)
#' MUopt <- result$MUopt
#' Bopt <- result$Bopt
#' MAEMUopt <- result$MAEMUopt
#' MSEMUopt <- result$MSEMUopt
#' opt <- result$opt
#' Yopt <- result$Yopt
#'
#'@importFrom VGAM rlaplace
#' @importFrom stats rnorm runif rcauchy rchisq rbeta rgeom lm logLik coef predict
#' @importFrom LaplacesDemon rllaplace rallaplace rslaplace
#' @importFrom relliptical rtelliptical
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_col labs theme_minimal
#' @importFrom dplyr %>% filter mutate select
LLIC <- function(X, y, alpha, K) {
  n <- nrow(X)
  p <- ncol(X)
  LIC_values <- numeric(K)
  subsets <- vector("list", K)
  for (i in 1:K) {
    subset_indices <- sample(1:n, n / K)
    subsets[[i]] <- data.frame(X[subset_indices, , drop = FALSE], y = y[subset_indices])
  }
  for (i in 1:K) {
    subset <- subsets[[i]]
    Xi <- subset[, -ncol(subset)]
    yi <- subset[, ncol(subset)]
    fit <- lm(yi ~ ., data = Xi)
    LIC_values[i] <- 2 * (-2 * logLik(fit)) - 2 * (length(coef(fit)) - 1)
  }
  optimal_index <- which.min(LIC_values)
  optimal_subset <- subsets[[optimal_index]]
  final_fit <- lm(optimal_subset[, ncol(optimal_subset)] ~ ., data = optimal_subset[, -ncol(optimal_subset)])
  MUopt <- colMeans(optimal_subset[, -ncol(optimal_subset)])
  Bopt <- coef(final_fit)
  MAEMUopt <- mean((optimal_subset[, ncol(optimal_subset)] - predict(final_fit))^2)
  MSEMUopt <- sum((optimal_subset[, ncol(optimal_subset)] - predict(final_fit))^2) / (nrow(optimal_subset) - length(Bopt))
  opt <- NULL
  Yopt <- predict(final_fit)
  return(list(MUopt = MUopt, Bopt = Bopt, MAEMUopt = MAEMUopt, MSEMUopt = MSEMUopt, opt = opt, Yopt = Yopt))
}

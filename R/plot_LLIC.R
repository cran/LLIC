#' advanced_plotting_LLIC for LLIC
#'
#' This function visualizes the results of the LLIC analysis, including a comparison of actual and predicted values, and a bar chart of model coefficients.
#'
#' @param X Design matrix used in the LLIC analysis.
#' @param Y Random response vector of observed values used in the LLIC analysis.
#' @param result A list containing the results of the Laplace LIC analysis from the LLIC function.
#'
#' @return A list containing two `ggplot` objects:
#' \item{Actual_vs_Pred}{A scatter plot comparing the actual vs predicted values.}
#' \item{Coef}{A bar chart displaying the model's coefficients.}
#' @export
#'
#' @examples
#'
#' set.seed(12)
#' library(VGAM)
#' library(rlang)
#' library(dplyr)
#' library(ggplot2)
#' X <- matrix(data = sample(1:3, 1200 * 5, replace = TRUE), nrow = 1200, ncol = 5)
#' b <- sample(1:3, 5, replace = TRUE)
#' e <- rlaplace(1200, 0, 1)
#' Y <- X %*% b + e
#' alpha <- 0.05
#' K <- 10
#' result <- LLIC(X, Y, alpha, K)
#' plot_LLIC(X, Y, result)
#' plots <- plot_LLIC(X, Y, result)
#' print(plots$Actual_vs_Pred)
#' print(plots$Coef)
#'
#' @importFrom VGAM rlaplace
#' @importFrom stats rnorm runif rcauchy rchisq rbeta rgeom lm logLik coef predict
#' @importFrom LaplacesDemon rllaplace rallaplace rslaplace
#' @importFrom relliptical rtelliptical
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_col labs theme_minimal
#' @importFrom dplyr %>% filter mutate select
#' @importFrom rlang sym
plot_LLIC <- function(X, Y, result) {
  MUopt <- result$MUopt
  Bopt <- result$Bopt
  Yopt <- result$Yopt
  plot_data <- data.frame(
    Actual = Y,
    Predicted = Yopt,
    X1 = X[, 1],
    X2 = X[, 2],
    X3 = X[, 3],
    X4 = X[, 4],
    X5 = X[, 5]
  )
  Coef_data <- data.frame(
    Coefficients = names(Bopt),
    Value = as.numeric(Bopt)
  )
  p1 <- ggplot(plot_data, aes(x = !!sym("Actual"), y = !!sym("Predicted"))) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Actual vs Predicted", x = "Actual Y", y = "Predicted Y") +
    theme_minimal()
  p2 <- ggplot(Coef_data, aes(x = !!sym("Coefficients"), y = !!sym("Value"))) +
    geom_col() +
    labs(title = "Coefficients of the Model", x = "Coefficients", y = "Values") +
    theme_minimal()
  return(list(Actual_vs_Pred = p1, Coef = p2))
}

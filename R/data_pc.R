#' Data Processing for LLIC Analysis
#'
#' This function processes the data generated for the LLIC analysis, including filtering, mutation, and selection of specific columns.
#'
#' @param data A data frame containing the raw data generated for the LLIC analysis.
#'
#' @return A data frame with the following columns:
#' \item{X1}{The filtered values of the original `X1` column, keeping only rows where `X1 <= 2`.}
#' \item{X2}{The original `X2` column.}
#' \item{X1_squared}{A new column containing the square of the `X1` values.}
#' @export
#'
#' @examples
#' set.seed(12)
#' library(dplyr)
#' library(VGAM)
#' raw_data <- data.frame(
#'   X1 = sample(1:3, 1200, replace = TRUE),
#'   X2 = sample(1:3, 1200, replace = TRUE),
#'   X3 = sample(1:3, 1200, replace = TRUE),
#'   X4 = sample(1:3, 1200, replace = TRUE),
#'   X5 = sample(1:3, 1200, replace = TRUE),
#'   Y = rlaplace(1200, 0, 1)
#' )
#' processed_data <- data_pc(raw_data)
#'
#' @importFrom VGAM rlaplace
#' @importFrom stats rnorm runif rcauchy rchisq rbeta rgeom lm logLik coef predict
#' @importFrom LaplacesDemon rllaplace rallaplace rslaplace
#' @importFrom relliptical rtelliptical
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_col labs theme_minimal
#' @importFrom dplyr %>% filter mutate select
#' @importFrom rlang sym .data
data_pc <- function(data) {
  processed_data <- data %>%
    dplyr::filter(.data$X1 <= 2) %>%
    dplyr::mutate(X1_squared = .data$X1^2) %>%
    dplyr::select(.data$X1, .data$X2, .data$X1_squared)
  return(processed_data)
}

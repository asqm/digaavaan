#' Dig through lavaan objects and extract information as data frames.
#'
#' \code{\link{lavaan-class}} related objects contain a lot of information, specially in
#' group-wise models and models containing a lot of customization. Users
#' usually use `lavaan::summary`` to print information form the model but then
#' find themselves rambling through the inners of the model to actually
#' extract the same estimates that were previewed in `lavaan::summary`
#' as actual numbers and data frames. `summary_lavaan` is a rewrite
#' of `lavaan::summary` but instead of printing, it returns the same
#' information as a list with the information as data frames.
#'
#' @param object A model of class class \code{\link{lavaan-class}}.
#' @param ... all arguments from `lavaan::summary`. All argument information
#' is available at ?`lavaan-class` in the `summary` section.
#'
#' @return a list containing data frames with relevant information.
#' @export
#'
#' @examples
#' library(lavaan)
#' 
#' model <- "
#'    # latent variables
#'     ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + y2 + y3 + y4
#'      dem65 =~ y5 + y6 + y7 + y8
#'    # regressions
#'      dem60 ~ ind60
#'      dem65 ~ ind60 + dem60
#'    # residual covariances
#'      y1 ~~ y5
#'      y2 ~~ y4 + y6
#'      y3 ~~ y7
#'      y4 ~~ y8
#'      y6 ~~ y8"
#'
#' 
#' fit <- sem(model, data=PoliticalDemocracy)
#' # Traditional print
#' summary(fit)
#' 
#' # Same information but as a data frames
#' summary_lavaan(fit)
#' 
summary_lavaan <- function(object, ...) {
  .local <- function (object, header = TRUE, fit.measures = FALSE, 
                      estimates = TRUE, ci = FALSE, fmi = FALSE, standardized = FALSE, 
                      rsquare = FALSE, std.nox = FALSE, modindices = FALSE, 
                      nd = 3L) {
    final_list <- list()
    
    if (std.nox) 
      standardized <- TRUE
    if (header) {
      final_list$header <- short_summary_df(object)
    }
    if (fit.measures) {
      if (object@Options$test == "none") {
        warning("lavaan WARNING: fit measures not available if test = \"none\"\n\n")
      } else if (object@optim$npar > 0L && !object@optim$converged) {
        warning("lavaan WARNING: fit measures not available if model did not converge\n\n")
      } else {
        fitted_obj <- lavaan::fitMeasures(object, fit.measures = "default")
        final_list$fitted_measures <- fit_measures_details(fitted_obj)
      }
    }
    if (estimates) {
      PE <- parameter_estimates(object, ci = ci, standardized = standardized, 
                                rsquare = rsquare, fmi = fmi, remove.eq = FALSE, 
                                remove.system.eq = TRUE, remove.ineq = FALSE, 
                                remove.def = FALSE, add.attributes = TRUE)
      if (standardized && std.nox) {
        PE$std.all <- PE$std.nox
      }
      
      final_list$estimates <- PE
    }
    if (modindices) {
      final_list$modindices <- lavaan::modificationIndices(object, standardized = TRUE)
    }
    final_list
  }
  
  .local(object = object, ...)
}
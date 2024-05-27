


#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a cbmod object
#'
#' @param x A `cbmod` object.
#' @param conf.int Logical indicating whether or not to include a confidence interval in the tidied output. Defaults to FALSE.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Defaults to 0.95, which corresponds to a
#'   95 percent confidence interval.
#' @param ... Unused.
#'
#' @returns Returns a tibble with the following columns:
#' * `term` The coefficients name.
#' * `estimate` The point estimates of the coefficients.
#' * `std.error` Standard error of the regression coefficient estimates.
#' * `p.value` P-values of the null hypothesis that the regression regression coefficient estimate is 0.
#' * `conf.low` If `conf.int = TRUE`, the lower end of the confidence interval.
#' * `conf.high`If `conf.int = TRUE`, the upper end of the confidence interval.
#'
#' @export
tidy.cbmod <- function(x, conf.int=FALSE, conf.level = 0.95, ...){

  stopifnot(conf.level > 0,
            conf.level < 1)

  res <- tibble::tibble(term = names(x$parameters),
                        estimate = x$parameters,
                        std.error = x$se,
                        p.value = x$p_values)

  if (conf.int) {
    ci <- confint.cbmod(x, level = conf.level)
    res$conf.low <- ci[,1]
    res$conf.high <- ci[,2]
  }

  return(res)

}



#' @importFrom generics glance
#' @export
generics::glance

#' Glance at a cbmod object
#'
#' @param x A `cbmod` object.
#' @param ... Unused.
#'
#' @returns Returns a tibble with the following columns:
#' * `logLik` The model's log-likelihood.
#' * `npar` Numnber of parameters in the model.
#'
#' @export
glance.cbmod <- function(x, ...){


  res <- tibble::tibble(logLik = x$loglikelihood,
                        npar = x$npar)

  return(res)

}


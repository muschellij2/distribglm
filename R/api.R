api_dep_check = function() {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Need httr and jsonlite to run api functions")
  }
}

#' @rdname api
#' @export
api_get_current_beta = function(url, model_name) {
  api_dep_check()
  b = httr::GET(paste0(url, "/get_current_beta"),
                query = list(
                  model_name = model_name))
  beta = jsonlite::fromJSON(httr::content(b, as = "text"))
  if (is.character(beta)) {
    beta = jsonlite::fromJSON(beta)
  }
  return(beta)
}

#' @rdname api
#' @export
api_model_specification = function(url, model_name) {
  api_dep_check()
  # check at the compute or data site
  mod_spec = httr::GET(paste0(url, "/model_specification"),
                       query = list(
                         model_name = model_name),
                       encode = "json")
  spec = jsonlite::fromJSON(httr::content(mod_spec, as = "text"))
  if (is.character(spec)) {
    spec = jsonlite::fromJSON(spec)
  }
  spec
}

#' API Functions and wrappers
#' @rdname api
#'
#' @param url URL to the Plumber Server
#' @param model_name name of your model
#' @param all_site_names all the site names to fit this model
#' @param data dataset to get gradient value from.  The code runs
#' \code{\link{gradient_value}} to calculate the gradient, no individual
#' data is submitted.
#' @param site_name name of the site, needs to be one of the
#' \code{all_site_names}
#' @param formula model formula to fit, with tilde syntax
#' @param family generalized linear model family, see \code{\link{family}}
#' @param shuffle_rows should the rows of the dataset be permuted, so
#' as to decrease privacy concerns
#' @param link link function to use with family
#' @param verbose print out diagnostic messages
#' @param dry_run if \code{TRUE}, nothing with respect to the data
#' is submitted to the server, but returned to see what would be submitted.
#'
#' @export
api_submit_gradient = function(
  url, model_name, data, site_name,
  shuffle_rows = TRUE,
  verbose = TRUE,
  dry_run = FALSE) {

  api_dep_check()

  # beta = api_model_specification(url, model_name)
  beta = api_get_current_beta(url, model_name)

  family = make_family(beta$family, link = beta$link)
  body = list(site_name = site_name,
              model_name = model_name)
  grad = gradient_value(beta = beta$beta,
                        data = data,
                        formula = beta$formula,
                        family = family,
                        iteration_number = beta$iteration_number,
                        shuffle_rows = shuffle_rows)
  # https://github.com/jeroen/jsonlite/issues/283
  # doing this due to json issue
  # class(grad$A_mat) = "character"
  # class(grad$u) = "character"
  # class(grad$gradient) = "character"
  body$gradient_list = grad
  if (verbose) {
    message("data submitted to server is:")
    print(body)
  }
  if (dry_run) {
    return(body)
  }
  body = jsonlite::toJSON(body, digits = 20)

  # submit a gradient
  submitted = httr::PUT(paste0(url, "/submit_gradient_list"),
                        # body = list(gradient_list = grad,
                        #             site_name = "Brian",
                        #             model_name = model_name),
                        body = body,
                        encode = "json")
  submitted = jsonlite::fromJSON(httr::content(submitted, as = "text"))
  return(submitted)
}

#' @rdname api
#' @export
api_model_converged = function(url, model_name) {
  api_dep_check()
  b = httr::GET(paste0(url, "/model_converged"),
                query = list(
                  model_name = model_name))
  conv = jsonlite::fromJSON(httr::content(b, as = "text"))
  if (is.character(conv)) {
    conv = jsonlite::fromJSON(conv)
  }
  conv
}

#' @rdname api
#' @export
api_setup_model = function(url, model_name,
                           formula = "y ~ x1 + x2",
                           family = "binomial",
                           link = "logit",
                           all_site_names) {
  if (inherits(formula, "formula")) {
    formula = as.character(formula)
    formula = trimws(formula)
    formula = formula[ formula != "~"]
    formula = paste0(formula[1], "~",
                     paste0(formula[-1], collapse = " + "))
  }
  family = make_family(family, link = link)
  link = family$link
  family = family$family
  api_dep_check()
  res = httr::PUT(paste0(url, "/setup_model"),
                  body = list(
                    model_name = model_name,
                    formula = formula,
                    family = family,
                    link = link,
                    all_site_names = all_site_names),
                  encode = "json")
  model_setup = jsonlite::fromJSON(httr::content(res, as = "text"))
  model_setup
}

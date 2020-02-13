api_dep_check = function() {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Need httr and jsonlite to run api functions")
  }
}

#' @rdname api
#' @export
api_available_models = function(url) {
  b = httr::GET(paste0(url, "/available_models"))
  beta = jsonlite::fromJSON(httr::content(b, as = "text"))
  return(beta)
}

#' @rdname api
#' @export
api_get_current_beta = function(url, model_name) {
  api_dep_check()
  if (missing(model_name)) {
    model_name = api_available_models(url = url)
  }
  stopifnot(length(model_name) == 1)
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
  if (missing(model_name)) {
    model_name = api_available_models(url = url)
  }
  stopifnot(length(model_name) == 1)
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
  if (missing(model_name)) {
    model_name = api_available_models(url = url)
  }
  stopifnot(length(model_name) == 1)
  stopifnot(length(site_name) == 1)

  # beta = api_model_specification(url, model_name)
  beta = api_get_current_beta(url, model_name)

  family = make_family(beta$family, link = beta$link)
  body = list(site_name = site_name,
              model_name = model_name)
  mod_spec = api_model_specification(
    url = url,
    model_name = model_name)
  if (!is.null(mod_spec$all_site_names)) {
    if (!site_name %in% mod_spec$all_site_names) {
      stop(paste0(
        "Site Name ", site_name, " not one of the site names",
        " specified for model ", model_name, ", which are ",
        paste(mod_spec$all_site_names, collapse = "," ),
        ". Please ask those",
        " who set up the model")
      )
    }
  }
  site_name = match.arg(site_name, choices = mod_spec$all_site_names)
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
  if (missing(model_name)) {
    model_name = api_available_models(url = url)
  }
  stopifnot(length(model_name) == 1)
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
  api_dep_check()
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
  if (missing(model_name)) {
    model_name = api_available_models(url = url)
  }
  stopifnot(length(model_name) == 1)
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

#' @rdname api
#' @export
api_clear_model = function(url, model_name) {
  api_dep_check()
  stopifnot(length(model_name) == 1)
  res = httr::PUT(paste0(url, "/clear_model"),
                  body = list(
                    model_name = model_name),
                  encode = "json")
  model_setup = jsonlite::fromJSON(httr::content(res, as = "text"))
  model_setup
}

#' @rdname api
#' @param wait_time Time, in seconds, to wait until to try to
#' get new estimate
#' @param ... addition alarguments to send to
#' \code{\link{api_submit_gradient}}
#' @export
api_estimate_model = function(
  url, model_name,
  data,
  site_name,
  wait_time = 1,
  ...) {
  beta = list(converged = FALSE)
  while (!beta$converged) {
    # run at either site
    beta = api_get_current_beta(url, model_name = model_name)
    print(beta)

    api_submit_gradient(
      url = url,
      model_name = model_name,
      data = data,
      site_name = site_name,
      ...)
    Sys.sleep(wait_time)
  }
  out = api_model_converged(url, model_name)
  return(out)
}

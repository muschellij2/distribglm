api_dep_check = function() {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Need httr and jsonlite to run api functions")
  }
}

#' @rdname api
#' @export
api_url = function(url = NULL) {
  if (is.null(url)) {
    url = getOption("distribglm_url")
  }
  if (is.null(url)) {
    url = Sys.getenv("DISTRIBGLM_URL", unset = NA)
    if (is.na(url)) url = NULL
  }
  if (is.null(url)) {
    url = "https://rsconnect.biostat.jhsph.edu/distribglm"
  }
  return(url)
}

#' @rdname api
#' @export
api_set_url = function(url) {
  options(distribglm_url = url)
  return(url)
}

httr_no_check_ssl = function(expr) {
  httr::with_config(
    httr::config(ssl_verifypeer = FALSE),
    expr
  )
}

#' @rdname api
#' @return The `api_available_models` function returns the available
#' models running or already run.
#' @export
api_available_models = function(
  url = api_url(),
  config = list(),
  ...) {

  api_dep_check()
  b = httr_no_check_ssl({
    httr::GET(
      paste0(url, "/available_models"),
      config = config,
      ...)
  })
  httr::warn_for_status(b)
  beta = jsonlite::fromJSON(httr::content(b, as = "text", encoding = "UTF-8"))
  return(beta)
}

#' @rdname api
#' @return The `api_get_current_beta` function returns the current beta
#' estimates.
#' @export
api_get_current_beta = function(
  model_name,
  url = api_url(),
  config = list(),
  ...) {
  api_dep_check()
  if (missing(model_name)) {
    model_name = api_available_models(url = url,
                                      config = config,
                                      ...)
  }
  stopifnot(length(model_name) == 1)
  b = httr_no_check_ssl({
    httr::GET(
      paste0(url, "/get_current_beta"),
      query = list(
        model_name = model_name),
      config = config,
      ...)
  })
  httr::warn_for_status(b)
  beta = jsonlite::fromJSON(httr::content(b, as = "text", encoding = "UTF-8"))
  if (is.character(beta)) {
    beta = jsonlite::fromJSON(beta)
  }
  return(beta)
}

#' @rdname api
#' @return The `api_model_trace` function returns a list of the values
#' throughout iterations of the model fitting.
#' @export
api_model_trace = function(
  model_name,
  url = api_url(),
  config = list(),
  ...) {
  api_dep_check()
  if (missing(model_name)) {
    model_name = api_available_models(url = url,
                                      config = config,
                                      ...)
  }
  stopifnot(length(model_name) == 1)
  b = httr_no_check_ssl({
    httr::GET(
      paste0(url, "/model_trace"),
      query = list(
        model_name = model_name),
      config = config,
      ...)
  })
  httr::warn_for_status(b)
  result = jsonlite::fromJSON(httr::content(b, as = "text", encoding = "UTF-8"))
  result = lapply(result, function(beta) {
    if (is.character(beta)) {
      beta = jsonlite::fromJSON(beta)
    }
    beta
  })
  return(result)
}

#' @rdname api
#' @export
#'
#' @return The `api_model_specification` function returns a list of the
#' parameters of the model specification, if the model is present.
api_model_specification = function(
  model_name,
  url = api_url(),
  config = list(),
  ...) {
  api_dep_check()
  if (missing(model_name)) {
    model_name = api_available_models(url = url, ...)
  }
  stopifnot(length(model_name) == 1)
  # check at the compute or data site
  mod_spec = httr_no_check_ssl({
    httr::GET(paste0(url, "/model_specification"),
              query = list(
                model_name = model_name),
              encode = "json",
              config = config,
              ...)
  })
  httr::warn_for_status(mod_spec)
  spec = jsonlite::fromJSON(httr::content(mod_spec, as = "text", encoding = "UTF-8"))
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
#' @param config additional configuration settings such as http
#' authentication and additional headers.
#' @param tolerance tolerance for convergence
#'
#' @export
#' @examples
#' api_url()
#' api_set_url(api_url())
#' api_available_models()
#'
#' @return The `api_submit_gradient` function returns a list from the result of
#' the API call.
api_submit_gradient = function(
  model_name,
  url = api_url(),
  data, site_name,
  shuffle_rows = TRUE,
  verbose = TRUE,
  dry_run = FALSE,
  config = list(),
  ...) {

  api_dep_check()
  if (missing(model_name)) {
    model_name = api_available_models(url = url,
                                      config = config,
                                      ...)
  }
  stopifnot(length(model_name) == 1)
  stopifnot(length(site_name) == 1)

  # beta = api_model_specification(url, model_name)
  beta = api_get_current_beta(
    url = url,
    model_name = model_name,
    config = config,
    ...)

  family = make_family(beta$family, link = beta$link)
  body = list(site_name = site_name,
              model_name = model_name)
  mod_spec = api_model_specification(
    url = url,
    model_name = model_name,
    config = config,
    ...)
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
  grad = gradient_value(
    beta = beta$beta,
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
  submitted = httr_no_check_ssl({
    httr::PUT(
      paste0(url, "/submit_gradient_list"),
      # body = list(gradient_list = grad,
      #             site_name = "Brian",
      #             model_name = model_name),
      body = body,
      encode = "json",
      config = config,
      ...)
  })
  httr::warn_for_status(submitted)
  submitted = jsonlite::fromJSON(httr::content(submitted, as = "text", encoding = "UTF-8"))
  return(submitted)
}

#' @rdname api
#' @export
#' @return The `api_model_converged` function returns an indicator if
#' the model converges or not.
api_model_converged = function(
  model_name,
  url = api_url(),
  config = list(),
  ...) {
  api_dep_check()
  if (missing(model_name)) {
    model_name = api_available_models(url = url,
                                      config = config,
                                      ...)
  }
  stopifnot(length(model_name) == 1)
  b = httr_no_check_ssl({
    httr::GET(
      paste0(url, "/model_converged"),
      query = list(
        model_name = model_name),
      config = config,
      ...)
  })
  httr::warn_for_status(b)
  model = jsonlite::fromJSON(httr::content(b, as = "text", encoding = "UTF-8"))
  if (is.character(model)) {
    model = jsonlite::fromJSON(model)
  }
  if (is.null(model$null.deviance)) {
    model$null.deviance = NA
  }
  class(model) = c("glm", "lm")
  names(model$coefficients) = model$beta_names
  model$call = call(
    "glm",
    formula = as.formula(model$setup$formula),
    family = as.symbol(
      paste0(model$setup$family$family, "(link=\"", model$setup$family$link, "\")")))
  model$z_value = model$coefficients / sqrt(diag(model$covariance))
  model$p_value = 2 * stats::pnorm(abs(model$z_value), lower.tail = FALSE)

  model
}

#' @rdname api
#' @export
#' @return The `api_setup_model` function submits a model to set up on the
#' server.
api_setup_model = function(
  model_name,
  url = api_url(),
  formula = "y ~ x1 + x2",
  family = "binomial",
  link = "logit",
  all_site_names,
  config = list(),
  tolerance = 1e-9,
  ...) {
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
    model_name = api_available_models(url = url,
                                      config = config,
                                      ...)
  }
  stopifnot(length(model_name) == 1)
  res = httr_no_check_ssl({
    httr::PUT(
      paste0(url, "/setup_model"),
      body = list(
        model_name = model_name,
        formula = formula,
        family = family,
        link = link,
        tolerance = tolerance,
        all_site_names = all_site_names),
      encode = "json",
      config = config,
      ...)
  })
  httr::warn_for_status(res)
  model_setup = jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  model_setup
}

#' @rdname api
#' @export
#' @return The `api_clear_model` function clears out a model and returns the
#' output from the API.
api_clear_model = function(model_name,
                           url = api_url(),
                           config = list(),
                           ...) {
  api_dep_check()
  stopifnot(length(model_name) == 1)
  res = httr_no_check_ssl({
    httr::PUT(
      paste0(url, "/clear_model"),
      body = list(
        model_name = model_name),
      config = config,
      ...,
      encode = "json")
  })
  httr::warn_for_status(res)
  model_setup = jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  model_setup
}

#' @rdname api
#' @param wait_time Time, in seconds, to wait until to try to
#' get new estimate
#' @param ... additional arguments to send to
#' \code{\link{api_submit_gradient}}
#' @export
api_estimate_model = function(
  model_name,
  url = api_url(),
  data,
  site_name,
  wait_time = 1,
  config = list(),
  verbose = TRUE,
  ...) {
  api_dep_check()

  beta = list(converged = FALSE)
  while (!beta$converged) {
    # run at either site
    beta = api_get_current_beta(
      url = url, model_name = model_name,
      config = config,
      ...)
    if (verbose) {
      print(beta)
    }

    api_submit_gradient(
      url = url,
      model_name = model_name,
      data = data,
      site_name = site_name,
      config = config,
      verbose = verbose,
      ...)
    Sys.sleep(wait_time)
  }
  out = api_model_converged(url = url, model_name = model_name,
                            config = config,
                            ...)
  return(out)
}

rsconnect_authorization = function(
  api_key = Sys.getenv("CONNECT_API_KEY")) {
  auth_hdr = httr::add_headers(
    Authorization = paste0("Key ", api_key))
}

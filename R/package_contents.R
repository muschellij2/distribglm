
#' Estimate the update gradient value
#'
#' @param beta current beta value, leave \code{NULL} to initialize
#' @param data dataset to get gradient value from
#' @param formula model formula to fit, with tilde syntax
#' @param family generalized linear model family, see \code{\link{family}}
#' @param iteration_number number of fitting iteration, used for tracking
#' @param shuffle_rows should the rows of the dataset be permuted, so
#' as to decrease privacy concerns
#' @param link link function to use with family
#'
#' @return A list of estimated values, including the gradient,
#' sample size, iteration number, covariance matrix (\code{A_mat}),
#' number of samples with non-zero weights, the sum of the dispersion
#' values (for overdispersion estimates), and a vector of values
#' for combining to create the population gradient (`u`), with length
#' of the number of beta values
#'
#' @export
#' @examples
#' data = data.frame(y = c(0, 0, 1),
#' pois_y = c(4, 1, 0),
#' x2 = c(-2.19021287072066,
#'        -0.344307138450805, 3.47215796952745),
#' x1 = c(-0.263859503846267,
#'        -0.985160029707486, 0.227262373184513))
#' gradient_value(data = data, formula = y ~ x1 + x2,
#' family = "binomial")
#' gradient_value(data = data, formula = pois_y ~ x1 + x2,
#' family = "poisson")
gradient_value = function(beta = NULL, data, formula,
                          family = binomial(),
                          iteration_number = 0,
                          shuffle_rows = TRUE,
                          link = NULL) {
  if (shuffle_rows) {
    data = data[sample(nrow(data)), ]
  }
  formula = as.formula(formula)
  y = model.frame(formula, data = data)[,1]
  X = model.matrix(formula, data = data)
  beta_names = colnames(X)
  cc = complete.cases(X)
  X = X[cc,]
  y = y[cc]

  stopifnot(NCOL(y) == 1)
  n = rep(1, length(y))
  if (is.null(beta)) {
    beta = rep(0, ncol(X))
  }
  family = make_family(family = family, link = link)
  family
  # print(family)
  linkinv = family$linkinv
  # print(linkinv)
  variance <- family$variance
  mu.eta <- family$mu.eta
  dev.resids <- family$dev.resids
  aic <- family$aic

  # dev.resids <- family$dev.resids
  eta = drop(X %*% beta)
  mu = linkinv(eta)
  # expb = exp(X %*% beta)
  # p = expb / (1 + expb)
  mu.eta.val <- mu.eta(eta)
  mu = c(mu)
  W =  (mu.eta.val^2) / variance(mu)
  # fisher scoring from Nelder page 42 (2nd ed)
  u = t(X) %*% ((y - mu) * (mu.eta.val / variance(mu)))
  A = t(X) %*% diag(W) %*% X
  gradient = solve(A) %*% u

  weights = rep(1, length = length(eta))
  good <- (weights > 0) & (mu.eta.val != 0)
  w <- sqrt((weights[good] * mu.eta.val[good]^2)/variance(mu)[good])
  wt = w^2
  residuals <- (y - mu)[good]/mu.eta.val[good]
  dispersion_sum = sum(wt * residuals^2)
  deviance <- sum(dev.resids(y, mu, weights))

  intercept = TRUE
  offset = rep(0, length(y))
  wtdmu <- if (intercept) {
    sum(weights * y)/sum(weights)
  } else {
    linkinv(offset)
  }
  # null.deviance <- sum(dev.resids(y, wtdmu, weights))
  aic.model <- aic(y, n, mu, weights, deviance)


  # gradient here is d_loglik/d_beta without variance components
  # gradient = drop(t(X) %*% (y - mu))
  # gradient = gradient / nrow(X)
  #
  # gradient = 1/nrow(X) * drop(t(X) %*% diag(W) %*% (y - mu))
  #  h = 1 / (1 + exp(-eta))
  # log_lik = -(t(y) %*% log(h) + t(1 - y) %*% log(1 - h))
  stopifnot(length(gradient) == length(beta))
  result = list(
    gradient = gradient,
    sample_size = nrow(X),
    iteration_number = iteration_number,
    A_mat = A,
    n_ok = sum(weights != 0),
    dispersion_sum = dispersion_sum,
    u = u,
    deviance = deviance,
    # null.deviance = null.deviance,
    aic = aic.model,
    beta_names = beta_names
  )
  return(result)
}


#' Setup Model and Formula
#'
#' @inheritParams clear_model
#' @param clear_model Should the model be cleared (all files deleted
#' model with same name) before creating new model
#' @param formula model formula to fit, with tilde syntax
#' @param family generalized linear model family, see \code{\link{family}}
#' @param all_site_names all the site names to fit this model
#' @param link link function to use with family
#' @param max_iterations maximum number of iterations to run
#' @param tolerance tolerance for convergence
#'
#' @return A character path to a formula/model file
#' @export
#' @examples
#' tdir = tempfile()
#' dir.create(tdir)
#' model_name = "logistic_example"
#' form_file = setup_model(model_name = model_name,
#' synced_folder = tdir,
#' formula =  y ~ x1 + x2, family =  binomial())
#'
setup_model = function(model_name, synced_folder,
                       clear_model = TRUE,
                       formula = y ~ x1 + x2,
                       family = binomial(),
                       all_site_names = NULL,
                       link = NULL,
                       max_iterations = 100,
                       tolerance = 1e-9
                       ) {
  # this structure is the same on all sites
  fols = file.path(synced_folder,
                   c("formulas", "gradients", "models",
                     "betas"))
  sapply(fols, dir.create, showWarnings = FALSE)
  model_folder = file.path(synced_folder, "formulas")
  # gradients_folder = file.path(synced_folder, "gradients")
  # beta_folder = file.path(synced_folder, "betas")
  # converged_folder = file.path(synced_folder, "models")
  stopifnot(length(model_name) == 1)

  family = make_family(family = family, link = link)

  formula_file = file.path(model_folder, paste0(model_name, ".rds"))
  if (file.exists(formula_file) & !clear_model) {
    warning("formula file already exists and will be overwritten")
  }
  # which model are we running
  if (clear_model) {
    clear_model(model_name, synced_folder)
  }

  L = list(formula = formula,
           family = family,
           model_name = model_name,
           max_iterations = max_iterations,
           tolerance = tolerance)
  L$all_site_names = all_site_names
  readr::write_rds(L, formula_file)
  return(formula_file)
}


#' @export
#' @rdname gradient_value
#' @examples
#' data = data.frame(y = c(0, 0, 1),
#' pois_y = c(4, 1, 0),
#' x2 = c(-2.19021287072066,
#'        -0.344307138450805, 3.47215796952745),
#' x1 = c(-0.263859503846267,
#'        -0.985160029707486, 0.227262373184513))
#' use_glm_gradient_value(data = data, formula = y ~ x1 + x2,
#' family = binomial(link = "probit"))
use_glm_gradient_value = function(
  beta = NULL, data, formula,
  family = binomial(),
  iteration_number = 0,
  shuffle_rows = TRUE) {

  if (shuffle_rows) {
    data = data[sample(nrow(data)), ]
  }
  formula = as.formula(formula)
  X = model.matrix(formula, data = data)
  beta_names = colnames(X)

  # mu.eta <- family$mu.eta
  if (is.null(beta)) {
    beta = rep(0, ncol(X))
  }
  start = beta
  mod = glm(
    formula = formula,
    data = data,
    family = family,
    start = start,
    control = list(maxit = 1))
  gradient = coef(mod) - start

  result = list(
    gradient = gradient,
    sample_size = nrow(X),
    iteration_number = iteration_number,
    # null.deviance = mod$null.deviance,
    deviance = mod$deviance,
    aic = mod$aic,
    beta_names = beta_names
  )
  return(result)
}




#' Aggregate Gradient values
#'
#' @param all_gradient_files vector of character paths to files for
#' gradients to combine together on the computing site
#' @param iteration_number number of fitting iteration, used for tracking
#' and checking
#'
#' @return A list of estimated values, including the gradient,
#' sample size, iteration number, covariance matrix (\code{A_mat}),
#' number of samples with non-zero weights, the sum of the dispersion
#' values (for overdispersion estimates)
#' @export
#'
aggregate_gradients = function(
  all_gradient_files,
  iteration_number) {
  gradient_list = lapply(all_gradient_files, readr::read_rds)
  names(gradient_list) = all_gradient_files


  iter_nums = sapply(gradient_list, function(x) x$iteration_number)
  stopifnot(is.vector(iter_nums))
  stopifnot(all(iter_nums == iteration_number))
  n_ok = sapply(gradient_list, function(x) x$n_ok)
  n_ok = sum(n_ok)

  check_beta_names = sapply(gradient_list, function(x) {
    if (is.null(x$beta_names)) {
      return(FALSE)
    }
    all(x$beta_names == gradient_list[[1]]$beta_names)
  })
  beta_names = NULL
  if (!all(check_beta_names)) {
    warning("Not all the names for beta are equal, may be an error")
  } else {
    beta_names = gradient_list[[1]]$beta_names
  }

  dispersion_sum = sapply(gradient_list, function(x) x$dispersion_sum)
  dispersion_sum = sum(dispersion_sum)

  deviance = sapply(gradient_list, function(x) x$deviance)
  deviance = sum(deviance)

  # null.deviance = sapply(gradient_list, function(x) x$null.deviance)
  # null.deviance = sum(null.deviance)

  aic = sapply(gradient_list, function(x) x$aic)
  aic = sum(aic)

  ss = sapply(gradient_list, function(x) x$sample_size)
  stopifnot(is.vector(ss))
  n = sum(ss)
  A_mat = sapply(gradient_list, function(x) {
    A_mat = x$A_mat
  })
  A_mat = rowSums(A_mat)
  A_mat = array(A_mat, dim = dim(gradient_list[[1]]$A_mat))
  colnames(A_mat) = rownames(A_mat) = beta_names
  u = sapply(gradient_list, function(x) {
    u = x$u
  })
  u = rowSums(u)
  covariance = solve(A_mat)
  gradient = covariance %*% u
  result = list(
    gradient = gradient,
    A_mat = A_mat,
    total_sample_size = n,
    covariance_unscaled = covariance,
    dispersion_sum = dispersion_sum,
    deviance = deviance,
    # null.deviance = null.deviance,
    aic = aic,
    iteration_number = iteration_number,
    n_ok = n_ok,
    beta_names = beta_names)
  return(result)
  #
  # sum_grads = sapply(gradient_list, function(x) x$gradient)
  # if (is.null(beta)) {
  #   beta = rep(0, nrow(sum_grads))
  # }
  # stopifnot(is.matrix(sum_grads))
  # grad = rowSums(sum_grads)
  #

  #
  # ss = sapply(gradient_list, function(x) x$sample_size)
  # stopifnot(is.vector(ss))
  # n = sum(ss)
  # grad = grad / n
  #
  # # weight the gradient updates - new way
  # sum_grads = sapply(gradient_list, function(x) {
  #   x$gradient * x$sample_size / n
  # })
  # grad = rowSums(sum_grads)
  # result = list(
  #   gradient = grad,
  #   total_sample_size = n)
  # return(result)
}




#' Estimate Site Gradient
#'
#' @param site_name name of the site, needs to be one of the
#' \code{all_site_names}
#' @inheritParams clear_model
#' @param data dataset to get gradient value from
#' @param shuffle_rows should the rows of the dataset be permuted, so
#' as to decrease privacy concerns
#' @param all_site_names all the site names used to fit this model
#' @param experimental using the `glm` function rather than a
#' custom-written function
#'
#'
#' @return A character filename of the gradient file, with the
#' output from \code{\link{gradient_value}}
#' @importFrom readr read_csv read_rds
#' @importFrom stats family binomial poisson glm coef as.formula
#' @importFrom stats model.matrix model.frame complete.cases
#' @export
#' @examples
#' data = data.frame(y = c(0, 0, 1),
#'                   pois_y = c(4, 1, 0),
#'                   x2 = c(-2.19021287072066,
#'                          -0.344307138450805, 3.47215796952745),
#'                   x1 = c(-0.263859503846267,
#'                          -0.985160029707486, 0.227262373184513))
#' tdir = tempfile()
#' dir.create(tdir)
#' model_name = "logistic_example"
#' form_file = setup_model(model_name = model_name,
#'                         synced_folder = tdir,
#'                         formula =  "y ~ x1 + x2", family =  "binomial")
#' outfile = estimate_site_gradient(
#'   model_name = model_name, synced_folder = tdir,
#'   all_site_names = "site1",
#'   data = data)
#' clear_model(model_name, tdir)
#' testthat::expect_error({
#' outfile = estimate_site_gradient(
#'   model_name = model_name, synced_folder = tdir,
#'   all_site_names = "site1",
#'   data = data)
#' })
estimate_site_gradient = function(
  model_name, synced_folder,
  site_name = "site1", data,
  all_site_names = NULL,
  shuffle_rows = TRUE,
  experimental = FALSE) {


  stopifnot(length(model_name) == 1)

  file_list = folder_names(synced_folder)
  gradients_folder = file_list$gradients_folder
  model_folder = file_list$model_folder

  # which model are we running
  formula_file = file.path(model_folder,
                           paste0(model_name, ".rds"))

  if (!file.exists(formula_file)) {
    stop(paste0("Formula file: ", formula_file, " doesn't exist!",
                " You may need to contact processing site or check your ",
                "synced_folder"))
  } else {
    formula_list = readr::read_rds(formula_file)
    formula = formula_list$formula
    formula = as.formula(formula)
    family = formula_list$family
    if (is.null(all_site_names)) {
      all_site_names = formula_list$all_site_names
    }

    if (is.character(family)) {
      family = get(family, envir = .BaseNamespaceEnv)
    }
    if (is.function(family)) {
      family = family()
    }
    if (!inherits(family, "family")) {
      stop("family specified is not a family object - see setup_model")
    }
  }
  stopifnot(!is.null(all_site_names))
  site_name = match.arg(site_name, choices = all_site_names)


  res = get_current_beta(model_name, synced_folder)
  beta = res$beta
  iteration_number = res$iteration_number

  gradient_file = file.path(
    gradients_folder,
    paste0(model_name, "-",
           site_name,
           sprintf("-iteration%04.0f", iteration_number),
           ".rds"))
  all_gradient_files = file.path(
    gradients_folder,
    paste0(model_name, "-",
           all_site_names,
           sprintf("-iteration%04.0f", iteration_number),
           ".rds"))
  # here we would simply wait
  # should check if converged
  if (file.exists(gradient_file)) {
    if (!all(file.exists(all_gradient_files))) {
      print("Waiting for other sites to create gradients")
    } else {
      print("Waiting for compute site to create new betas")
    }
  } else {
    print(paste0("Writing Gradient, iteration ",
                 iteration_number))
    if (experimental) {
      grad = use_glm_gradient_value(beta = beta,
                                    data = data,
                                    formula = formula,
                                    family = family,
                                    iteration_number = iteration_number,
                                    shuffle_rows = shuffle_rows)
    } else {
      grad = gradient_value(beta = beta,
                            data = data,
                            formula = formula,
                            family = family,
                            iteration_number = iteration_number,
                            shuffle_rows = shuffle_rows)
    }
    print(grad)
    readr::write_rds(grad, gradient_file)
    rm(grad)
  }
  return(gradient_file)
}




#' Estimate the updated beta value
#'
#' @inheritParams clear_model
#' @param all_site_names all the site names used to fit this model
#' @param verbose print diagnostic messages
#' @return A file name of the estimated values necessary for
#' the final estimates
#' @export
#' @examples
#' data = data.frame(y = c(0, 0, 1),
#'                   pois_y = c(4, 1, 0),
#'                   x2 = c(-2.19021287072066,
#'                          -0.344307138450805, 3.47215796952745),
#'                   x1 = c(-0.263859503846267,
#'                          -0.985160029707486, 0.227262373184513))
#' synced_folder = tempfile()
#' dir.create(synced_folder)
#' model_name = "logistic_example"
#' form_file = setup_model(model_name = model_name,
#'                         synced_folder = synced_folder,
#'                         formula =  y ~ x1 + x2, family =  binomial(),
#'                         tolerance = 5)
#' outfile = estimate_site_gradient(
#'   model_name = model_name, synced_folder = synced_folder,
#'   all_site_names = "site1",
#'   data = data)
#' estimate_new_beta(model_name, synced_folder,
#' all_site_names = "site1")
#' master_beta_file(model_name, synced_folder)
#' outfile = estimate_site_gradient(
#'   model_name = model_name, synced_folder = synced_folder,
#'   all_site_names = "site1",
#'   data = data)
#'
#' estimate_new_beta(model_name, synced_folder,
#' all_site_names = "site1")
#' master_beta_file(model_name, synced_folder)
estimate_new_beta = function(
  model_name, synced_folder,
  all_site_names = NULL,
  verbose = TRUE) {

  stopifnot(length(model_name) == 1)

  file_list = folder_names(synced_folder)
  gradients_folder = file_list$gradients_folder
  beta_folder = file_list$beta_folder
  model_folder = file_list$model_folder
  converged_folder = file_list$converged_folder


  final_file = file.path(converged_folder,
                         paste0(model_name, ".rds"))

  if (file.exists(final_file)) {
    warning("Model already converged, delete iterations to run again")
    return(final_file)
  }


  res = get_current_beta(model_name, synced_folder)
  beta = res$beta
  iteration_number = res$iteration_number

  out_beta_file = file.path(
    beta_folder,
    paste0(
      model_name,
      sprintf("-iteration%04.0f", iteration_number),
      ".rds")
  )
  # list_gradient_files = list.files(
  #   gradients_folder,
  #   pattern = paste0("^", model_name, ".*",
  #                    sprintf("-iteration%04.0f", iteration_number),
  #                    ".rds"),
  #   full.names = TRUE)

  # which model are we running
  formula_file = file.path(model_folder,
                           paste0(model_name, ".rds"))
  formula_list = readr::read_rds(formula_file)
  max_iterations = formula_list$max_iterations
  if (is.null(max_iterations)) {
    max_iterations = 100
  }
  tolerance = formula_list$tolerance
  if (is.null(tolerance)) {
    tolerance = 1e-9
  }
  if (is.null(all_site_names)) {
    if (!file.exists(formula_file)) {
      stop(paste0("Formula file: ", formula_file, " doesn't exist!",
                  " You may need to contact processing site or check your ",
                  "synced_folder"))
    } else {
      all_site_names = formula_list$all_site_names
    }
    stopifnot(!is.null(all_site_names))
  }


  all_gradient_files = file.path(
    gradients_folder,
    paste0(model_name, "-",
           all_site_names,
           sprintf("-iteration%04.0f", iteration_number),
           ".rds"))

  fe = file.exists(all_gradient_files)

  # should check if converged
  if (!file.exists(out_beta_file)) {
    if (!all(fe)) {
      print("Waiting for other sites to create gradients")
      # print("Missing files:")
      # print(all_gradient_files[!fe])
    } else {
      print(paste0(
        "Reading in gradients, iteration ", iteration_number))
      result = aggregate_gradients(
        all_gradient_files, iteration_number)
      gradient = result$gradient
      total_sample_size = result$total_sample_size
      dispersion_sum = result$dispersion_sum
      deviance = result$deviance
      # null.deviance = result$null.deviance
      aic = result$aic
      n_ok = result$n_ok
      A_mat = result$A_mat
      covariance_unscaled = result$covariance_unscaled
      tol <- max(dim(A_mat)) * .Machine$double.eps
      q.r = qr(A_mat, tol = tol, LAPACK = FALSE)
      beta_names = result$beta_names

      object = formula_list
      if (object$family$family %in% c("poisson", "binomial")) {
        dispersion = 1
      } else {
        dispersion = dispersion_sum/(n_ok - q.r$rank)
      }
      covariance = dispersion * covariance_unscaled
      # print(gradient)

      if (is.null(beta)) {
        beta = rep(0, length(gradient))
        epsilon = Inf
      } else {
        # see glm.control
        epsilon = max(abs(gradient)/(abs(beta) + 0.01))
      }
      if (verbose) {
        message("epsiolon value")
      }
      print(epsilon)
      converged = epsilon < tolerance
      if (converged || iteration_number >= max_iterations) {
        if (converged) {
          print("Model has converged!")
        }
        final_beta_list = list(
          setup = formula_list,
          beta = beta,
          coefficients = as.numeric(beta),
          num_iterations = iteration_number,
          max_iterations = max_iterations,
          converged = converged,
          gradient = gradient,
          dispersion = dispersion,
          estimated_dispersion =  dispersion_sum/(n_ok - q.r$rank),
          tolerance = tolerance,
          epsilon = epsilon,
          A_mat = A_mat,
          rank = q.r$rank,
          n_ok = n_ok,
          covariance = covariance,
          covariance_unscaled = covariance_unscaled,
          dispersion_sum = dispersion_sum,
          df.residual = n_ok - q.r$rank,
          # df.null = n_ok - as.integer(intercept)
          df.null = n_ok - 1L,
          total_sample_size = total_sample_size,
          deviance = deviance,
          # null.deviance = null.deviance,
          aic = aic + 2 * q.r$rank,
          beta_names = beta_names,
          max_gradient = max(abs(gradient)))
        readr::write_rds(final_beta_list, final_file)
        return(final_file)
      }
      beta = beta + gradient
      beta_list = list(
        setup = formula_list,
        beta = beta,
        coefficients = as.numeric(beta),
        previous_gradient = gradient,
        total_sample_size = total_sample_size,
        num_iterations = iteration_number,
        max_iterations = max_iterations,
        iteration_number_next = iteration_number +  1,
        tolerance = tolerance,
        epsilon = epsilon,
        A_mat = A_mat,
        rank = q.r$rank,
        n_ok = n_ok,
        covariance = covariance,
        covariance_unscaled = covariance_unscaled,
        dispersion_sum = dispersion_sum,
        df.residual = n_ok - q.r$rank,
        df.null = n_ok - 1L,
        deviance = deviance,
        # null.deviance = null.deviance,
        aic = aic + 2 * q.r$rank,
        beta_names = beta_names
      )
      readr::write_rds(beta_list, out_beta_file)
      rm(beta_list)
      return(out_beta_file)

    }
  } else {
    if (!all(fe)) {
      print("Waiting for other sites to create gradients")
      # print("Missing files:")
      # print(all_gradient_files[!fe])
    }
  }

}



#' Clear Out Model and Other Helper Functions
#'
#' @param model_name name of your model
#' @param synced_folder synced folder to do computation
#'
#' @return No return value, called for side effects.
#' @export
#' @examples
#' synced_folder = tempfile()
#' dir.create(synced_folder)
#' model_name = "logistic_example"
#' form_file = setup_model(model_name = model_name,
#'                         synced_folder = synced_folder,
#'                         formula =  y ~ x1 + x2, family =  binomial())
#' fnames = folder_names(synced_folder)
#' model_output_file(model_name, synced_folder)
#' master_beta_file(model_name, synced_folder)
#' get_current_beta(model_name, synced_folder)
#' clear_model(model_name, synced_folder)
clear_model = function(
  model_name, synced_folder
) {

  stopifnot(length(model_name) == 1)
  file_list = folder_names(synced_folder)
  files = sapply(file_list, function(x) {
    list.files(path = x,
               pattern = paste0("^", model_name, ".*.rds"),
               full.names = TRUE)
  })
  file.remove(unlist(files))
  return(invisible(NULL))
}


#' @rdname clear_model
#' @export
folder_names = function(synced_folder) {
  L = list(
    # this structure is the same on all sites
    model_folder = file.path(synced_folder, "formulas"),
    gradients_folder = file.path(synced_folder, "gradients"),
    beta_folder = file.path(synced_folder, "betas"),
    converged_folder = file.path(synced_folder, "models")
  )
  return(L)
}

#' @rdname clear_model
#' @export
model_output_file = function(
  model_name, synced_folder
) {
  stopifnot(length(model_name) == 1)

  file_list = folder_names(synced_folder)
  converged_folder = file_list$converged_folder
  final_file = file.path(converged_folder, paste0(model_name, ".rds"))
  final_file
}


#' @rdname clear_model
#' @export
master_beta_file = function(model_name, synced_folder) {
  stopifnot(length(model_name) == 1)
  file_list = folder_names(synced_folder)
  beta_folder = file_list$beta_folder

  all_beta_files = list.files(
    beta_folder,
    pattern = paste0("^", model_name, "-iteration.*.rds"),
    full.names = TRUE)
  if (length(all_beta_files) == 0) {
    beta = NULL
    iteration_number = 1
  } else {
    beta_number = sub(".*iteration(.*)[.]rds", "\\1",
                      basename(all_beta_files))
    beta_number = as.numeric(beta_number)
    beta_list = read_rds(all_beta_files[ which.max(beta_number)])
    beta = beta_list$beta
    iteration_number = beta_list$iteration_number_next
  }
  out_beta_file = file.path(
    beta_folder,
    paste0(
      model_name,
      sprintf("-iteration%04.0f", iteration_number),
      ".rds")
  )
  return(out_beta_file)
}

#' @rdname clear_model
#' @export
get_current_beta = function(model_name, synced_folder) {
  stopifnot(length(model_name) == 1)
  file_list = folder_names(synced_folder)
  beta_folder = file_list$beta_folder
  all_beta_files = list.files(
    beta_folder,
    pattern = paste0("^", model_name, "-iteration.*.rds"),
    full.names = TRUE)

  if (length(all_beta_files) == 0) {
    beta = NULL
    rank = n_ok = dispersion_sum = covariance_unscaled = covariance = NULL
    iteration_number = 1
    L = list(
      iteration_number = 1
    )
  } else {
    beta_number = sub(".*iteration(.*)[.]rds", "\\1",
                      basename(all_beta_files))
    beta_number = as.numeric(beta_number)
    beta_list = read_rds(all_beta_files[ which.max(beta_number)])
    L = beta_list
    # beta = beta_list$beta
    # rank = beta_list$rank
    # n_ok = beta_list$n_ok
    # dispersion_sum = beta_list$dispersion_sum
    # covariance = beta_list$covariance
    # covariance_unscaled = beta_list$covariance_unscaled
    # iteration_number = beta_list$iteration_number_next
  }

  # L$beta =  beta
  # L$covariance =  covariance
  # L$covariance_unscaled =  covariance_unscaled
  # L$rank = rank
  # L$n_ok = n_ok
  # L$dispersion_sum = dispersion_sum
  # L$df.residual = n_ok - rank
  # L$model_name = model_name
  return(L)
}

#' @rdname clear_model
#' @param iteration_number number of fitting iteration, used for tracking
#' @export
get_beta = function(model_name, synced_folder, iteration_number) {
  stopifnot(length(model_name) == 1)

  file_list = folder_names(synced_folder)
  beta_folder = file_list$beta_folder

  out_beta_file = file.path(
    beta_folder,
    paste0(
      model_name,
      sprintf("-iteration%04.0f", iteration_number),
      ".rds"))
  if (!file.exists(out_beta_file)) {
    stop(paste0("Iteration ", iteration_number,
                " for model ", model_name, " does not exist"))
  }
  beta_list = readr::read_rds(out_beta_file)
  return(beta_list)
}




#' @rdname setup_model
#' @param link link function to use with family
#' @export
make_family = function(family, link = NULL) {
  if (is.character(family)) {
    family = get(family, envir = .BaseNamespaceEnv)
  }
  if (is.function(family)) {
    if (is.null(link)) {
      family = family()
    } else {
      family = family(link = link)
    }
  }
  return(family)
}

#' @rdname estimate_site_gradient
#' @param wait_time Time, in seconds, to wait until to try to
#' get new estimate
#' @param run_compute if \code{TRUE}, when estimating the model, it will also
#' try to run \code{\link{estimate_new_beta}} if all other sites have submitted.
#' This allows all sites to be a potential computation site.
#' @export
estimate_model = function(
  model_name, synced_folder,
  site_name = "site1",
  data,
  all_site_names = NULL,
  shuffle_rows = TRUE,
  wait_time = 1,
  run_compute = FALSE,
  experimental = FALSE) {

  final_file = model_output_file(model_name, synced_folder)

  while (!file.exists(final_file)) {
    gradient_file = estimate_site_gradient(
      model_name = model_name,
      synced_folder = synced_folder,
      site_name = site_name,
      data = data,
      all_site_names = all_site_names,
      shuffle_rows = shuffle_rows,
      experimental = experimental)
    Sys.sleep(wait_time)
    if (run_compute) {
      final_file = model_output_file(model_name, synced_folder)
      if (!file.exists(final_file)) {
        run = estimate_new_beta(
          model_name = model_name,
          synced_folder = synced_folder,
          all_site_names = all_site_names)
      }
    }
  }
  out = readr::read_rds(final_file)
  return(out)
}

#' @rdname estimate_new_beta
#' @param wait_time Time, in seconds, to wait until to try to
#' get new estimate
#' @export
compute_model = function(
  model_name, synced_folder,
  all_site_names = NULL,
  wait_time = 5
) {
  final_file = model_output_file(model_name, synced_folder)

  while (!file.exists(final_file)) {
    run = estimate_new_beta(
      model_name,
      synced_folder,
      all_site_names = all_site_names)
    Sys.sleep(wait_time)
  }
  result = readr::read_rds(final_file)
  return(result)
}

#' @rdname estimate_new_beta
#' @export
model_trace = function(
  model_name, synced_folder) {

  stopifnot(length(model_name) == 1)

  file_list = folder_names(synced_folder)
  beta_folder = file_list$beta_folder
  converged_folder = file_list$converged_folder


  final_file = file.path(converged_folder,
                         paste0(model_name, ".rds"))
  out_beta_file = file.path(
    beta_folder,
    paste0(
      model_name,
      sprintf("-iteration%04.0f", 1:1000),
      ".rds")
  )
  out_beta_file = out_beta_file[file.exists(out_beta_file)]

  if (file.exists(final_file)) {
    out_beta_file = c(out_beta_file, final_file)
  }
  names(out_beta_file) = basename(out_beta_file)
  out = lapply(out_beta_file, readr::read_rds)
  return(out)
}

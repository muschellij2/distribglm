library(plumber)
library(distribglm)
library(readr)

# data source is somewhere else! PRIVATE DATA

synced_folder = "~/plumber_models"
dir.create(synced_folder, recursive = TRUE)

#* @apiTitle Run Distributed GLM
#* @apiDescription Allows for the running of distributed generalized Linear models

#* Setup Model and Formula
#*
#* @param model_name:character name of your model
#* @param clear_model:bool Should the model be cleared (all files deleted model with same name) before creating new model
#* @param formula:character model formula to fit, with tilde syntax
#* @param family:character generalized linear model family
#* @param link:character link function for family
#*
#* @response A list of the specification and if the file exists
#* @put /setup_model
function(model_name,
         clear_model = TRUE,
         formula = "y ~ x1 + x2",
         family = "binomial",
         all_site_names,
         link = NULL) {

  family = make_family(family = family, link = link)
  char_formula = formula
  formula = as.formula(formula)
  file = setup_model(model_name = model_name,
                     synced_folder = synced_folder,
                     clear_model = clear_model,
                     formula = formula,
                     family = family,
                     all_site_names = all_site_names)
  L = list(
    formula = char_formula,
    family = family$family,
    link = family$link,
    file = file,
    file_created = file.exists(file),
    model_name = model_name,
    all_site_names = all_site_names
  )
  return(L)
}

#* Clears out a model
#* @param model_name:character name of your model
#*
#* @response Indication if models cleared
#* @put /clear_model
function(model_name) {

  stopifnot(length(model_name) == 1)
  clear_model(model_name = model_name,
                     synced_folder = synced_folder)
  msg = paste0("Model ", model_name, " has been cleared")
  return(msg)
}

#* Get Available Models
#*
#* @response A vector of the model names
#* @get /available_models
#* @put /available_models
function() {

  file_list = folder_names(synced_folder)
  model_folder = file_list$model_folder

  rds = list.files(pattern = ".rds",
                   path = model_folder,
                   full.names = TRUE)
  if (length(rds) == 0) {
    model_names = NULL
  } else {
    model_names = sapply(rds, function(x)
      readr::read_rds(x)$model_name)
  }
  return(model_names)
}


#* Get Model Specification
#*
#* @param model_name name of your model
#*
#* @response A list of the specification and if the file exists
#* @get /model_specification
function(model_name) {

  file_list = folder_names(synced_folder)
  model_folder = file_list$model_folder

  # which model are we running
  formula_file = file.path(model_folder,
                           paste0(model_name, ".rds"))
  if (!file.exists(formula_file)) {
    stop("Model has not been created!  Run /setup_model")
  }
  result = readr::read_rds(formula_file)
  formula = result$formula
  all_site_names = result$all_site_names
  formula = as.character(formula)
  formula = trimws(formula)
  formula = formula[ formula != "~"]
  formula = paste0(formula[1], " ~ ",
                   paste0(formula[-1], collapse = " + "))

  family = result$family
  L = list(
    family = family$family,
    link = family$link,
    file = formula_file,
    formula = formula,
    all_site_names = all_site_names,
    file_created = TRUE
  )
  return(L)
}

#* Get Current beta
#*
#* @param model_name:character name of your model
#* @response A list of beta coefficients and the iteration number
#* @get /get_current_beta
function(model_name) {
  file_list = folder_names(synced_folder)
  model_folder = file_list$model_folder
  converged_folder = file_list$converged_folder

  # which model are we running
  formula_file = file.path(model_folder,
                           paste0(model_name, ".rds"))
  if (!file.exists(formula_file)) {
    stop("Model has not been created!  Run /setup_model")
  }

  formula_list = readr::read_rds(formula_file)
  formula = formula_list$formula
  formula = as.character(formula)
  formula = trimws(formula)
  formula = formula[ formula != "~"]
  formula = paste0(formula[1], " ~ ",
                   paste0(formula[-1], collapse = " + "))

  family = formula_list$family

  print(formula_list)
  run = estimate_new_beta(
    model_name = model_name,
    synced_folder = synced_folder,
    all_site_names = formula_list$all_site_names)
  result = get_current_beta(
    model_name = model_name,
    synced_folder = synced_folder)
  result$formula = formula
  result$family = family$family
  result$link = family$link
  result$file = formula_file
  result$all_site_names = formula_list$all_site_names

  final_file = file.path(converged_folder,
                         paste0(model_name, ".rds"))
  result$converged = file.exists(final_file)
  result$all_site_names = formula_list$all_site_names
  result = jsonlite::toJSON(result, digits = 20)
  return(result)
}






#* Submit a set of estimated values, including the gradient
#*
#* @param model_name:character name of your model
#* @param site_name:character Name of the site
#* @param A:numeric vector of A, matrix
#* @param u:numeric u vector
#* @param n_ok number of samples
#* @param dispersion_sum sum of the dispersion
#* @param iteration_number:int number of fitting iteration, used for tracking
#* @response A message saying the file was created
#* @put /submit_gradient
function(A, u, n_ok, site_name,
         dispersion_sum,
         model_name,
         iteration_number) {

  # cat("Stuff")
  # print(A)
  # print(u)
  # print(n_ok)
  class(A) = "numeric"
  class(u) = "numeric"
  gradient_list = list(A_mat = A,
                       n_ok = n_ok,
                       u = u,
                       site_name = site_name,
                       iteration_number = iteration_number
  )

  file_list = folder_names(synced_folder)
  gradients_folder = file_list$gradients_folder

  gradient_file = file.path(
    gradients_folder,
    paste0(model_name, "-",
           site_name,
           sprintf("-iteration%04.0f", iteration_number),
           ".rds"))
  readr::write_rds(gradient_list, gradient_file)
  return("File was created")
}



#* Submit a list of estimated values, including the gradient
#*
#* @param model_name:character name of your model
#* @param site_name:character Name of the site
#* @param gradient_list A list of the gradient, from gradient_value function
#* @param iteration_number:int number of fitting iteration, used for tracking
#* @response A message saying the file was created
#* @put /submit_gradient_list
function(gradient_list, site_name = NULL,
         model_name = NULL,
         iteration_number = NULL) {
  if (is.null(iteration_number)) {
    iteration_number = gradient_list$iteration_number
  }
  if (!is.null(iteration_number) &&
      !is.null( gradient_list$iteration_number)) {
    if (gradient_list$iteration_number != iteration_number) {
      stop(paste0("Gradient list iteration_number and ",
                  "specified iteration_number not the same, error"))
    }
  }

  stopifnot(!is.null(iteration_number))
  if (is.null(model_name)) {
    model_name = gradient_list$model_name
  }
  stopifnot(!is.null(model_name))

  if (is.null(site_name)) {
    site_name = gradient_list$site_name
  }
  stopifnot(!is.null(site_name))

  file_list = folder_names(synced_folder)
  gradients_folder = file_list$gradients_folder

  file_list = folder_names(synced_folder)
  gradients_folder = file_list$gradients_folder

  gradient_file = file.path(
    gradients_folder,
    paste0(model_name, "-",
           site_name,
           sprintf("-iteration%04.0f", iteration_number),
           ".rds"))
  class(gradient_list$A_mat) = "numeric"
  class(gradient_list$u) = "numeric"
  class(gradient_list$gradient) = "numeric"
  readr::write_rds(gradient_list, gradient_file)
  return(paste0("File ", gradient_file, " was created"))
}


#* Check Model Convergence
#*
#* @param model_name:character name of your model
#* @response A message saying the model converged or not and a list of results
#* @get /model_converged
function(model_name) {
  file_list = folder_names(synced_folder)
  converged_folder = file_list$converged_folder


  final_file = file.path(converged_folder,
                         paste0(model_name, ".rds"))
  if (file.exists(final_file)) {
    out = readr::read_rds(final_file)
    out$converged = TRUE
  } else {
    out = list(converged = FALSE)
  }
  out$model_name = model_name
  out = jsonlite::toJSON(out, digits = 20)
  return(out)
}

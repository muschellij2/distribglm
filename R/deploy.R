
#' Deploy GLM API on Digital Ocean (DO)
#'
#' @param ... arguments to pass to \code{do_provision} from
#' \code{plumber} package
#' @param application_name Name of application, passed to \code{path}
#' argument of \code{do_deploy_api} function from \code{plumber} package
#' @param port port to deploy on Digital Ocean
#' @param swagger enable the Swagger interface,
#' passed to \code{do_deploy_api} function from
#' \code{plumber} package
#' @param forward setup requests targeting the root URL on the
#' server to point to this application,
#' passed to \code{do_deploy_api} function from
#' \code{plumber} package
#'
#' @return A droplet instance
#' @export
do_deploy_glm_api = function(...,
                             application_name = "glm",
                             port = 8000,
                             swagger = TRUE,
                             forward = TRUE) {
  if (!requireNamespace("analogsea", quietly = TRUE) ||
      requireNamespace("plumber", quietly = TRUE)) {
    stop("analogsea and plumber necessary for deploying")
  }
  droplet <- plumber::do_provision(...)
  analogsea::install_r_package(droplet,
                               c("readr", "remotes",
                                 "httr", "jsonlite") )
  install_github_r_package = function(droplet, repo) {
    analogsea::droplet_ssh(
      droplet,
      sprintf("Rscript -e \"remotes::install_github('%s')\"",
              repo))
  }

  droplet_ls = function(droplet, path) {
    analogsea::droplet_ssh(
      droplet,
      sprintf("ls %s", path))
  }


  install_github_r_package(droplet, "muschellij2/distribglm")
  local_file = system.file("extdata/plumber.R", package = "distribglm")

  tdir = tempfile()
  tdir = path.expand(tdir)
  dir.create(tdir, recursive = TRUE)
  file.copy(local_file, file.path(tdir, "plumber.R"))

  plumber::do_deploy_api(
    droplet,
    path = application_name,
    localPath = tdir,
    port = port,
    swagger = TRUE,
    forward = TRUE)
}

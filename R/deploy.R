install_github_r_package = function(droplet, repo) {
  analogsea::droplet_ssh(
    droplet,
    sprintf("Rscript -e \"remotes::install_github('%s')\"",
            repo))
}

deploy_check = function() {
  if (!requireNamespace("analogsea", quietly = TRUE) ||
      !requireNamespace("plumber", quietly = TRUE)) {
    stop("analogsea and plumber necessary for deploying")
  }
}

droplet_capture = function(droplet, command) {
  tf <- tempdir()
  randName <- paste(sample(c(letters, LETTERS), size = 10,
                           replace = TRUE), collapse = "")
  tff <- file.path(tf, randName)
  on.exit({
    if (file.exists(tff)) {
      file.remove(tff)
    }
  })
  analogsea::droplet_ssh(droplet, paste0(command, " > /tmp/",
                                         randName), verbose = FALSE)
  analogsea::droplet_download(droplet, paste0("/tmp/", randName),
                              tf, verbose = FALSE)
  analogsea::droplet_ssh(droplet, paste0("rm /tmp/", randName),
                         verbose = FALSE)

  have_remotes <- readLines(tff, warn = FALSE)
  return(have_remotes)
}

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
#' @param example If TRUE, will deploy an example API
#' named hello to the server on port 8000.
#'
#' @return A droplet instance
#' @rdname deploy
#' @export
do_provision_glm_api = function(
  ...,
  application_name = "glm",
  port = 8000,
  example = FALSE) {

  deploy_check()
  if (example && port == 8000) {
    stop("You chose to load the example, but also use port ",
         "8000, this will cause failures, please change port")
  }
  droplet <- plumber::do_provision(..., example = example)
  analogsea::install_r_package(droplet, c("readr", "remotes"))

  droplet_apt_install = function(droplet, pack, update = TRUE) {
    cmd = ""
    if (update) {
      cmd = "sudo apt-get update -q  && "
    }
    cmd = sprintf(paste0(cmd, " sudo apt-get install -y %s "),
                  pack)
    analogsea::droplet_ssh(droplet,cmd)
  }
  droplet_apt_install(droplet, "libcurl4-openssl-dev")
  droplet_apt_install(droplet, "libssl-dev", update = FALSE)
  analogsea::install_r_package(droplet, c("httr", "jsonlite"))


  droplet_ls = function(droplet, path) {
    analogsea::droplet_ssh(
      droplet,
      sprintf("ls %s", path))
  }


  install_github_r_package(droplet, "muschellij2/distribglm")

  droplet$application_name = application_name
  droplet
}


#' @param droplet droplet to deploy on
#' @export
#' @rdname deploy
do_deploy_glm_api = function(
  ...,
  application_name = "glm",
  port = 8000,
  swagger = TRUE,
  forward = TRUE,
  example = FALSE
) {

  droplet = do_provision_glm_api(
    ...,
    port = port,
    example = example,
    application_name = application_name)
  res = do_deploy_glm_api_only(
    droplet = droplet,
    application_name = application_name,
    port = port,
    swagger = swagger,
    forward = forward)
  res$application_name = application_name
  res
}


#' @param droplet droplet to deploy on
#' @export
#' @rdname deploy
do_deploy_glm_api_only = function(
  droplet,
  application_name = "glm",
  port = 8000,
  swagger = TRUE,
  forward = TRUE) {
  deploy_check()

  install_github_r_package(droplet, "muschellij2/distribglm")

  local_file = system.file("extdata/plumber.R",
                           package = "distribglm")

  tdir = tempfile()
  tdir = path.expand(tdir)
  dir.create(tdir, recursive = TRUE)
  file.copy(local_file, file.path(tdir, "plumber.R"))

  res = plumber::do_deploy_api(
    droplet,
    path = application_name,
    localPath = tdir,
    port = port,
    swagger = swagger,
    forward = forward)
  res$application_name = application_name
  res
}


#' @export
#' @rdname deploy
do_list_plumber = function(droplet) {

  deploy_check()
  out = droplet_capture(droplet, "systemctl | grep plumber")
  return(out)

}

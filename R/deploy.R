deploy_check = function() {
  if (!requireNamespace("analogsea", quietly = TRUE) ||
      !requireNamespace("plumberDeploy", quietly = TRUE)) {
    stop("analogsea and plumberDeploy necessary for deploying")
  }
}

droplet_capture = function(droplet, command, ...) {
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
                                         randName), verbose = FALSE, ...)
  analogsea::droplet_download(droplet, paste0("/tmp/", randName),
                              tf, verbose = FALSE, ...)
  analogsea::droplet_ssh(droplet, paste0("rm /tmp/", randName),
                         verbose = FALSE, ...)

  have_remotes <- readLines(tff, warn = FALSE)
  return(have_remotes)
}


#' Deploy GLM API on Digital Ocean (DO)
#'
#' @param ... arguments to pass to \code{do_provision} from
#' \code{plumberDeploy} package
#' @param application_name Name of application, passed to \code{path}
#' argument of \code{do_deploy_api} function from \code{plumberDeploy} package
#' @param port port to deploy on Digital Ocean
#' @param docs enable the Swagger interface,
#' passed to \code{do_deploy_api} function from
#' \code{plumberDeploy} package
#' @param forward setup requests targeting the root URL on the
#' server to point to this application,
#' passed to \code{do_deploy_api} function from
#' \code{plumberDeploy} package
#' @param example If TRUE, will deploy an example API
#' named hello to the server on port 8000.
#' @param r_packages Additional R packages to install, using
#' \code{install.packages}
#' @param github_r_packages Additional R packages to install from GitHub, using
#' \code{remotes::install_github}
#'
#'
#' @return A droplet instance
#' @rdname deploy
#' @export
#' @examples
#' \dontrun{
#'   d = analogsea::droplets()
#'   if (length(d) == 0) {
#'     droplet = NULL
#'   } else {
#'     droplet = d[[1]]
#'   }
#'   droplet = do_provision_glm_api(droplet = droplet, region = "sfo3")
#'   droplet = do_deploy_glm_api_only(droplet)
#'   ip = droplet$network$v4[[1]]$ip_address
#'   applet_url = paste0("http://", ip, "/", droplet$application_name,
#'                       "/__docs__/")
#'   if (interactive()) {
#'     browseURL(applet_url)
#'   }
#' }
do_provision_glm_api = function(
  ...,
  application_name = "glm",
  port = 8000,
  example = FALSE,
  r_packages = NULL,
  github_r_packages = NULL) {

  deploy_check()
  if (example && port == 8000) {
    stop("You chose to load the example, but also use port ",
         "8000, this will cause failures, please change port")
  }
  args = list(...)
  # these args are generally for droplet_ssh
  ssh_args = list(
    user = args$user %||% "root",
    keyfile = args$keyfile,
    ssh_passwd = args$ssh_passwd,
    verbose = args$verbose %||% FALSE
  )
  # these don't get passed to analogsea::droplet_create for now
  # with > 0.3
  # args$user = args$keyfile = args$ssh_passwd = args$verbose =  NULL

  args$example = example
  droplet <- do.call(plumberDeploy::do_provision, args = args)

  ssh_args$droplet = droplet
  ssh_args$package = c("readr", "remotes")
  do.call(analogsea::install_r_package,args = ssh_args)
  ssh_args$package = NULL

  droplet_apt_install = function(droplet, pack, update = TRUE) {
    cmd = ""
    if (update) {
      cmd = "sudo apt-get update -qq  && "
    }
    cmd = sprintf(paste0(cmd, " sudo apt-get install -y %s "),
                  pack)
    run_args = c(cmd, ssh_args)
    do.call(analogsea::droplet_ssh, args = run_args)
    # analogsea::droplet_ssh(droplet,cmd)
  }
  droplet_apt_install(droplet, "libcurl4-openssl-dev")
  droplet_apt_install(droplet, "libssh-dev")

  droplet_apt_install(droplet, "libssl-dev", update = FALSE)

  # analogsea::install_r_package(
  #   droplet,
  #   unique(c("httr", "jsonlite", r_packages)))
  ssh_args$package = unique(c("httr", "jsonlite"), r_packages)
  do.call(
    analogsea::install_r_package,
    args = ssh_args)
  ssh_args$package = NULL

  droplet_ls = function(droplet, path) {
    run_args = c(ssh_args, sprintf("ls %s", path))
    do.call(analogsea::droplet_ssh, args = run_args)
    # analogsea::droplet_ssh(
    #   droplet,
    #   sprintf("ls %s", path))
  }

  ssh_args$package = unique(c("muschellij2/distribglm", github_r_packages))
  do.call(
    analogsea::install_github_r_package,
    args = ssh_args)
  ssh_args$package = NULL
  # install_github_r_package(
  #   droplet,
  #   unique(c("muschellij2/distribglm", github_r_packages)))

  droplet$application_name = application_name
  droplet
}

#' @export
#' @rdname deploy
do_remove_glm_api = function(
  droplet,
  application_name = "glm",
  ...) {

  deploy_check()

  analogsea::droplet_ssh(
    droplet,
    paste("rm -rf",
          paste0("/var/plumber/", application_name, "/*")),
    ...)

  app_name = paste0("plumber-", application_name)
  analogsea::droplet_ssh(
    droplet,
    sprintf("(systemctl stop %s || true) && sleep 1", app_name),
    ...)
  analogsea::droplet_ssh(
    droplet,
    sprintf("(systemctl disable %s || true) && sleep 1", app_name),
    ...)


  analogsea::droplet_ssh(
    droplet,
    paste("rm -rf",
          paste0("/etc/systemd/system/",
                 app_name, ".service")),
    ...)
  analogsea::droplet_ssh(
    droplet,
    paste("rm -rf",
          paste0("/etc/nginx/sites-available/plumber-apis/",
                 application_name, ".conf")),
    ...)
  analogsea::droplet_ssh(droplet, "systemctl reload nginx",
                         ...)
  return(droplet)
}



#' @param droplet droplet to deploy on
#' @export
#' @rdname deploy
do_deploy_glm_api = function(
  ...,
  application_name = "glm",
  port = 8000,
  docs = TRUE,
  forward = TRUE,
  example = FALSE
) {

  deploy_check()
  droplet = do_provision_glm_api(
    ...,
    port = port,
    example = example,
    application_name = application_name)
  res = do_deploy_glm_api_only(
    droplet = droplet,
    application_name = application_name,
    port = port,
    docs = docs,
    forward = forward)
  res
}


#' @param droplet droplet to deploy on
#' @export
#' @rdname deploy
do_deploy_glm_api_only = function(
  droplet,
  application_name = "glm",
  port = 8000,
  docs = TRUE,
  forward = TRUE,
  ...) {
  deploy_check()

  analogsea::install_github_r_package(droplet, "muschellij2/distribglm", ...)

  local_file = system.file("extdata/plumber.R",
                           package = "distribglm")

  tdir = tempfile()
  tdir = path.expand(tdir)
  dir.create(tdir, recursive = TRUE)
  file.copy(local_file, file.path(tdir, "plumber.R"))

  try({
    plumberDeploy::do_remove_api(droplet, path = application_name,
                                 delete = TRUE, ...)
  }, silent = TRUE)
  res = plumberDeploy::do_deploy_api(
    droplet,
    path = application_name,
    localPath = tdir,
    port = port,
    docs = docs,
    forward = forward, ...)
  res$application_name = application_name
  res$port = port
  res$docs = docs
  res
}


#' @export
#' @rdname deploy
do_list_plumber = function(droplet, ...) {

  deploy_check()
  out = droplet_capture(droplet, "systemctl | grep plumber", ...)
  return(out)

}

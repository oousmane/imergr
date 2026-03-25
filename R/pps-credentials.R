# ==============================================================================
# PPS CREDENTIALS
# ==============================================================================
#
# PPS uses your registered email as BOTH username and password.
# Register at: https://registration.pps.eosdis.nasa.gov/registration/
#
# Credentials are stored in ~/.Renviron (persistent across sessions).
# The key name is PPS_EMAIL.
# ==============================================================================

#' Set your NASA PPS email credential
#'
#' Writes PPS_EMAIL to ~/.Renviron for persistent storage across sessions.
#' Your registered email serves as both username and password on the PPS server.
#'
#' @param email Character. Your registered PPS email address.
#'   Register at https://registration.pps.eosdis.nasa.gov/registration/
#'
#' @return Invisible. Prints a confirmation message.
#'
#' @examples
#' \dontrun{
#' set_pps_key("your@email.com")
#' }
#'
set_pps_key <- function(email) {
  if (!grepl("^[^@]+@[^@]+\\.[^@]+$", email))
    stop("Invalid email address: ", email)

  renviron_path <- path.expand("~/.Renviron")

  # Read existing .Renviron lines (create if missing)
  lines <- if (file.exists(renviron_path)) {
    readLines(renviron_path, warn = FALSE)
  } else {
    character(0)
  }

  # Remove any existing PPS_EMAIL entry
  lines <- lines[!grepl("^PPS_EMAIL=", lines)]

  # Append new entry
  lines <- c(lines, paste0("PPS_EMAIL=", email))
  writeLines(lines, renviron_path)

  # Apply immediately to current session
  Sys.setenv(PPS_EMAIL = email)

  message("PPS_EMAIL set to: ", email)
  message("Saved in: ", renviron_path)
  message("Active in current session.")
  invisible(email)
}


#' Get your NASA PPS email credential
#'
#' Reads PPS_EMAIL from the environment. Stops with a clear message
#' if the credential is not set.
#'
#' @param silent Logical. If TRUE, returns NULL instead of stopping
#'   when credential is missing (default FALSE).
#'
#' @return Character. Your registered PPS email address, or NULL if
#'   silent = TRUE and credential is not set.
#'
#' @examples
#' \dontrun{
#' get_pps_key()
#' }
get_pps_key <- function(silent = FALSE) {
  email <- Sys.getenv("PPS_EMAIL")

  if (nchar(email) == 0) {
    if (silent) return(invisible(NULL))
    stop(
      "PPS credential not found.\n",
      "Run: set_pps_key(\"your@email.com\")\n",
      "Register at: https://registration.pps.eosdis.nasa.gov/registration/"
    )
  }

  email
}

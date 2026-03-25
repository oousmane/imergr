#' Download one IMERG 30-min slot from a prebuilt URL
#'
#' Low-level worker called by [get_imerg()]. Use this function directly only
#' when you already have the URL (e.g. from a batch listing script).
#' Skips the download silently if the output file already exists.
#'
#' @param url Character. Full HTTPS URL to a PPS GIS `.zip` file.
#'   Typically built by the internal \code{.build_imerg_url()} function,
#'   or supplied manually from a PPS directory listing.
#' @param out_dir Character. Root output directory. Files are written
#'   directly into this folder (default \code{"data"}).
#' @param format Character. Output format, one of:
#'   \describe{
#'     \item{\code{"netcdf"}}{CF-1.7 compliant \code{.nc} file with
#'       \code{longitude}, \code{latitude}, and \code{time} dimensions.
#'       Compatible with CDO, xarray, ncdf4, and terra. (default)}
#'     \item{\code{"geotiff"}}{LZW-compressed GeoTIFF \code{.tif} with
#'       PPS scale factor applied (raw / 10 = mm).}
#'   }
#'
#' @return Invisible character. Absolute path to the output \code{.nc}
#'   or \code{.tif} file.
#'
#' @seealso [get_imerg()] for the preferred high-level interface,
#'   [set_pps_key()] to configure credentials.
#'
#' @examples
#' \dontrun{
#' # Requires a valid PPS credential — set once with:
#' # set_pps_key("your@email.com")
#'
#' # Supply a URL directly
#' url <- paste0(
#'   "https://jsimpsonhttps.pps.eosdis.nasa.gov/imerg/gis/",
#'   "2021/07/",
#'   "3B-HHR-L.MS.MRG.3IMERG.20210715-S063000-E065959.0390.V07B.30min.zip"
#' )
#'
#' # Download as NetCDF (default)
#' nc <- fetch_imerg(url, out_dir = "data/imerg")
#'
#' # Download as GeoTIFF
#' tif <- fetch_imerg(url, out_dir = "data/imerg", format = "geotiff")
#'
#' # Read result with terra
#' r <- terra::rast(nc)
#' terra::plot(r)
#' }
#'
#' @export
#'
fetch_imerg <- function(url, out_dir = "data", format = "netcdf") {
  format <- match.arg(format, c("netcdf", "geotiff"))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Determine expected output path and skip if already present
  fname    <- basename(url)
  out_ext  <- if (format == "netcdf") ".nc" else ".tif"
  out_name <- sub("\\.zip$", out_ext, fname)
  out_path <- file.path(out_dir, out_name)

  if (file.exists(out_path)) {
    message("Already exists: ", out_name)
    return(invisible(out_path))
  }

  # Download and unzip
  zip_path <- file.path(out_dir, fname)
  .download_zip(url, zip_path)
  utils::unzip(zip_path, exdir = out_dir)
  file.remove(zip_path)

  tif_path <- utils::tail(list.files(out_dir, pattern = "\\.tif$",
                               full.names = TRUE), 1)
  dt       <- .parse_imerg_dt(fname)

  # Convert to requested format
  out_path <- switch(format,
    netcdf = {
      nc_path <- .tif_to_nc(tif_path, dt)
      .cleanup_aux(tif_path, keep_ext = ".nc")
      nc_path
    },
    geotiff = {
      .finalize_tif(tif_path)
      .cleanup_aux(tif_path, keep_ext = ".tif")
      tif_path
    }
  )

  invisible(out_path)
}


#' Download IMERG 30-min precipitation for a given date and time slot
#'
#' Main user-facing function of \pkg{imergr}. Builds the PPS download URL
#' from the supplied parameters, fetches the GeoTIFF ZIP from the PPS server,
#' applies the scale factor, and saves the result as NetCDF or GeoTIFF.
#' Skips the download silently if the output file already exists, making it
#' safe to interrupt and resume batch downloads.
#'
#' @param date Character or Date. The date of the slot in \code{"YYYY-MM-DD"}
#'   format, or a \code{Date} object.
#' @param hour Integer (0--23). The UTC hour of the 30-min slot.
#' @param min Integer. Minutes within the hour: \code{0} (slot \code{HH:00}
#'   to \code{HH:29}) or \code{30} (slot \code{HH:30} to \code{HH:59}).
#' @param run Character. IMERG processing run:
#'   \describe{
#'     \item{\code{"late"}}{~14-hour latency, higher quality. Recommended
#'       for most applications. (default)}
#'     \item{\code{"early"}}{~4-hour latency, near real-time. Useful for
#'       operational monitoring.}
#'   }
#' @param version Character. IMERG algorithm version (default \code{"V07B"}).
#'   See \code{?imergr} for a table of available versions and their
#'   temporal coverage.
#' @param out_dir Character. Root output directory. Created recursively if
#'   it does not exist (default \code{"data"}).
#' @param format Character. Output format, one of:
#'   \describe{
#'     \item{\code{"netcdf"}}{CF-1.7 compliant \code{.nc} file. Compatible
#'       with CDO, xarray, ncdf4, and terra. (default)}
#'     \item{\code{"geotiff"}}{LZW-compressed GeoTIFF \code{.tif}.}
#'   }
#'
#' @return Invisible character. Absolute path to the output file.
#'
#' @seealso [fetch_imerg()] for the low-level interface,
#'   [set_pps_key()] to configure credentials.
#'
#' @examples
#' \dontrun{
#' # Requires a valid PPS credential — set once with:
#' # set_pps_key("your@email.com")
#'
#' # --- Single slot ---
#'
#' # Late run, NetCDF (default)
#' get_imerg("2021-07-15", hour = 6, min = 30)
#'
#' # Early run, GeoTIFF
#' get_imerg("2021-07-15", hour = 6, min = 0, run = "early", format = "geotiff")
#'
#' # Custom output directory
#' get_imerg("2021-07-15", hour = 12, min = 0, out_dir = "data/imerg")
#'
#' # --- All slots for one day (48 files) ---
#' slots <- expand.grid(hour = 0:23, min = c(0, 30)) |>
#'   dplyr::arrange(hour, min)
#'
#' purrr::walk2(slots$hour, slots$min, \(h, m) {
#'   tryCatch(
#'     get_imerg("2021-07-15", hour = h, min = m, out_dir = "data/imerg"),
#'     error = \(e) message("SKIP [", sprintf("%02d:%02d", h, m), "] ",
#'                          conditionMessage(e))
#'   )
#' })
#'
#' # --- Batch: April–October, 2000–2025 ---
#' dates <- seq(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "day")
#' dates <- dates[lubridate::month(dates) %in% 4:10]
#'
#' purrr::walk(dates, \(date) {
#'   purrr::walk2(slots$hour, slots$min, \(h, m) {
#'     tryCatch(
#'       get_imerg(date, hour = h, min = m, out_dir = "data/imerg"),
#'       error = \(e) message("SKIP [", format(date), " ",
#'                            sprintf("%02d:%02d", h, m), "] ",
#'                            conditionMessage(e))
#'     )
#'   })
#' })
#' }
#'
#'  @export
get_imerg <- function(date, hour, min,
                      run     = "late",
                      version = "V07B",
                      out_dir = "data",
                      format  = "netcdf") {
  url <- .build_imerg_url(
    date    = date,
    hour    = hour,
    min     = min,
    run     = run,
    version = version
  )
  fetch_imerg(url, out_dir = out_dir, format = format)
}

# Download a ZIP file from PPS server using curl + email credentials.
# Validates the ZIP magic bytes after download.
# ------------------------------------------------------------------------------

.download_zip <- function(url, zip_path) {
  email <- get_pps_key()

  status <- system2(
    "curl",
    args = c(
      "-u", paste0(email, ":", email),   # PPS: email = user = password
      "-L",                              # follow redirects
      "--fail", "--silent", "--show-error",
      "-o", shQuote(zip_path),
      shQuote(url)
    )
  )

  if (status != 0 || !file.exists(zip_path))
    stop("Download failed: ", basename(url))

  # Validate ZIP magic bytes (PK\003\004)
  con <- file(zip_path, "rb")
  sig <- readBin(con, "raw", 4)
  close(con)

  if (!identical(sig, charToRaw("PK\003\004"))) {
    file.remove(zip_path)
    stop("Invalid ZIP (auth failed or corrupted): ", basename(url))
  }

  invisible(zip_path)
}


# Extract a POSIXct datetime from an IMERG filename.
# Parses the YYYYMMDD-SHHMMSS pattern (start time of the 30-min slot).
#
# Example filename:
#   3B-HHR-L.MS.MRG.3IMERG.20210715-S063000-E065959.0390.V07B.30min.zip
#                            ^^^^^    ^^^^^
#                            date   start time

.parse_imerg_dt <- function(fname) {
  m <- regmatches(fname, regexpr("\\d{8}-S\\d{6}", fname))
  if (length(m) == 0)
    stop("Cannot parse datetime from filename: ", fname)

  lubridate::ymd_hms(
    paste0(
      substr(m, 1,  8), " ",
      substr(m, 11, 12), ":",   # HH
      substr(m, 13, 14), ":",   # MM
      substr(m, 15, 16)         # SS
    ),
    tz = "UTC"
  )
}



# Add CF-1.7 compliant axis and standard_name attributes to coordinate
# variables. Required for CDO, xarray and other tools that rely on these
# attributes to identify spatial and temporal axes.
#
# Note: terra::writeCDF() does not expose parameters to set these attributes
# on coordinate variables — ncdf4 patching is the only available workaround.

.patch_cf_attrs <- function(nc_path) {
  nc <- ncdf4::nc_open(nc_path, write = TRUE)
  on.exit(ncdf4::nc_close(nc))

  ncdf4::ncatt_put(nc, "longitude", "standard_name", "longitude")
  ncdf4::ncatt_put(nc, "longitude", "axis",          "X")
  ncdf4::ncatt_put(nc, "latitude",  "standard_name", "latitude")
  ncdf4::ncatt_put(nc, "latitude",  "axis",          "Y")
  ncdf4::ncatt_put(nc, "time",      "standard_name", "time")
  ncdf4::ncatt_put(nc, "time",      "axis",          "T")
  ncdf4::ncatt_put(nc, "time",      "calendar",      "standard")

  invisible(nc_path)
}



# Convert a PPS GeoTIFF to a CF-1.7 compliant NetCDF.
# Applies the PPS ×0.1 scale factor, assigns the layer datetime,
# writes with writeCDF(), then patches coordinate attributes with ncdf4.

.tif_to_nc <- function(tif_path, dt) {
  r <- terra::rast(tif_path) * 0.1   # 16-bit integer → mm
  terra::time(r) <- dt

  nc_path <- sub("\\.tif$", ".nc", tif_path)

  terra::writeCDF(
    r,
    filename  = nc_path,
    varname   = "precipitation",
    longname  = "IMERG precipitation accumulation",
    unit      = "mm",
    zname     = "time",
    xname     = "longitude",
    yname     = "latitude",
    prec      = "float",
    overwrite = TRUE,
    atts      = c(
      "Conventions=CF-1.7",
      "source=NASA GPM IMERG PPS GIS GeoTIFF",
      "version=V07B"
    )
  )

  .patch_cf_attrs(nc_path)
  invisible(nc_path)
}



# Apply the PPS ×0.1 scale factor and rewrite as a compressed GeoTIFF.
# LZW compression + tiling for efficient spatial access.

.finalize_tif <- function(tif_path) {
  r      <- terra::rast(tif_path) * 0.1
  scaled <- sub("\\.tif$", ".scaled.tif", tif_path)

  terra::writeRaster(
    r, scaled,
    overwrite = TRUE,
    gdal      = c("COMPRESS=LZW", "TILED=YES")
  )

  file.remove(tif_path)
  file.rename(scaled, tif_path)
  invisible(tif_path)
}

# Remove all files sharing the same stem as tif_path, except the file
# matching keep_ext. Cleans up .tfw, .aux.xml, and other sidecar files.

.cleanup_aux <- function(tif_path, keep_ext) {
  stem <- sub("\\.tif$", "", basename(tif_path))
  all  <- list.files(dirname(tif_path), pattern = stem, full.names = TRUE)
  file.remove(all[!grepl(paste0("\\", keep_ext, "$"), all)])
  invisible(NULL)
}



# Construct the HTTPS download URL for a single IMERG 30-min GeoTIFF ZIP.
#
# URL structure:
#   https://jsimpsonhttps.pps.eosdis.nasa.gov/imerg/gis/[early/]YYYY/MM/fname
#
# Filename pattern:
#   {prefix}.MS.MRG.3IMERG.{YYYYMMDD}-S{HHMMSS}-E{HHMMSS}.{MMMM}.{version}.30min.zip

.build_imerg_url <- function(date, hour, min,
                              run = "late", version = "V07B") {
  date <- as.Date(date)
  if (!(min %in% c(0, 30)))
    stop("min must be 0 or 30")

  yyyy   <- format(date, "%Y")
  mm     <- format(date, "%m")
  mmdd   <- format(date, "%m%d")
  prefix <- if (run == "early") "3B-HHR-E" else "3B-HHR-L"
  mm_e   <- if (min == 0) 29 else 59
  s_str  <- sprintf("%02d%02d00", hour, min)
  e_str  <- sprintf("%02d%02d59", hour, mm_e)
  mmmm   <- sprintf("%04d", hour * 60 + min)

  fname <- sprintf(
    "%s.MS.MRG.3IMERG.%s-S%s-E%s.%s.%s.30min.zip",
    prefix, paste0(yyyy, mmdd), s_str, e_str, mmmm, version
  )

  paste0(
    "https://jsimpsonhttps.pps.eosdis.nasa.gov/imerg/gis/",
    if (run == "early") "early/" else "",
    yyyy, "/", mm, "/", fname
  )
}

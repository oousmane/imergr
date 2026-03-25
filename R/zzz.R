.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    "  imergr ", utils::packageVersion("imergr"), "\n",
    "  NASA GPM IMERG download toolkit\n",
    "  source  : PPS GIS (jsimpsonhttps.pps.eosdis.nasa.gov)\n"
  )
}

# =====================================================================
# install_packages.R: installs the R packages needed by the project.
# Do this ONCE per computer (internet required).
# RStudio: open this file, then click "Source".
# =====================================================================
options(timeout = 600)                       # slow internet: wait longer
cat("R version:", as.character(getRversion()), "\n")
if (getRversion() < "4.0.0")
  stop("Your R version is too old. Install R 4.2 or newer (cran.r-project.org).")

needed <- c("jsonlite",   # reads the parameter file (data/parameters.json)
            "testthat",   # automatic checks
            "rmarkdown",  # Knit button: builds the Word / HTML document
            "knitr",      # runs the R code inside the document
            "officer")    # builds the Word deliverables (optional)

missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) == 0) {
  cat("\nEverything is already installed. You can move on to run_all.R.\n")
} else {
  cat("\nInstalling:", paste(missing_pkgs, collapse = ", "), "\n")
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  still <- missing_pkgs[!vapply(missing_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still) == 0) {
    cat("\nDone: all packages are installed.\n")
  } else {
    cat("\nWarning, these packages could not be installed:", paste(still, collapse = ", "),
        "\nCheck your internet connection then run this script again.\n")
  }
}

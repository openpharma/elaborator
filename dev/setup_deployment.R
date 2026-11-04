# Prepare a Posit Connect manifest using Posit Public Package Manager (P3M).
#
# Run from the repository root (directory containing app.R and DESCRIPTION):
#   Rscript dev/setup_deployment.R
#
# Or interactively: source("dev/setup_deployment.R")
options(repos = c(CRAN = sprintf("https://packagemanager.posit.co/cran/latest/bin/linux/jammy-%s/%s", R.version["arch"], as.character(getRversion()[, 1:2]))))


Sys.setenv("RENV_CONFIG_PAK_ENABLED" = TRUE)
Sys.setenv("RENV_CONFIG_PPM_ENABLED" = TRUE)
Sys.setenv("RENV_CONFIG_AUTOLOADER_ENABLED" = FALSE)

# Match your Connect / build image (Ubuntu 22.04 = jammy).
# CRAN <- "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
CRAN <- getOption("repos") |> unname() |> unlist()
# Repositories that should be replaced with P3M when refreshing the library.
INTERNAL_REPOS <- c(
  "https://ppm.titans.int.bayer.com/cran/latest",
  "https://ppm-dev.titans.int.bayer.com/cran/__linux__/jammy/latest",
  "https://ppm-dev.titans.int.bayer.com/cran/latest"
)

usethis::use_package("pkgload", "Imports")

renv::init(repos = CRAN, bare = TRUE)
renv::settings$use.cache(FALSE)
renv::settings$ppm.enabled(TRUE)

purrr::map(
  desc_pkgs, 
  \(x) {
    # renv::install(x, repos = CRAN, prompt = FALSE, rebuild = TRUE)
    utils::install.packages(x, repos = CRAN)
  }
)


renv::snapshot(type = "explicit")

# ---------------------------------------------------------------------------
# 2) Reinstall packages
# ---------------------------------------------------------------------------
# Direct dependencies from DESCRIPTION (good for a minimal library):
desc_pkgs <- description_packages("DESCRIPTION")


rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")



# # Install all packages from Posit Public Package manager
# Sys.setenv("RENV_CONFIG_PAK_ENABLED" = TRUE)
# Sys.setenv("RENV_CONFIG_PPM_ENABLED" = TRUE)
# Sys.setenv("RENV_CONFIG_AUTOLOADER_ENABLED" = FALSE)

# CRAN <- "https://packagemanager.posit.co/cran/__linux__/jammy/latest"

# pkgs <- renv::lockfile_read()$Packages
# df <- tibble::tibble(
#   Package    = names(pkgs),
#   Repository = purrr::map_chr(pkgs, "Repository", .default = NA_character_)
# )

# df |> 
#   # dplyr::filter(!Repository %in% c("CRAN", "RSPM")) |> 
#   dplyr::filter(Repository %in% c("https://ppm-dev.titans.int.bayer.com/cran/__linux__/jammy/latest")) |> 
#   dplyr::pull("Package") |> 
#   purrr::walk(\(x) renv::install(x, repos = CRAN, prompt = FALSE, rebuild = TRUE)) 

# renv::install("utf8", repos = CRAN, prompt = FALSE, rebuild = TRUE)
# utils::install.packages("utf8", repos = CRAN)
# utils::install.packages("tibble", repos = CRAN)

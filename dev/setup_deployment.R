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
# Parse DESCRIPTION dependency fields (Depends, Imports, Suggests, ...)
# ---------------------------------------------------------------------------
parse_dep_field <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(x)) {
    return(character())
  }
  x <- gsub("\n", " ", x, fixed = TRUE)
  parts <- strsplit(x, ",")[[1L]]
  trimws(sub("\\s*\\([^)]*\\)", "", parts))
}

description_packages <- function(desc_path = "DESCRIPTION") {
  desc <- as.data.frame(read.dcf(desc_path), stringsAsFactors = FALSE)
  fields <- intersect(
    c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo"),
    names(desc)
  )
  pkgs <- unique(unlist(lapply(desc[1L, fields, drop = TRUE], parse_dep_field)))
  setdiff(pkgs, c("R", ""))
}

# ---------------------------------------------------------------------------
# Packages listed in an existing manifest.json (full transitive closure)
# ---------------------------------------------------------------------------
manifest_packages <- function(manifest_path = "manifest.json") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = CRAN)
  }
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  sort(names(manifest$packages))
}

is_internal_manifest_entry <- function(entry) {
  repo <- entry$Repository %||% ""
  remote <- entry$description$RemoteRepos %||% ""
  any(c(repo, remote) %in% INTERNAL_REPOS) ||
    grepl("ppm\\.titans\\.int\\.bayer\\.com", repo) ||
    grepl("ppm\\.titans\\.int\\.bayer\\.com", remote)
}

manifest_packages_to_reinstall <- function(
    manifest_path = "manifest.json",
    reinstall_all = identical(tolower(Sys.getenv("REINSTALL_ALL_MANIFEST", "false")), "true")
) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = CRAN)
  }
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  flags <- vapply(manifest$packages, is_internal_manifest_entry, logical(1L))
  if (reinstall_all) {
    return(sort(names(manifest$packages)))
  }
  sort(names(manifest$packages)[flags])
}


install_from_ppm <- function(packages, repos = CRAN) {
  packages <- setdiff(unique(packages), "")
  if (!length(packages)) {
    return(invisible(NULL))
  }
  message("Installing ", length(packages), " package(s) from P3M …")
  if (requireNamespace("renv", quietly = TRUE)) {
    renv::install(packages, repos = repos, prompt = FALSE, rebuild = TRUE)
  } else {
    utils::install.packages(packages, repos = repos)
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# 1) Point R at P3M only (avoid picking up internal repo from .Rprofile)
# ---------------------------------------------------------------------------
options(repos = c(CRAN = CRAN))
Sys.setenv(RSPM = CRAN)

# ---------------------------------------------------------------------------
# 2) Reinstall packages
# ---------------------------------------------------------------------------
# Direct dependencies from DESCRIPTION (good for a minimal library):
desc_pkgs <- description_packages("DESCRIPTION")
message("DESCRIPTION packages: ", paste(desc_pkgs, collapse = ", "))
install_from_ppm(desc_pkgs)

# Packages from manifest.json (transitive deps). By default only entries that
# still point at internal Bayer PPM; set REINSTALL_ALL_MANIFEST=true to
# reinstall every package in the manifest (safest before writeManifest).
if (file.exists("manifest.json")) {
  manifest_pkgs <- manifest_packages_to_reinstall("manifest.json")
  message(
    "Reinstalling ", length(manifest_pkgs),
    " manifest package(s) from P3M …"
  )
  install_from_ppm(manifest_pkgs)
}

# Local package (Connect bundles the app, but manifest generation needs deps resolved):
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::install(quick = FALSE, upgrade = "never", quiet = TRUE)
} else if (requireNamespace("pak", quietly = TRUE)) {
  pak::local_install(upgrade = FALSE)
} else {
  message(
    "Install devtools or pak to install elaborator from source, ",
    "or rely on pkgload::load_all() only for local testing."
  )
}

# ---------------------------------------------------------------------------
# 3) Regenerate manifest.json (must run AFTER P3M installs)
# ---------------------------------------------------------------------------
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = CRAN)
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(normalizePath("."))

rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")

message("Done. Review manifest.json Repository / RemoteRepos fields for P3M URLs.")


# Install all packages from Posit Public Package manager
Sys.setenv("RENV_CONFIG_PAK_ENABLED" = TRUE)
Sys.setenv("RENV_CONFIG_PPM_ENABLED" = TRUE)
Sys.setenv("RENV_CONFIG_AUTOLOADER_ENABLED" = FALSE)

CRAN <- "https://packagemanager.posit.co/cran/__linux__/jammy/latest"

pkgs <- renv::lockfile_read()$Packages
df <- tibble::tibble(
  Package    = names(pkgs),
  Repository = purrr::map_chr(pkgs, "Repository", .default = NA_character_)
)

df |> 
  # dplyr::filter(!Repository %in% c("CRAN", "RSPM")) |> 
  dplyr::filter(Repository %in% c("https://ppm-dev.titans.int.bayer.com/cran/__linux__/jammy/latest")) |> 
  dplyr::pull("Package") |> 
  purrr::walk(\(x) renv::install(x, repos = CRAN, prompt = FALSE, rebuild = TRUE)) 

renv::install("utf8", repos = CRAN, prompt = FALSE, rebuild = TRUE)
utils::install.packages("utf8", repos = CRAN)
utils::install.packages("tibble", repos = CRAN)

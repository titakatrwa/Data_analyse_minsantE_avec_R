packages <- c(
  "tidyverse",
  "lubridate",
  "janitor",
  "shiny",
  "bslib",
  "plotly",
  "DT",
  "scales"
)

missing_packages <- packages[!vapply(
  packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

message("Toutes les dépendances sont disponibles.")


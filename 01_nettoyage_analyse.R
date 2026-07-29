suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(scales)
})

dir.create("outputs", showWarnings = FALSE)

fix_mojibake <- function(x) {
  if (!is.character(x)) return(x)
  needs_fix <- !is.na(x) & str_detect(x, "Ã|Â")
  corrected <- x
  corrected[needs_fix] <- iconv(
    iconv(x[needs_fix], from = "UTF-8", to = "latin1"),
    from = "latin1",
    to = "UTF-8"
  )
  corrected
}

mode_value <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) == 0) return(NA_character_)
  names(sort(table(values), decreasing = TRUE))[1]
}

raw <- readr::read_csv(
  "dirty_health_data.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(across(where(is.character), fix_mojibake))

n_raw <- nrow(raw)
n_duplicates <- sum(duplicated(raw))

deduplicated <- raw |> distinct()

standardized <- deduplicated |>
  mutate(
    region = recode(
      region,
      "LITTORAL" = "Littoral",
      "litoral" = "Littoral",
      "SW" = "Sud-Ouest",
      "sud ouest" = "Sud-Ouest",
      "centre" = "Centre",
      "Ctr" = "Centre",
      "CENTER" = "Centre"
    ),
    gender = recode(
      gender,
      "Male" = "Masculin",
      "male" = "Masculin",
      "M" = "Masculin",
      "Masculin" = "Masculin",
      "Female" = "Féminin",
      "female" = "Féminin",
      "F" = "Féminin",
      "Feminin" = "Féminin"
    )
  )

district_region <- standardized |>
  filter(!is.na(region)) |>
  group_by(district) |>
  summarise(region_from_district = mode_value(region), .groups = "drop")

cost_nonnegative <- standardized$treatment_cost[
  !is.na(standardized$treatment_cost) & standardized$treatment_cost >= 0
]
q1 <- quantile(cost_nonnegative, 0.25, na.rm = TRUE)
q3 <- quantile(cost_nonnegative, 0.75, na.rm = TRUE)
cost_upper_bound <- as.numeric(q3 + 1.5 * IQR(cost_nonnegative, na.rm = TRUE))

clean <- standardized |>
  left_join(district_region, by = "district") |>
  mutate(
    consultation_date_raw = consultation_date,
    patient_age_raw = patient_age,
    treatment_cost_raw = treatment_cost,
    consultation_date = suppressWarnings(ymd(consultation_date)),
    patient_age = if_else(
      between(patient_age, 0, 120),
      as.numeric(patient_age),
      NA_real_
    ),
    treatment_cost = if_else(
      treatment_cost >= 0 & treatment_cost <= cost_upper_bound,
      treatment_cost,
      NA_real_,
      missing = NA_real_
    ),
    region = coalesce(region, region_from_district, "Non renseigné"),
    gender = coalesce(gender, "Non renseigné"),
    diagnosis = coalesce(diagnosis, "Non renseigné"),
    insurance_status = coalesce(insurance_status, "Non renseigné"),
    age_group = case_when(
      is.na(patient_age) ~ "Non renseigné",
      patient_age < 5 ~ "0–4 ans",
      patient_age < 15 ~ "5–14 ans",
      patient_age < 25 ~ "15–24 ans",
      patient_age < 45 ~ "25–44 ans",
      patient_age < 65 ~ "45–64 ans",
      TRUE ~ "65 ans et plus"
    ),
    consultation_month = floor_date(consultation_date, "month"),
    medication_stockout = medication_available == "Stockout",
    is_emergency = consultation_type == "Emergency"
  ) |>
  select(-region_from_district) |>
  distinct() |>
  arrange(consultation_date, region, district)

audit <- tibble(
  controle = c(
    "Lignes brutes",
    "Doublons supprimés après standardisation",
    "Dates invalides",
    "Régions manquantes après reconstitution",
    "Genres non renseignés",
    "Diagnostics non renseignés",
    "Statuts d'assurance non renseignés",
    "Âges hors limites",
    "Coûts manquants, négatifs ou aberrants"
  ),
  nombre = c(
    n_raw,
    n_raw - nrow(clean),
    sum(is.na(clean$consultation_date)),
    sum(clean$region == "Non renseigné"),
    sum(clean$gender == "Non renseigné"),
    sum(clean$diagnosis == "Non renseigné"),
    sum(clean$insurance_status == "Non renseigné"),
    sum(is.na(clean$patient_age)),
    sum(is.na(clean$treatment_cost))
  ),
  traitement = c(
    "Référence",
    "Suppression",
    "Conversion en NA",
    "Valeur Non renseigné",
    "Valeur Non renseigné",
    "Valeur Non renseigné",
    "Valeur Non renseigné",
    "Conversion en NA",
    paste0("Conversion en NA au-delà de ", round(cost_upper_bound, 2))
  )
)

known_insurance <- clean |> filter(insurance_status %in% c("Insured", "Uninsured"))

kpis <- tibble(
  indicateur = c(
    "Consultations après dédoublonnage",
    "Consultations avec date valide",
    "Taux de rupture de médicaments",
    "Part non assurée parmi les statuts connus",
    "Part des urgences",
    "Coût médian valide",
    "Âge médian valide"
  ),
  valeur = c(
    nrow(clean),
    sum(!is.na(clean$consultation_date)),
    mean(clean$medication_stockout),
    mean(known_insurance$insurance_status == "Uninsured"),
    mean(clean$is_emergency),
    median(clean$treatment_cost, na.rm = TRUE),
    median(clean$patient_age, na.rm = TRUE)
  ),
  unite = c("nombre", "nombre", "proportion", "proportion", "proportion", "coût", "ans")
)

monthly <- clean |>
  filter(!is.na(consultation_month)) |>
  count(consultation_month, name = "consultations")

diagnostics <- clean |>
  count(diagnosis, sort = TRUE, name = "consultations") |>
  mutate(part = consultations / sum(consultations))

regions <- clean |>
  group_by(region) |>
  summarise(
    consultations = n(),
    taux_rupture = mean(medication_stockout),
    taux_urgence = mean(is_emergency),
    cout_median = median(treatment_cost, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(consultations))

write_csv(clean, "outputs/clean_health_data.csv", na = "")
write_csv(audit, "outputs/audit_qualite.csv", na = "")
write_csv(kpis, "outputs/kpi_globaux.csv", na = "")
write_csv(monthly, "outputs/consultations_mensuelles.csv", na = "")
write_csv(diagnostics, "outputs/diagnostics.csv", na = "")
write_csv(regions, "outputs/regions.csv", na = "")

theme_health <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", colour = "#143B5D"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}

p_monthly <- ggplot(monthly, aes(consultation_month, consultations)) +
  geom_line(colour = "#007C91", linewidth = 1.1) +
  geom_point(colour = "#007C91", size = 2.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title = "Évolution mensuelle des consultations",
    x = NULL,
    y = "Consultations"
  ) +
  theme_health()

p_diagnosis <- diagnostics |>
  filter(diagnosis != "Non renseigné") |>
  slice_max(consultations, n = 8) |>
  mutate(diagnosis = fct_reorder(diagnosis, consultations)) |>
  ggplot(aes(consultations, diagnosis)) +
  geom_col(fill = "#E16B4B", width = 0.7) +
  labs(
    title = "Diagnostics les plus fréquents",
    x = "Consultations",
    y = NULL
  ) +
  theme_health()

p_stockout <- regions |>
  mutate(region = fct_reorder(region, taux_rupture)) |>
  ggplot(aes(taux_rupture, region)) +
  geom_col(fill = "#D7A431", width = 0.7) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Taux de rupture de médicaments par région",
    x = "Taux de rupture",
    y = NULL
  ) +
  theme_health()

ggsave("outputs/figure_consultations_mensuelles.png", p_monthly, 9, 5, dpi = 160)
ggsave("outputs/figure_diagnostics.png", p_diagnosis, 9, 5, dpi = 160)
ggsave("outputs/figure_ruptures_region.png", p_stockout, 9, 5.5, dpi = 160)

message("Analyse terminée. Résultats disponibles dans outputs/.")

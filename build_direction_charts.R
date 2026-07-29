suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

dir.create("livrables_direction/_assets", recursive = TRUE, showWarnings = FALSE)
monthly <- read_csv("outputs/consultations_mensuelles.csv", show_col_types = FALSE)
regions <- read_csv("outputs/regions.csv", show_col_types = FALSE)
diagnostics <- read_csv("outputs/diagnostics.csv", show_col_types = FALSE)

theme_direction <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", colour = "#143B5D"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(colour = "#64748B"),
      plot.margin = margin(8, 12, 8, 8)
    )
}

p_monthly <- monthly |>
  ggplot(aes(consultation_month, consultations)) +
  geom_area(fill = "#007C91", alpha = .12) +
  geom_line(colour = "#007C91", linewidth = 1.1) +
  geom_point(colour = "#007C91", size = 2.4) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(limits = c(740, 910), breaks = seq(750, 900, 50)) +
  labs(x = NULL, y = "Consultations") +
  theme_direction()

p_regions <- regions |>
  mutate(region = fct_reorder(region, taux_rupture),
         couleur = if_else(taux_rupture >= .195, "Priorité", "Autres")) |>
  ggplot(aes(taux_rupture, region, fill = couleur)) +
  geom_col(width = .66) +
  geom_vline(xintercept = weighted.mean(regions$taux_rupture, regions$consultations),
             linetype = "dashed", colour = "#143B5D") +
  geom_text(aes(label = percent(taux_rupture, accuracy = .1)), hjust = -.1, size = 3.3) +
  scale_fill_manual(values = c("Autres" = "#D7A431", "Priorité" = "#E16B4B")) +
  scale_x_continuous(labels = percent, limits = c(0, .225)) +
  labs(x = "Part des consultations en rupture", y = NULL) +
  theme_direction() +
  theme(legend.position = "none")

p_diag <- diagnostics |>
  filter(!str_detect(str_to_lower(diagnosis), "renseign")) |>
  slice_head(n = 8) |>
  mutate(diagnosis = fct_reorder(diagnosis, consultations)) |>
  ggplot(aes(consultations, diagnosis)) +
  geom_col(fill = "#E16B4B", width = .66) +
  geom_text(aes(label = number(consultations, big.mark = " ")), hjust = -.15, size = 3.3) +
  scale_x_continuous(limits = c(0, 1220)) +
  labs(x = "Nombre de consultations", y = NULL) +
  theme_direction()

ggsave(filename = "livrables_direction/_assets/consultations_mensuelles.png",
       plot = p_monthly, width = 8.5, height = 3.6, dpi = 180)
ggsave(filename = "livrables_direction/_assets/ruptures_region.png",
       plot = p_regions, width = 8.2, height = 4.5, dpi = 180)
ggsave(filename = "livrables_direction/_assets/diagnostics.png",
       plot = p_diag, width = 8.2, height = 4.2, dpi = 180)

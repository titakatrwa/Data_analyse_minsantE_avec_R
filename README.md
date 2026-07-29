# Projet R — Analyse de donnée MinSanté 

Ce projet transforme `dirty_health_data.csv` en données propres, indicateurs métier,
graphiques et dashboard Shiny.

## Exécution

1. Ouvrir ce dossier dans RStudio.
2. Exécuter `install_packages.R` une seule fois.
3. Exécuter `source("01_nettoyage_analyse.R")`.
4. Lancer le dashboard avec `shiny::runApp()`.

Les résultats sont écrits dans `outputs/` :

- `clean_health_data.csv` : base nettoyée ;
- `audit_qualite.csv` : anomalies détectées et traitements ;
- `kpi_globaux.csv` : indicateurs principaux ;
- `consultations_mensuelles.csv`, `diagnostics.csv`, `regions.csv` : agrégats ;
- `figure_*.png` : graphiques exportés.

## Règles de nettoyage

- suppression des doublons exacts, puis des doublons révélés par la
  standardisation des catégories ;
- correction de l'encodage mojibake (`SantÃ©` → `Santé`) ;
- harmonisation des régions et du genre ;
- régions manquantes reconstituées à partir du district quand c'est possible ;
- dates impossibles converties en valeurs manquantes ;
- âges hors de `[0, 120]` convertis en valeurs manquantes ;
- coûts négatifs et coûts au-dessus de la borne de Tukey
  `Q3 + 1,5 × IQR` convertis en valeurs manquantes ;
- catégories manquantes conservées sous `Non renseigné` afin de ne pas inventer
  d'information.

La colonne brute et la colonne nettoyée sont toutes deux conservées pour l'âge,
le coût et la date. Cela garantit la traçabilité.

## Résultats principaux

Après dédoublonnage, la base contient **10 015 consultations**. Parmi elles,
**9 815 ont une date exploitable**.

- Rupture de médicaments : **18,0 %** des consultations.
- Patients non assurés parmi les statuts connus : **64,9 %**.
- Consultations d'urgence : **19,5 %**.
- Coût médian valide : **16,35** unités monétaires.
- Diagnostic le plus fréquent hors valeurs manquantes : **Anémie**.
- Région ayant le taux de rupture le plus élevé : **Est** (environ **20,1 %**).

## Recommandations

1. Prioriser la disponibilité des médicaments dans l'Est, le Sud et le Centre,
   où les taux de rupture observés sont les plus élevés.
2. Cibler l'accès financier : près de deux patients sur trois ayant un statut
   connu ne sont pas assurés.
3. Renforcer les contrôles à la saisie : listes fermées pour les catégories,
   validation des dates, âge limité à 0–120 et coût obligatoirement positif.
4. Suivre séparément l'anémie, la malnutrition et les infections respiratoires,
   qui dominent le volume des diagnostics renseignés.

## Limites

Les données sont simulées. Les lignes dont la date est invalide restent dans la
base propre, mais sont exclues des analyses temporelles. Les coûts aberrants ne
sont pas remplacés : ils restent disponibles dans `treatment_cost_raw` et sont
mis à `NA` dans `treatment_cost`.

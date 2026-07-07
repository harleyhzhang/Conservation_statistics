# Shared libraries, lookups and helpers for the plot and stand summaries.
# Run every script from the repository root, e.g. `Rscript plot_stats/2023/2023_summary_code.R`.
library(dplyr)
library(tidyr)
library(stringr)

species_names <- c(
  ABCO = "White fir",
  ABLA = "Subalpine fir",
  ACGL = "Rocky Mountain maple",
  JUSC = "Rocky Mountain juniper",
  PIED = "Colorado pinyon",
  PIEN = "Engelmann spruce",
  PIFL = "Limber pine",
  PIPO = "Ponderosa pine",
  POTR = "Aspen",
  PSME = "Douglas fir"
)

damage_names <- c(
  barkbeetle = "bark beetle",
  douglasfiradelgid = "douglas fir adelgid",
  mechanicaldamage = "mechanical damage",
  sprucebudworm = "spruce budworm",
  winddamage = "wind damage"
)

# Common name for one species code; NULL for an unknown code.
common_name <- function(code) if (code %in% names(species_names)) species_names[[code]] else NULL

# Common names for a vector of codes, keeping unknown codes as they are.
common_names <- function(codes) unname(ifelse(codes %in% names(species_names), species_names[codes], as.character(codes)))

# Readable damage types; NA for anything unrecognised.
damage_label <- function(damage) unname(damage_names[damage])

# Dominant tree species in one plot, plus the runner-up when the leader is 50% or less.
dominant_species <- function(species) {
  species_counts <- table(species)
  max_count <- max(species_counts)
  most_common_species <- names(species_counts)[species_counts == max_count]
  percent_frequency <- max_count / length(species) * 100
  species_name <- common_name(most_common_species[1])
  if (percent_frequency <= 50) {
    second_most_common_species <- names(sort(species_counts, decreasing = TRUE))[2]
    second_percent_frequency <- species_counts[second_most_common_species] / length(species) * 100
    paste(species_name, " (", format(round(percent_frequency, 2), nsmall = 2), "%), ",
          common_name(second_most_common_species), " (", format(round(second_percent_frequency, 2), nsmall = 2), "%)", sep = "")
  } else {
    paste(species_name, " (", format(round(percent_frequency, 2), nsmall = 2), "%)", sep = "")
  }
}

# Per stand: how many plots have `column == value`, as "N plots, P% of plots".
plot_share <- function(data, column, value) {
  data %>%
    group_by(stand) %>%
    summarise(
      total_rows = n(),
      matching_rows = sum(.data[[column]] == value),
      percentage = round((matching_rows / total_rows) * 100, 2)
    ) %>%
    mutate(!!column := paste0(matching_rows, " plots, ", percentage, "% of plots")) %>%
    select(stand, all_of(column))
}

stand_species_pattern <- "(Aspen|Douglas fir|Colorado Pinyon|Engelmann spruce|Limber pine|Ponderosa pine|Rocky Mountain juniper|Rocky Mountain maple|Subalpine fir|White fir|No live adult trees present)"

# Per stand: the species most often dominant across its plots. `none_label` ranks below real species.
dominant_by_stand <- function(data, column, output, none_label) {
  data %>%
    mutate(!!column := str_extract_all(.data[[column]], stand_species_pattern)) %>%
    group_by(stand) %>%
    mutate(total = sum(n())) %>%
    ungroup() %>%
    unnest(all_of(column)) %>%
    group_by(across(all_of(c("stand", column)))) %>%
    summarise(occurrences = n(),
              total = unique(total)) %>%
    group_by(stand) %>%
    mutate(
      percent_frequency = occurrences / total,
      !!output := paste0(.data[[column]], ", ", occurrences, " plots, ", round(percent_frequency*100, 2), "% of plots"),
      rank = ifelse(.data[[column]] == none_label, occurrences - max(occurrences), occurrences)
    ) %>%
    group_by(stand) %>%
    slice(which.max(rank)) %>%
    select(stand, all_of(output))
}

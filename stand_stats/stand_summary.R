source("common.R")

# Step 1: Read in raw/input CSVs
summary_2022 <- read.csv("plot_stats/2022/2022_summary.csv")
summary_2023 <- read.csv("plot_stats/2023/2023_summary.csv")
stand_plots <- read.csv("stand_stats/stand_plots.csv")

# Step 2: Convert data frames all to <character>
prepare_summary <- function(summary) {
  summary %>%
    mutate(treatment_year = as.character(treatment_year),
           basal_area_per_acre_in = as.numeric(basal_area_per_acre_in),
           average_dbh_in = as.numeric(average_dbh_in),
           average_height_ft = as.numeric(average_height_ft)) %>%
    rename(dominant_tree = dominant_tree_species,
           dominant_regeneration = dominant_regeneration_species)
}

# Step 3: Merge data frames
summary_merged <- bind_rows(prepare_summary(summary_2022), prepare_summary(summary_2023))

# Step 4: Assign stands to each plot in the merged data frame
summary_merged <- right_join(stand_plots, summary_merged, by = "plot")

# Step 5: When there is a second species in `dominant_tree_species`, get rid of the second one unless same %
remove_second_dom <- function(species) {
  species_list <- strsplit(species, ",")[[1]]
  if (length(species_list) == 1) {
    return(species)
  }
  if (length(species_list) == 2) {
    percent1 <- as.numeric(gsub("[^0-9.]", "", species_list[1]))
    percent2 <- as.numeric(gsub("[^0-9.]", "", species_list[2]))
    if (percent1 == percent2) {
      return(species)
    } else {
      if (percent1 > percent2) {
        return(species_list[1])
      } else {
        return(species_list[2])
      }
    }
  }
}

summary_merged <- summary_merged %>%
  mutate(
    dominant_tree = sapply(dominant_tree, remove_second_dom),
    dominant_regeneration = sapply(dominant_regeneration, remove_second_dom)
  )

stand_data <- summary_merged %>%
  filter(!is.na(stand))


#### TREE STATISTICS ####

# Step 5: Average basal area per acre (in)
average_basal_area_per_acre_in <- stand_data %>%
  group_by(stand) %>%
  summarise(average_basal_area_per_acre_in = round(mean(basal_area_per_acre_in), 2))

# Step 6: Average DBH (in)
average_dbh_in <- stand_data %>%
  group_by(stand) %>%
  summarise(average_dbh_in = round(mean(average_dbh_in), 2))

# Step 7: Average height (ft)
average_height_ft <- stand_data %>%
  group_by(stand) %>%
  summarise(average_height_ft = round(mean(average_height_ft), 2))

# Step 8: Dominant tree species
dominant_tree_species <- dominant_by_stand(stand_data, "dominant_tree", "dominant_tree_species", "No live adult trees present")


#### REGENERATION STATISTICS ####

# Step 8: Regeneration presence (Y/N)
regeneration_presence <- plot_share(stand_data, "regeneration_presence", "Regeneration present")

# Step 9: Average seedlings per acre
average_seedlings_per_acre <- stand_data %>%
  group_by(stand) %>%
  summarise(average_seedlings_per_acre = round(mean(seedlings_per_acre), 2))

# Step 10: Dominant regeneration species
dominant_regeneration_species <- dominant_by_stand(stand_data, "dominant_regeneration", "dominant_regeneration_species", "None")


#### DAMAGE STATISTICS ####

# Step 11: Insect presence (Y/N)
insect_damage_presence <- plot_share(stand_data, "insect_damage_presence", "Insect damage present")

# Step 12: Browse presence (Y/N)
browse_damage_presence <- plot_share(stand_data, "browse_damage_presence", "Browse present")

# Step 13: List of damage types
list_damage <- stand_data %>%
  mutate(
    list_damage = tolower(list_damage),
    list_damage = gsub("\\s+", "", list_damage),
  ) %>%
  separate_rows(list_damage, sep = ",") %>%
  mutate(
    list_damage = damage_label(list_damage)
  ) %>%
  distinct(stand, list_damage) %>%
  group_by(stand) %>%
  summarise(
    list_damage = if_else(all(list_damage %in% NA), "None", paste(sort(na.omit(list_damage)), collapse = ", "))
  ) %>%
  mutate(
    list_damage = str_to_sentence(list_damage, locale="en"),
    list_damage = gsub("douglas", "Douglas", list_damage)
  )


#### MERGE TO CREATE SUMMARY ####

# Step 14: Merge all outputs into one dataframe
stand_summary <- Reduce(function(x, y) merge(x, y, by = "stand", all = TRUE), 
                        list(average_basal_area_per_acre_in, average_dbh_in, average_height_ft, dominant_tree_species,
                             regeneration_presence, average_seedlings_per_acre, dominant_regeneration_species,
                             insect_damage_presence, browse_damage_presence, list_damage))

# Step 15: If list_damage="None", but browse/insect are present, indicate that in list_damage
stand_summary <- stand_summary %>%
  mutate(
    list_damage = case_when(
      list_damage == "None" & browse_damage_presence != "0 plots, 0% of plots" & insect_damage_presence == "0 plots, 0% of plots" ~ "Browse",
      list_damage == "None" & browse_damage_presence == "0 plots, 0% of plots" & insect_damage_presence != "0 plots, 0% of plots" ~ "Insect",
      list_damage == "None" & browse_damage_presence != "0 plots, 0% of plots" & insect_damage_presence != "0 plots, 0% of plots" ~ "Browse, insect",
      TRUE ~ list_damage
    )
  )

# Step 16: Write output to CSV
write.csv(stand_summary, file = "stand_stats/stand_summary.csv", row.names = FALSE)

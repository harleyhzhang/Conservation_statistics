source("common.R")

# Step 1: Read in raw/input CSV
input_data <- read.csv("plot_stats/2023/cleanedMerged_Forestry23_030524_2.csv")

# Rename "new_plot_key" to "plot" (to match 2022)
input_data <- input_data %>%
  rename(plot = new_plot_key)


#### TREATMENT STATISTICS ####

# Step 2: Treatment year
treatment_year <- input_data %>%
  group_by(plot) %>%
  summarise(treatment_year = ifelse(is.na(unique(treatment_year)) | unique(treatment_year) == 0, "Unknown", as.character(unique(treatment_year))))

# Step 3: Treatment type
treatment_type <- input_data %>%
  separate_rows(short_description_of_treament, sep = ",") %>%
  mutate(short_description_of_treament = trimws(short_description_of_treament),
         short_description_of_treament = case_when(
           short_description_of_treament == "THIN_FROM_BELOW" ~ "thin from below",
           TRUE ~ NA_character_
           )) %>%
  distinct(plot, short_description_of_treament) %>%
  group_by(plot) %>%
  summarise(treatment_type = if_else(all(short_description_of_treament %in% NA), "Unknown", paste(sort(na.omit(short_description_of_treament)), collapse = "; "))) %>%
  mutate(treatment_type = str_to_sentence(treatment_type, locale="en"))


#### TREE STATISTICS ####

# Step 4: Basal area per acre (in)
basal_area_per_acre_in <- input_data %>%
  filter(alive_or_dead == "living", !is.na(dbh_cm_tree)) %>%
  group_by(plot) %>%
  summarise(basal_area_per_acre_in = round(sum(0.005454 * (dbh_cm_tree/2.54)^2) * 5, 2))

# Step 5: Average DBH (in)
average_dbh_in <- input_data %>%
  filter(alive_or_dead == "living", !is.na(dbh_cm_tree)) %>%
  group_by(plot) %>%
  summarise(average_dbh_in = round(mean(dbh_cm_tree) / 2.54, 2))

# Step 6: Average height (ft)
average_height_ft <- input_data %>%
  filter(alive_or_dead == "living", !is.na(total_height_m_tree)) %>%
  group_by(plot) %>%
  summarise(average_height_ft = round(mean(total_height_m_tree) * 3.28084, 2))

# Step 7: Dominant tree species
dominant_tree_species <- input_data %>%
  filter(alive_or_dead == "living", size_class == "tree", !is.na(tree_species)) %>%
  group_by(plot) %>%
  summarise(dominant_tree_species = dominant_species(tree_species))


#### REGENERATION STATISTICS ####

# Step 8: Regeneration presence (Y/N)
regeneration_presence <- input_data %>%
  group_by(plot) %>%
  summarise(regeneration_presence = ifelse(any((size_class == "sapling" & alive_or_dead == "living") | (size_class == "seedling" & number_of_seedlings > 0)), "Regeneration present", "Regeneration absent"))

# Step 9: Seedlings per acre
seedlings_per_acre <- input_data %>%
  filter(size_class == "seedling", !is.na(number_of_seedlings)) %>%
  group_by(plot) %>%
  summarise(seedlings_per_acre = sum(number_of_seedlings, na.rm = TRUE) * 50) %>%
  right_join(distinct(select(input_data, plot)), by = "plot") %>%
  mutate(seedlings_per_acre = if_else(is.na(seedlings_per_acre), 0, seedlings_per_acre))

# Step 10: Dominant regeneration species
dominant_regeneration_species <- input_data %>%
  filter((size_class == "sapling" & alive_or_dead == "living") | (size_class == "seedling" & number_of_seedlings > 0)) %>%
  group_by(plot, tree_species) %>%
  summarise(regeneration_count = sum(size_class == "sapling") + sum(ifelse(size_class == "seedling", number_of_seedlings, 0))) %>%
  group_by(plot) %>%
  mutate(percent_frequency = regeneration_count / sum(regeneration_count) * 100,
         tree_species = common_names(tree_species)) %>%
  arrange(desc(percent_frequency)) %>%
  summarise(dominant_regeneration_species = {
    if (percent_frequency[1] <= 50) {
      paste(tree_species[1], " (", round(percent_frequency[1], 2), "%), ", 
            tree_species[2], " (", round(percent_frequency[2], 2), "%)", sep = "")
    } else {
      paste(tree_species[1], " (", round(percent_frequency[1], 2), "%)", sep = "")
    }
  })

dominant_regeneration_species <- input_data %>%
  distinct(plot) %>%
  left_join(dominant_regeneration_species, by = "plot") %>%
  mutate(dominant_regeneration_species = ifelse(is.na(dominant_regeneration_species), "None", dominant_regeneration_species))


#### DAMAGE STATISTICS ####

# Step 11: Insect presence (Y/N)
insect_damage_presence <- input_data %>%
  filter(!is.na(insect_presence)) %>%
  group_by(plot) %>%
  summarise(insect_damage_presence = ifelse(any(insect_presence == 1), "Insect damage present", "Insect damage absent"))

# Step 12: Browse presence (Y/N)
browse_damage_presence <- input_data %>%
  filter(!is.na(browse_presence)) %>%
  group_by(plot) %>%
  summarise(browse_damage_presence = ifelse(any(browse_presence == 1), "Browse present", "Browse absent"))

# Step 13: List of damage types
list_damage <- input_data %>%
  mutate(
    what_if_any_disease_damage_present = tolower(what_if_any_disease_damage_present),
    what_if_any_disease_damage_present = gsub("mechanicaldamamge", "mechanicaldamage", what_if_any_disease_damage_present),
    what_if_any_disease_damage_present = gsub("woodpeckers", "woodpecker", what_if_any_disease_damage_present)
  ) %>%
  separate_rows(what_if_any_disease_damage_present, sep = ",") %>%
  mutate(
    what_if_any_disease_damage_present = trimws(what_if_any_disease_damage_present),
    what_if_any_disease_damage_present = damage_label(what_if_any_disease_damage_present)
  ) %>%
  distinct(plot, what_if_any_disease_damage_present) %>%
  group_by(plot) %>%
  summarise(
    list_damage = if_else(all(what_if_any_disease_damage_present %in% NA), "None", paste(sort(na.omit(what_if_any_disease_damage_present)), collapse = ", "))
  ) %>%
  mutate(
    list_damage = str_to_sentence(list_damage, locale="en"),
    list_damage = gsub("douglas", "Douglas", list_damage)
  )


#### MERGE TO CREATE SUMMARY ####

# Step 14: Merge all outputs into one dataframe
summary_2023 <- Reduce(function(x, y) merge(x, y, by = "plot", all = TRUE), 
                            list(treatment_year, treatment_type, basal_area_per_acre_in, average_dbh_in, average_height_ft, dominant_tree_species, 
                                 regeneration_presence, seedlings_per_acre, dominant_regeneration_species,
                                 insect_damage_presence, browse_damage_presence, list_damage))

# Step 15: Write output to CSV
write.csv(summary_2023, file = "plot_stats/2023/2023_summary.csv", row.names = FALSE)

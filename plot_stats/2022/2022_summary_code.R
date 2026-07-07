source("common.R")

# Step 1: Read in raw/input CSV
input_data <- read.csv("plot_stats/2022/2024Updated_2022_ForestMont_MergedData.csv")

input_data <- input_data %>%
  mutate(plot = paste0(plot, "_2022"))


#### TREATMENT STATISTICS ####

# Step 2: Treatment year
treatment_year <- input_data %>%
  group_by(plot) %>%
  summarise(treatment_year = ifelse(is.na(unique(treatment_year)) | unique(treatment_year) == 0, "Unknown", as.character(unique(treatment_year))))

# Step 3: Treatment type
## *NOTE*: before this step, go into the input_data CSV and remove any special character in front of "pipu" and change "remove all abco" to "remove abco"
treatment_type <- input_data %>%
  mutate(
    treatment_type_long = tolower(treatment_type_long),
    treatment_type_long = gsub("\\s+", "", treatment_type_long),
    treatment_type_long = gsub("heavybudwormdamagewithtoppedpsmeandabco", "heavybudwormdamagewithdeadtoppeddfandwf", treatment_type_long),
    treatment_type_long = gsub("incleanupportionremovewhitefirandcbsandmistletoepp", "inthecleanupportionremoveabco,pipu,andpipowithmistletoe", treatment_type_long),
    treatment_type_long = gsub("mastecation", "mastication", treatment_type_long),
    treatment_type_long = gsub("removeallabco&mistletoe", "removeallabcoandmistletoe", treatment_type_long),
    treatment_type_long = gsub("removeallabcoandmistletoetrees", "removeallabcoandmistletoe", treatment_type_long),
    treatment_type_long = gsub("removeallwhitefirandmistletoe", "removeallabcoandmistletoe", treatment_type_long),
    treatment_type_long = gsub("removealmostallconiferfromtheriparianareaandretaincw/as", "removealmostallconiferfromriparianareasandretainpodeandpotr", treatment_type_long),
    treatment_type_long = gsub("retainhealthlyppanddf", "retainhealthyponderosapineanddouglasfir", treatment_type_long),
    treatment_type_long = gsub("retainhealthypipoandpsme", "retainhealthyponderosapineanddouglasfir", treatment_type_long),
    treatment_type_long = gsub("shelterwoood", "shelterwood", treatment_type_long),
    treatment_type_long = gsub("w/", "with", treatment_type_long),
    treatment_type_long = gsub("[./]", ",", treatment_type_long),
    treatment_type_long = gsub("&", "and", treatment_type_long),
    treatment_type_long = gsub(":", "", treatment_type_long),
  ) %>%
  separate_rows(treatment_type_long, sep = ",") %>%
  mutate(
    treatment_type_long = trimws(treatment_type_long),
    treatment_type_long = case_when(
      treatment_type_long == "2010heavyblowndown" ~ "heavy blow down (2010)",
      treatment_type_long == "drainagewithdeadtoppedpsmeandabco" ~ "drainage with dead topped douglas fir and white fir",
      treatment_type_long == "heavybudwormdamagewithdeadtoppeddfandwf" ~ "heavy budworm damage with dead topped douglas fir and white fir",
      treatment_type_long == "inthecleanupportionremoveabco,pipu,andpipowithmistletoe" ~ "in the cleanup portion, remove white fir, pipu, and ponderosa pine with mistletoe",
      treatment_type_long == "removeabco" ~ "remove all white fir",
      treatment_type_long == "removeallabcoandmistletoe" ~ "remove all white fir and mistletoe",
      treatment_type_long == "removeallwhitefir,rmj,andlargepoorlyformeddf" ~ "remove all whitefir, rocky mountain juniper, and large poorly formed douglas fir",
      treatment_type_long == "removeallwhitefiranddougfir<14\"dbhandlargeifsufficientseedtreesinthearea" ~ "remove all white fir and douglas fir with <14\" dbh and large if sufficient seed trees in the area",
      treatment_type_long == "removealldefiguredabcowith14\"dbhandlargerifsufficentseedtreesinarea" ~ "remove all defigured white fir with ≥14\" dbh if sufficent seed trees in area",
      treatment_type_long == "removealmostallconiferfromriparianareasandretainpodeandpotr" ~ "remove almost all conifer from riparian areas and retain cottonwood and aspen",
      treatment_type_long == "removemistletoe" ~ "remove all mistletoe",
      treatment_type_long == "retainaspen,ponderosapine,andyoungdf" ~ "retain aspen, ponderosa pine, and young douglas fir",
      treatment_type_long == "retainhealthyponderosapineanddouglasfir" ~ "retain healthy ponderosa pine and douglas fir",
      treatment_type_long == "thinfrombelow" ~ "thin from below",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(plot, treatment_type_long) %>%
  group_by(plot) %>%
  summarise(
    treatment_type = if_else(all(treatment_type_long %in% NA), "Unknown", paste(sort(na.omit(treatment_type_long)), collapse = "; "))
  ) %>%
  mutate(
    treatment_type = str_to_sentence(treatment_type, locale="en"),
    treatment_type = gsub("douglas", "Douglas", treatment_type),
    treatment_type = gsub("rocky mountain", "Rocky Mountain", treatment_type),
    treatment_type = gsub("dbh", "DBH", treatment_type)
  )


#### TREE STATISTICS ####

# Step 4: Basal area per acre (in)
basal_area_per_acre_in <- input_data %>%
  filter(living_dead == "L", !is.na(dbh)) %>%
  group_by(plot) %>%
  summarise(basal_area_per_acre_in = round(sum(0.005454 * (dbh/2.54)^2) * 5, 2))

# Step 5: Average DBH (in)
average_dbh_in <- input_data %>%
  filter(living_dead == "L", !is.na(dbh)) %>%
  group_by(plot) %>%
  summarise(average_dbh_in = round(mean(dbh) / 2.54, 2))

# Step 6: Average height (ft)
average_height_ft <- input_data %>%
  filter(living_dead == "L", !is.na(total_height)) %>%
  group_by(plot) %>%
  summarise(average_height_ft = round(mean(total_height) * 3.28084, 2))

# Step 7: Dominant tree species
dominant_tree_species <- input_data %>%
  filter(living_dead == "L", size_class == "tree", !is.na(species)) %>%
  group_by(plot) %>%
  summarise(dominant_tree_species = dominant_species(species))


#### REGENERATION STATISTICS ####

# Step 8: Regeneration presence (Y/N)
regeneration_presence <- input_data %>%
  group_by(plot) %>%
  summarise(regeneration_presence = ifelse(any((size_class == "sapling" & living_dead == "L") | (size_class == "seedling" & count_seedling > 0)), "Regeneration present", "Regeneration absent"))

# Step 9: Seedlings per acre
seedlings_per_acre <- input_data %>%
  filter(size_class == "seedling", !is.na(count_seedling)) %>%
  group_by(plot) %>%
  summarise(seedlings_per_acre = sum(count_seedling, na.rm = TRUE) * 50) %>%
  right_join(distinct(select(input_data, plot)), by = "plot") %>%
  mutate(seedlings_per_acre = if_else(is.na(seedlings_per_acre), 0, seedlings_per_acre))

# Step 10: Dominant regeneration species
dominant_regeneration_species <- input_data %>%
  filter((size_class == "sapling" & living_dead == "L") | (size_class == "seedling" & count_seedling > 0)) %>%
  group_by(plot, species) %>%
  summarise(regeneration_count = sum(size_class == "sapling") + sum(ifelse(size_class == "seedling", count_seedling, 0))) %>%
  group_by(plot) %>%
  mutate(percent_frequency = regeneration_count / sum(regeneration_count) * 100,
         species = common_names(species)) %>%
  arrange(desc(percent_frequency)) %>%
  summarise(dominant_regeneration_species = {
    if (percent_frequency[1] <= 50) {
      paste(species[1], " (", round(percent_frequency[1], 2), "%), ", 
            species[2], " (", round(percent_frequency[2], 2), "%)", sep = "")
    } else {
      paste(species[1], " (", round(percent_frequency[1], 2), "%)", sep = "")
    }
  })

dominant_regeneration_species <- input_data %>%
  distinct(plot) %>%
  left_join(dominant_regeneration_species, by = "plot") %>%
  mutate(dominant_regeneration_species = ifelse(is.na(dominant_regeneration_species), "None", dominant_regeneration_species))


#### DAMAGE STATISTICS ####

# Step 11: Insect presence (Y/N)
insect_damage_presence <- input_data %>%
  filter(!is.na(insect_disease)) %>%
  group_by(plot) %>%
  summarise(insect_damage_presence = ifelse(any(insect_disease == 1), "Insect damage present", "Insect damage absent"))

# Step 12: Browse presence (Y/N)
browse_damage_presence <- input_data %>%
  filter(!is.na(browsing_damage)) %>%
  group_by(plot) %>%
  summarise(browse_damage_presence = ifelse(any(browsing_damage == 1), "Browse present", "Browse absent"))


#### MERGE TO CREATE SUMMARY ####

# Step 13: Merge all outputs into one dataframe
summary_2022 <- Reduce(function(x, y) merge(x, y, by = "plot", all = TRUE), 
                            list(treatment_year, treatment_type, basal_area_per_acre_in, average_dbh_in, average_height_ft, dominant_tree_species, 
                                 regeneration_presence, seedlings_per_acre, dominant_regeneration_species,
                                 insect_damage_presence, browse_damage_presence))

summary_2022 <- summary_2022 %>%
  mutate(basal_area_per_acre_in = ifelse(is.na(basal_area_per_acre_in), "0.00", basal_area_per_acre_in),
         average_dbh_in = ifelse(is.na(average_dbh_in), "0.00", average_dbh_in),
         average_height_ft = ifelse(is.na(average_height_ft), "0.00", average_height_ft),
         dominant_tree_species = ifelse(is.na(dominant_tree_species), "No live adult trees present", dominant_tree_species),
         regeneration_presence = ifelse(is.na(regeneration_presence), "Regeneration absent", regeneration_presence))

# Step 14: Write output to CSV
write.csv(summary_2022, file = "plot_stats/2022/2022_summary.csv", row.names = FALSE)

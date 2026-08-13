library(dplyr)
library(tidyr)
library(tibble)
library(vegan)


# ----------------------------------------------------------
# 2. Keep only the columns needed
#    - survey_id = sampling unit
#    - habitat   = grouping variable for plotting/testing
#    - species   = species name
#    - pres      = presence/absence (0/1)
# ----------------------------------------------------------
dat <- presence %>%
  select(survey_id, habitat, species, pres)

# ----------------------------------------------------------
# 3. Make sure presence/absence is numeric
# ----------------------------------------------------------
dat <- dat %>%
  mutate(pres = as.numeric(pres))

# ----------------------------------------------------------
# 4. Collapse duplicate survey_id × species rows
#    If a species appears more than once in a survey,
#    keep it as present if it was ever recorded (= max)
# ----------------------------------------------------------
dat2 <- dat %>%
  group_by(survey_id, habitat, species) %>%
  summarise(pres = max(pres, na.rm = TRUE), .groups = "drop")

# ----------------------------------------------------------
# 5. Optional check:
#    See whether any survey_id has more than one habitat
#    If this returns rows, survey_id may not be the right
#    sampling unit, or habitat values may need cleaning
# ----------------------------------------------------------
habitat_check <- dat2 %>%
  distinct(survey_id, habitat) %>%
  count(survey_id) %>%
  filter(n > 1)

print(habitat_check)

# ----------------------------------------------------------
# 6. Remove rare species
#    Keep only species present in at least 5 surveys
# ----------------------------------------------------------
dat3 <- dat2 %>%
  group_by(species) %>%
  filter(sum(pres == 1, na.rm = TRUE) >= 5) %>%
  ungroup()

# ----------------------------------------------------------
# 7. Optional check:
#    Count how many surveys each remaining species occurs in
# ----------------------------------------------------------
species_freq <- dat3 %>%
  group_by(species) %>%
  summarise(n_surveys = sum(pres == 1, na.rm = TRUE)) %>%
  arrange(n_surveys)

print(species_freq)

# ----------------------------------------------------------
# 8. Build the community matrix
#    Rows    = surveys
#    Columns = species
#    Values  = 0/1 presence-absence
# ----------------------------------------------------------
# comm_mat <- dat3 %>%
#   select(survey_id, species, pres) %>%
#   pivot_wider(
#     names_from = species,
#     values_from = pres,
#     values_fill = 0
#   ) %>%
#   column_to_rownames("survey_id") %>%
#   as.matrix()

comm_mat <- dat3 %>%
  select(survey_id, species, pres) %>%
  distinct() %>%
  pivot_wider(
    names_from = species,
    values_from = pres,
    values_fill = list(pres = 0)
  ) %>%
  column_to_rownames("survey_id") %>%
  as.matrix()


# ----------------------------------------------------------
# 9. Build habitat metadata table
#    One habitat value per survey
#    Reorder rows to match comm_mat row order
# ----------------------------------------------------------
hab <- dat3 %>%
  group_by(survey_id) %>%
  summarise(habitat = first(habitat), .groups = "drop") %>%
  slice(match(rownames(comm_mat), survey_id))

# ----------------------------------------------------------
# 10. Optional check:
#     Make sure row order in habitat table matches comm_mat
# ----------------------------------------------------------
all(hab$survey_id == rownames(comm_mat))

# ----------------------------------------------------------
# 11. Optional check:
#     Remove empty surveys if any exist
#     (surveys with no species left after filtering)
# ----------------------------------------------------------
comm_mat <- comm_mat[rowSums(comm_mat, na.rm = TRUE) > 0, , drop = FALSE]

hab <- hab %>%
  filter(survey_id %in% rownames(comm_mat)) %>%
  slice(match(rownames(comm_mat), survey_id))

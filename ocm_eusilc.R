###############################################################################
# Occupational Mobility Network for Europe
# INET Oxford Project
###############################################################################

# Source libraries for the project
source("libraries.R")

# Read in EU-SILC data - merged releases for 2012-2014
# and select variables -> export file and read in what I need:
# raw.data <- readstata13::read.dta13("data/EUSILC/eusilc_long2012_2014.dta")
# raw.data <- import("data/EUSILC/eusilc_omn_data.sav")
# lean.data <- raw.data %>%
#   select(YEAR, COUNTRY, HID, PID, PYNR, PYCOUNT,
#          PL050)
# export(lean.data, "data/EUSILC/lean_data.RData")
data <- import("data/EUSILC/lean_data.RData")

# create an unique identifier for each individual (U_ID)
data <- data %>%
  mutate(U_ID = paste0(COUNTRY, HID, PID)) %>%
  arrange(U_ID, YEAR)

# create a balanced panel
iddatvol <- expand.grid(U_ID = unique(data$U_ID), YEAR = unique(data$YEAR))
iddatvol <- iddatvol[order(iddatvol$U_ID, iddatvol$YEAR), ]
data_balanced <- merge(data, iddatvol, 
                       all.x = TRUE, all.y = TRUE, by = c("U_ID", "YEAR"))
rm(iddatvol)

# impute missing occupations from (t-1)
# data_balanced_imputed <- data_balanced %>%
#   group_by(U_ID) %>% 
#   zoo::na.locf() %>%
#   ungroup()

# transform into wide format
# data_imputed_wide <- data_balanced_imputed %>%
#   pivot_wider(id_cols = c("COUNTRY", "U_ID"), names_from = "YEAR",
#               names_prefix = "year", values_from = "PL050")

data_wide <- data_balanced %>%
  pivot_wider(id_cols = c("COUNTRY", "U_ID"), names_from = "YEAR",
              names_prefix = "year", values_from = "PL050")
data_wide <- data_wide[!is.na(data_wide$COUNTRY), ]

data_wide2 <- data_wide %>%
  mutate(year2009_o = !is.na(year2009),
         year2010_o = !is.na(year2010),
         year2011_o = !is.na(year2011),
         year2012_o = !is.na(year2012),
         year2013_o = !is.na(year2013),
         year2014_o = !is.na(year2014),
         num_obs = year2009_o + year2010_o + year2011_o + year2012_o +
           year2013_o + year2014_o) %>%
  filter(num_obs > 1)

data_wide3 <- data_wide2 %>%
  mutate(year2010 = ifelse(is.na(year2010), year2009, year2010))

t1 <- xtabs( ~ year2010 + year2009, data = data_wide3)
t2 <- xtabs( ~ year2011 + year2010, data = data_wide3)

t_tot <- t1 + t2
colSums(t1)


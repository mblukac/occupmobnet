###############################################################################
# Occupational Mobility Network for Europe
# INET Oxford Project
# EU-SILC data
# Martin Lukac @mblukac
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

### import classifications
classif <- rio::import("data/EUSILC/eusilc_5c_PH_Classification_withTSUE.sav")
data_classif <- data_balanced %>%
  select(U_ID, YEAR, PL050) %>%
  left_join(classif, by = c("U_ID", "YEAR"))

data_classif_final <- data_classif %>%
  mutate(segment = ifelse(`MClass#` == 3, "Unemployed",
                   ifelse(`MClass#` == 2, "Part-time",
                   ifelse(`MClass#` == 5, "Temporary",
                   ifelse(`MClass#` == 4, "Supervisors",
                   ifelse(`MClass#` == 1, "Standard", NA)))))) %>%
  mutate(segment = factor(segment, 
                          levels = c("Unemployed", "Part-time",
                                     "Temporary", "Standard",
                                     "Supervisors")))

data_classif_final %>%
  mutate(seg_pt = ifelse(segment == "Part-time", 1, 0),
         seg_temp = ifelse(segment == "Temporary", 1, 0),
         seg_insecure = seg_pt + seg_temp) %>%
  group_by(PL050) %>%
  summarize(prop_PT = mean(seg_pt, na.rm = T),
            prop_TEMP = mean(seg_temp, na.rm = T),
            prop_INSECURE = mean(seg_insecure, na.rm = T)) -> occup_insec_summary



t1 <- xtabs( ~ year2010 + year2009, data = data_wide3)
t2 <- xtabs( ~ year2011 + year2010, data = data_wide3)

t_tot <- t1 + t2

# % of people transitioning y2y
diag(t_tot) / colSums(t_tot)

colSums(t1)

t_tot_m <- as.matrix(t_tot)[-c(1:10), -c(1:10)]
t_tot_m_norm <- t_tot_m / ( sqrt(rowSums(t_tot_m)) * sqrt(colSums(t_tot_m)) )
t_tot_m_normtrim <- ifelse(t_tot_m_norm < .01, 0, 1)

occup_insec_summary %>%
  filter(PL050 %in% row.names(t_tot_m_normtrim)) -> occup_insec_summary_trim
  

# Visualize the net
o2o <- graph_from_adjacency_matrix(t_tot_m_normtrim, mode = "directed")

# Network clustering via Louvain algo
# set.seed(124)
# louvain <- cluster_louvain(skilldat, weights = NULL)
# # vector of communities
# skill_cluster <- membership(louvain)
# skill_cluster <- ifelse(!(skill_cluster %in% c(15, 20, 43, 26, 28, 30, 31, 32, 33)),
#                         1, skill_cluster)
# 
# # Add clusters
V(o2o)$PT <- occup_insec_summary_trim$prop_PT
V(o2o)$TEMP <- occup_insec_summary_trim$prop_TEMP
V(o2o)$INSECURE <- occup_insec_summary_trim$prop_INSECURE
# V(skilldat)$prevalence <- summaries_per_skill_trimmed$skill_occurence

ggraph(o2o, layout = "kk") +
  geom_edge_link(alpha = 0.1) +
  geom_node_point(aes(color = TEMP), size = 9) +
  theme_void() +
  scale_color_viridis_c() +
  geom_node_text(aes(label = name), color = "white") +
  labs(caption = "More than 1% changing",
       title = "Occupation mobility network")
  #geom_node_text(aes(label = name)) +
################################################################################
############ QUICK SCRIPT TO CREATE THE POVERTY MAP FOR SOMALIA ################
################################################################################
devtools::load_all()

pacman::p_load("dplyr", "data.table", "povmap", "sf", "ggplot2", "viridis")

#### read in the data
spatial_dt <- readRDS("data-raw/data-full.rds")
spatial_dt <- spatial_dt$admin2_District

geosurvey_dt <- haven::read_dta("data-raw/SIHBS_SAE.dta")
geosurvey_dt <- geosurvey_dt[, c("hhsize", "wgt_adj2", "pcer", "poor_ub",
                                 "ubpl", "latitude", "longitude")]
geosurvey_dt$population_weight <- geosurvey_dt$wgt_adj2 * geosurvey_dt$hhsize


povrate_full <-
  geosurvey_dt %>%
  mutate(poor = ifelse(pcer < ubpl, 1, 0)) %>%
  summarise(weighted.mean(x = poor, w = wgt_adj2*hhsize, na.rm = TRUE))

povrate_geocoded <-
  geosurvey_dt %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  mutate(poor = ifelse(pcer < ubpl, 1, 0)) %>%
  summarise(weighted.mean(x = poor, w = wgt_adj2*hhsize, na.rm = TRUE))


geosurvey_dt <-
  geosurvey_dt %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(crs = 4326,
           agr = "constant",
           coords = c("longitude", "latitude"))

### ensure we have properly created the geospatial object from the survey data
shp_dt <- sf::st_read(dsn = "data-raw/som-shapefiles/admin2_District",
                      layer = "admin2_District")


ggplot() +
  geom_sf(data = shp_dt) +
  geom_sf(data = geosurvey_dt, color = "red", alpha = 0.5, size = 2) +
  labs(title = "Household Locations in Somalia") +
  theme_minimal()



### poverty mapping time!
shp_dt <-
  shp_dt %>%
  mutate(targetarea_codes = substr(admin2Pcod, 3, nchar(admin2Pcod))) %>%
  mutate(targetarea_codes = as.integer(targetarea_codes))


### merge with household survey
geosurvey_dt <-
  geosurvey_dt %>%
  st_join(shp_dt)

### compute poverty rates for households that don't merge
povrate_geomerged <-
geosurvey_dt %>%
  filter(!is.na(admin0Pcod)) %>%
  mutate(poor = ifelse(pcer < ubpl, 1, 0)) %>%
  summarise(weighted.mean(x = poor_ub, w = wgt_adj2*hhsize, na.rm = TRUE))


### compute fh model
geosurvey_dt <- as.data.table(geosurvey_dt)
domsize_dt <-
  geosurvey_dt[, sum(population_weight, na.rm = TRUE), by = targetarea_codes] %>%
  setnames(old = "V1", new = "domsize") %>%
  filter(!is.na(targetarea_codes))

geosurvey_dt[, poor := ifelse(pcer < ubpl, 1, 0)]

saepop_dt <-
  sae::direct(y = poor,
              dom = targetarea_codes,
              sweight = population_weight,
              domsize = domsize_dt,
              data = geosurvey_dt) %>%
  mutate(var = SD ^ 2)

setnames(saepop_dt, "Domain", "targetarea_codes")

spatial_dt <- as.data.table(spatial_dt)

saepop_dt <- as.data.table(saepop_dt)

spatial_dt <-
  spatial_dt %>%
  mutate(targetarea_codes = substr(admin2Pcod, 3, nchar(admin2Pcod))) %>%
  mutate(targetarea_codes = as.integer(targetarea_codes))

spatial_dt <- saepop_dt[spatial_dt, on = "targetarea_codes"]

na_vars <- colnames(spatial_dt)[apply(spatial_dt, 2, function(x) any(is.na(x)))]
na_vars <- na_vars[!(na_vars %in% c("SampSize", "Direct", "SD", "CV", "var",
                                    "year"))]

### create region dummy
# spatial_dt <-
#   spatial_dt %>%
#   cbind(spatial_dt[, dummify(admin1Pcod)])


candidate_vars <- colnames(spatial_dt)[!(colnames(spatial_dt) %in%
                                           c("SampSize", "Direct", "SD", "CV", "var",
                                             "year", "admin0Pcod", "admin1Pcod", "admin2Name",
                                             "admin2Pcod", "year", "estimated_population_current",
                                             "targetarea_codes", "Direct", na_vars))]

selvars_list <- countrymodel_select(dt = spatial_dt[!is.na(Direct),],
                                    xvars = candidate_vars,
                                    y = "Direct")

# haven::write_dta(spatial_dt[!is.na(Direct), c(candidate_vars, "Direct"), with = F],
#                  "data-clean/som_model_select.dta")

### combine data
combine_dt <- povmap::combine_data(pop_data = spatial_dt[, c(selvars_list,
                                                             "targetarea_codes"),
                                                         with = FALSE],
                                   pop_domains = "targetarea_codes",
                                   smp_data = spatial_dt[!is.na(Direct),
                                                         c("targetarea_codes",
                                                           "Direct",
                                                           "var",
                                                           "SampSize"),
                                                         with = FALSE],
                                   smp_domains = "targetarea_codes")


fhmodel_not <-
  povmap::fh(fixed = as.formula(paste("Direct ~ ", paste(selvars_list, collapse= "+"))),
             vardir = "var",
             combined_data = combine_dt,
             domains = "targetarea_codes",
             method = "ml",
             MSE = TRUE,
             mse_type = "analytical")

fhmodel_log <-
  povmap::fh(fixed = as.formula(paste("Direct ~ ", paste(selvars_list, collapse= "+"))),
             vardir = "var",
             combined_data = combine_dt,
             domains = "targetarea_codes",
             method = "ml",
             MSE = TRUE,
             mse_type = "analytical",
             transformation = "log",
             backtransformation = "bc_crude")

fhmodel_arcsin <-
  povmap::fh(fixed = as.formula(paste("Direct ~ ", paste(selvars_list, collapse= "+"))),
             vardir = "var",
             combined_data = combine_dt,
             domains = "targetarea_codes",
             method = "ml",
             MSE = TRUE,
             mse_type = "boot",
             transformation = "arcsin",
             backtransformation = "bc",
             eff_smpsize = "SampSize")


result_dt <- as.data.table(fhmodel_not$ind)




#### compare province level rates
fhpov_dt <-
  result_dt %>%
  merge(unique(spatial_dt[, c("estimated_population_current",
                              "targetarea_codes",
                              "admin1Pcod")]),
        by.y = "targetarea_codes",
        by.x = "Domain") %>%
  group_by(admin1Pcod) %>%
  summarise(provFH = weighted.mean(x = FH,
                                   w = estimated_population_current,
                                   na.rm = TRUE)) %>%
  merge(geosurvey_dt %>%
          group_by(admin1Pcod) %>%
          summarise(provDirect = weighted.mean(x = poor,
                                               w = population_weight,
                                               na.rm = TRUE)),
        by = "admin1Pcod")


### write the poverty rates to file

result_dt <-
  result_dt %>%
  merge(unique(spatial_dt[, c("targetarea_codes", "admin2Name")]),
        by.x = "Domain",
        by.y = "targetarea_codes",
        all.x = TRUE)

write.csv(result_dt, "data-clean/ebp_results/fh_model_not_admin2.csv")


### some post estimation statistics
coeftable_dt <- fh_reportcoef_table(model = fhmodel_not)

write.csv(coeftable_dt, "data-clean/ebp_results/fh_model_not_coeftable_admin2.csv")


### comparing Direct vs Small Area Estimates
plota <-
  result_dt %>%
  ggplot() +
  geom_point(aes(x = Direct,
                 y = FH)) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red") +
  ylab("FH Model Estimates") +
  xlab("Direct Estimates") +
  theme_bw()

#### include the names to the fhpov_dt for the table
provdomsize_dt <-
  geosurvey_dt[, sum(population_weight, na.rm = TRUE), by = admin1Pcod] %>%
  setnames(old = "V1", new = "domsize") %>%
  filter(!is.na(admin1Pcod))

directpov_dt <-
  sae::direct(y = poor,
              dom = admin1Pcod,
              sweight = population_weight,
              domsize = provdomsize_dt,
              data = geosurvey_dt[!is.na(admin1Pcod),]) %>%
  setnames(.,"Domain", "admin1Pcod") %>%
  mutate(var = SD ^ 2)

provpov_dt <-
  directpov_dt %>%
  merge(fhpov_dt[, c("admin1Pcod", "provFH")],
        by = "admin1Pcod")

provpov_dt <- as.data.table(provpov_dt)

provpov_dt[, DirectLB := Direct - (SD)*1.96]
provpov_dt[, DirectUB := Direct + (SD)*1.96]

provshp_dt <- st_read("data-raw/som-administrative-divisions-shapefiles/Som_Admbnda_Adm1_UNDP.shp")

provpov_dt <-
  provpov_dt %>%
  merge(provshp_dt[, c("admin1Name", "admin1Pcod")] %>% st_drop_geometry())

provpov_dt %>%
  ggplot(aes(x = admin1Name)) +
  geom_point(aes(y = provFH), color = "blue", size = 2) +  # Plotting provFH
  geom_errorbar(aes(ymin = DirectLB, ymax = DirectUB), width = 0.2, color = "red") +  # Error bars for DirectLB and DirectUB
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Adding a horizontal line at y = 0
  labs(x = "Province", y = "Poverty Rate") +  # Labeling axes
  theme_bw() +  # Setting a white background theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability


write.csv(provpov_dt[, c("admin1Name","Direct", "DirectLB", "DirectUB")] %>% st_drop_geometry(),
          "data-clean/ebp_results/fhmodel_not_directprovpovrates.csv")



# communeshp_dt <-
#   communeshp_dt %>%
#   merge(result_dt, all = TRUE)

## includ population
povshp_dt <-
  shp_dt %>%
  merge(spatial_dt[, c("admin2Pcod", "estimated_population_current")],
        by = "admin2Pcod") %>%
  merge(result_dt[, c("Domain", "FH", "Direct")],
        by.x = "targetarea_codes",
        by.y = "Domain")


povshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Poverty Rate")

ggsave("figures/poverty_map_fhmodelnot.png")

povshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH * estimated_population_current)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Population \nof Poor")

ggsave("figures/poverty_map_count_fhmodelnot.png")












################################################################################
############ QUICK SCRIPT TO CREATE THE POVERTY MAP FOR SOMALIA ################
################################################################################
# devtools::load_all()

pacman::p_load("dplyr", "data.table", "povmap", "sf", "ggplot2", "viridis",
               "gridExtra")

#### read in the data
spatial_dt <- readRDS("data-raw/data-full.rds")
spatial_dt <- spatial_dt$admin2_District

geosurvey_dt <- haven::read_dta("data-raw/SIHBS_SAE (1).dta")
geosurvey_dt <- geosurvey_dt[, c("hhsize", "wgt_adj2", "pcer", "poor_ub",
                                 "ubpl", "latitude", "longitude", "admin2Pcod",
                                 "hhid")]

#### we need to remove the 3 areas the NSO flag as missing spatial_dt and the
#### survey
geosurvey_dt <- geosurvey_dt[!geosurvey_dt$admin2Pcod %in%
                               c("SO1104", "SO1503", "SO2601"),]

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


# geosurvey_dt <-
#   geosurvey_dt %>%
#   filter(!is.na(latitude) & !is.na(longitude)) %>%
#   st_as_sf(crs = 4326,
#            agr = "constant",
#            coords = c("longitude", "latitude"))

### ensure we have properly created the geospatial object from the survey data
shp_dt <- sf::st_read(dsn = "data-raw/som-shapefiles/admin2_District",
                      layer = "admin2_District")


# ggplot() +
#   geom_sf(data = shp_dt) +
#   geom_sf(data = geosurvey_dt, color = "red", alpha = 0.5, size = 2) +
#   labs(title = "Household Locations in Somalia") +
#   theme_minimal()



### poverty mapping time!
shp_dt <-
  shp_dt %>%
  mutate(targetarea_codes = substr(admin2Pcod, 3, nchar(admin2Pcod))) %>%
  mutate(targetarea_codes = as.integer(targetarea_codes))


### merge with household survey with 2 pronged approach (one for the households with admin2Pcod and
### another for households without admin2Pcod)
geosurvey_dt <-
  geosurvey_dt %>%
  merge(shp_dt %>%
          st_drop_geometry(),
        by = "admin2Pcod",
        all.x = TRUE)

# missing_dt <-
#   add_dt %>%
#   filter(admin2Pcod == "" & !is.na(longitude) & !is.na(latitude)) %>%
#   st_as_sf(crs = 4326,
#            agr = "constant",
#            coords = c("longitude", "latitude"))
# missing_dt$admin2Pcod <-
#   missing_dt %>%
#   st_nearest_feature(shp_dt[,colnames(shp_dt)[!colnames(shp_dt) %in% colnames(add_dt)]]) %>%
#   shp_dt$admin2Pcod[.]
#
#
# geosurvey_dt <-
#   add_dt %>%
#   filter(!admin2Pcod == "") %>%
#   select("hhsize", "wgt_adj2", "pcer", "poor_ub",
#          "ubpl", "admin2Pcod", "hhid") %>%
#   rbind(missing_dt %>%
#           st_drop_geometry() %>%
#           select("hhsize", "wgt_adj2", "pcer", "poor_ub",
#                  "ubpl", "admin2Pcod", "hhid")) %>%
#   merge(shp_dt[, c("admin0Pcod", "admin1Pcod", "admin2Name",
#                    "admin2Pcod", "targetarea_codes")] %>%
#           st_drop_geometry(),
#         by = "admin2Pcod")

# ggplot() +
#   geom_sf(data = shp_dt) +
#   geom_sf(data = missing_dt, color = "red", alpha = 0.5, size = 2) +
#   labs(title = "Household Locations in Somalia") +
#   theme_minimal()


### compute poverty rates for households that don't merge
# povrate_geomerged <-
# geosurvey_dt %>%
#   filter(!is.na(admin0Pcod)) %>%
#   mutate(poor = ifelse(pcer < ubpl, 1, 0)) %>%
#   summarise(weighted.mean(x = poor_ub, w = wgt_adj2*hhsize, na.rm = TRUE))


### compute fh model
geosurvey_dt <- as.data.table(geosurvey_dt)
geosurvey_dt[, population_weight := hhsize * wgt_adj2]
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

### include Kamwoo's wealth indices
wealth_dt <-
  read.csv("data-raw/SOM_estimated_wealth_index.csv") %>%
  st_as_sf(crs = 4326,
           agr = "constant",
           coords = c("lon", "lat")) %>%
  select(country_name, img_prob_poor, img_prob_lower_middle,
         img_prob_upper_middle, img_prob_rich, estimated_IWI)

wealth_cols <- c("img_prob_poor", "img_prob_lower_middle",
                 "img_prob_upper_middle", "img_prob_rich",
                 "estimated_IWI")

### compute target area averages
wealth_dt <-
  wealth_dt %>%
  st_join(shp_dt) %>%
  st_drop_geometry() %>%
  group_by(targetarea_codes) %>%
  summarise(img_prob_poor = mean(img_prob_poor, na.rm = TRUE),
            img_prob_lower_middle = mean(img_prob_lower_middle, na.rm = TRUE),
            img_prob_upper_middle = mean(img_prob_upper_middle, na.rm = TRUE),
            img_prob_rich = mean(img_prob_rich, na.rm = TRUE),
            estimated_IWI = mean(estimated_IWI, na.rm = TRUE))

### include dummies
spatial_dt <- merge(spatial_dt, wealth_dt, by = "targetarea_codes")

dummy_dt <- as.data.table(dummify(spatial_dt$admin1Pcod))

spatial_dt <- cbind(spatial_dt, dummy_dt)

candidate_vars <- c(candidate_vars, colnames(wealth_dt), colnames(dummy_dt))

candidate_vars <- candidate_vars[!grepl("plant_area_", candidate_vars)]

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

## include population
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

povshp_dt <-
povshp_dt %>%
  mutate(poppoor = estimated_population_current * FH)

#### save an excel file for Alastair
write.csv(povshp_dt %>% st_drop_geometry(),
          "data-clean/ebp_results/povertymap_som.csv")



#### benchmark the values
popshare_dt <-
  fhmodel_not$ind %>%
  merge(spatial_dt[, c("targetarea_codes", "estimated_population_current", "admin1Pcod")],
        by.x = "Domain", by.y = "targetarea_codes") %>%
  group_by(admin1Pcod) %>%
  mutate(popshare = estimated_population_current / sum(estimated_population_current, na.rm = TRUE))


bench_dt <-
  spatial_dt[, c("targetarea_codes", "admin1Pcod")] %>%
  merge(geosurvey_dt %>%
          group_by(admin1Pcod) %>%
          mutate(poor = ifelse(pcer < ubpl, 1, 0)) %>%
          summarise(benchrate = weighted.mean(x = poor, w = wgt_adj2*hhsize, na.rm = TRUE)),
        all.x = TRUE, by = "admin1Pcod")

### assign the national average to the missing regional poverty rate
bench_dt[is.na(benchrate), benchrate := 0.674]



benchrake_dt <- benchmark2(object = fhmodel_not,
                           benchmark = bench_dt$benchrate,
                           share = popshare_dt$popshare,
                           type = "raking",
                           group = bench_dt$admin1Pcod) %>%
  setnames(old = "FH_Bench",
           new = "FH_Bench_rake")


benchrate_dt <- benchmark2(object = fhmodel_not,
                           benchmark = bench_dt$benchrate,
                           share = popshare_dt$popshare,
                           type = "ratio",
                           group = bench_dt$admin1Pcod) %>%
  setnames(old = "FH_Bench",
           new = "FH_Bench_ratio")

benchmsea_dt <- benchmark2(object = fhmodel_not,
                           benchmark = bench_dt$benchrate,
                           share = popshare_dt$popshare,
                           type = "MSE_adj",
                           group = bench_dt$admin1Pcod) %>%
  setnames(old = "FH_Bench",
           new = "FH_Bench_mse")


#### merge all three methods
result_dt <-
  result_dt %>%
  merge(benchrake_dt[, c("Domain", "FH_Bench_rake")]) %>%
  merge(benchrate_dt[, c("Domain", "FH_Bench_ratio")]) %>%
  merge(benchmsea_dt[, c("Domain", "FH_Bench_mse")]) %>%
  merge(spatial_dt[, c("targetarea_codes", "estimated_population_current", "admin1Pcod")],
        by.x = "Domain", by.y = "targetarea_codes")

#### plot all three maps for the three different benchmarking methods

povshp_dt <-
  povshp_dt %>%
  merge(result_dt[, c("Domain", "FH_Bench_rake", "FH_Bench_ratio", "FH_Bench_mse")],
        by.x = "targetarea_codes", by.y = "Domain")

plot_fh <-
povshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Poverty Rate")

plot_fhrake <-
  povshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH_Bench_rake)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Benchmark Poverty Rate \n (Raking Method)")

plot_fhratio <-
  povshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH_Bench_ratio)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Benchmark Poverty Rate \n (Ratio Method)")

plot_fhmse <-
  povshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH_Bench_mse)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Benchmark Poverty Rate \n (MSE Adjustment)")

pdf("figures/poverty_map_fhmodelnot_withbenchmarking.pdf", width = 10,
    height = 12)

grid.arrange(grobs = list(plot_fh, plot_fhratio, plot_fhrake, plot_fhmse),
             nrow = 2)


dev.off()

write.csv(result_dt, "data-clean/ebp_results/povmap_benchmark.csv")



################################################################################
##### CHECKING THE SAMPLING STRUCTURE OF THE SIHBS FOR THE 3 PROBLEM AREAS #####
################################################################################

##### ---------- check for the possibility of undersampling ------------- ######

### first, include the survey weights

povshp_dt <- merge(povshp_dt,
                   geosurvey_dt %>%
                     group_by(targetarea_codes) %>%
                     summarize(population_weight = sum(population_weight, na.rm = TRUE)),
                   all.x = TRUE)

povshp_dt <- merge(povshp_dt,
                   saepop_dt[, c("targetarea_codes", "SampSize")],
                   all.x = TRUE)

povshp_dt <- merge(povshp_dt,
                   saepop_dt[, c("targetarea_codes", "CV")],
                   all.x = TRUE)

#### rank CVs
# write.csv(povshp_dt %>%
#             st_drop_geometry() %>%
#             select(admin2Name, CV) %>%
#             arrange(desc(CV)) %>%
#             filter(!is.na(CV)),
#           "data-clean/direct_CV_sample.csv")

# #### include the poverty rates estimated
# povshp_dt <- merge(povshp_dt,
#                    fhmodel_not$ind[, c("Domain", "FH", "Out")] %>%
#                      rename(targetarea_codes = Domain,
#                             FH_fix = FH))


####

povshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Head Count \nPoverty Rate")





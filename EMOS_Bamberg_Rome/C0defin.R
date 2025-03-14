install.packages("dplyr")
library(openeo)
library(raster)
library(sf)
library(dplyr)

##########
connect(host = "https://openeo.dataspace.copernicus.eu")
login()

#################
#list_collections()
#describe_collection("SENTINEL3_SLSTR")
#list_processes()
#describe_process("aggregate_temporal")
#process_viewer("load_collection")
f <- list_file_formats()
p <- processes()
########

#grundlegendes:


TimeFire <- list("2020-07-04",
                 "2020-07-04")
TimeCovariates <- list("2020-07-03",
                       "2020-07-03")
Region <- list(west=7.20,south=51.85, east=7.205,north=51.855)

data <- get_data_fire(TimeFire,TimeCovariates,Region)
write.csv(data, "data.csv", row.names = FALSE)
################################

# Otero de Rosdes - Grece
TimeFireOtero <- list("2022-06-16",
                      "2022-06-16")
TimeCovariatesOtero <- list("2022-06-11",
                            "2022-06-15")
RegionOtero <- list(west=-6.2205, east=-6.1268, south=41.8771,
                    north=41.9259)

Otero <- get_data_fire(TimeFireOtero,TimeCovariatesOtero, RegionOtero )
write.csv(Otero, "Otero.csv", row.names = FALSE)

# Athen - Grece
TimeFireAthen <- list("2023-07-18",
                      "2023-07-19")
TimeCovariatesAthen <- list("2023-07-14",
                            "2023-07-18")
RegionAthen <- list(west = 23.427866, east = 23.477176,
                    south = 38.014774, north = 38.090272)

Athen <- get_data_fire(TimeFireAthen, TimeCovariatesAthen, RegionAthen )
write.csv(Athen, "Athen.csv", row.names = FALSE)


# Monchique - Portugal
TimeFireMonchique <- list("2018-08-03",
                          "2018-08-08")
TimeCovariatesMonchique <- list("2018-07-30",
                                "2018-08-03")
RegionMonchique <- list(west = -8.616817, east = -8.416549,
                        south = 37.205651, north = 37.366324)

Monchique <- get_data_fire(TimeFireMonchique, TimeCovariatesMonchique,
                           RegionMonchique )
write.csv(Monchique, "Monchique.csv", row.names = FALSE)

# Lysychansk - Ukrain
TimeFireLysychansk <- list("2020-07-06",
                           "2020-07-07")
TimeCovariatesLysychansk <- list("2020-07-02",
                                 "2020-07-06")
RegionLysychansk <- list(west = 38.560076, east = 38.785555,
                         south = 48.831838, north = 48.956929)

Lysychansk <- get_data_fire(TimeFireLysychansk, TimeCovariatesLysychansk,
                            RegionLysychansk)
write.csv(Lysychansk, "Lysychansk.csv", row.names = FALSE)

# Argeles sur Mer - France
TimeFireArgeles <- list("2023-08-14",
                        "2023-08-14")
TimeCovariatesArgeles <- list("2023-08-10",
                              "2023-08-14")
RegionArgeles <- list(west = 2.954068, east = 3.166473,
                      south = 42.398519, north = 42.559469)

Argeles < get_data_fire(TimeFireArgeles, TimeCovariatesArgeles, 
                        RegionArgeles)

write.csv(Argeles, "Argeles.csv", row.names = FALSE)


# Andalucia - Spain - Drought Region
TimeDrought <- list("2023-07-13",
                    "2023-07-15")
TimeCovariatesDrought <- list("2023-07-09",
                              "2023-07-13")
RegionDrought <- list(west = -4.827390, east = -4.516992,
                      south = 37.117626, north = 37.330726)

Drought <- get_fire_data(TimeDrought, TimeCovariatesDrought, RegionDrought )

write.csv(Drought, "Drought.csv", row.names = FALSE)


# Norway - Winter Region
TimeWinter <- list("2023-07-13",
                   "2023-07-15")
TimeCovariatesWinter <- list("2023-07-09",
                             "2023-07-13")
RegionWinter <- list(west = 8.496667, east = 8.902804,
                     south = 60.704283, north = 60.939341)

Winter <- get_data_fire(TimeWinter, TimeCovariatesWinter, RegionWinter)

write.csv(Winter, "Winter.csv", row.names = FALSE)


# Stikli-Lettland  
TimeFireStikli <- list("2018-07-18",
                       "2018-07-22")
TimeCovariatesStikli <- list("2018-07-14",
                             "2018-07-18")
RegionStikli <- list(west = 22.124602, east = 22.393321,
                     south = 57.287556, north = 57.434097)

Stikli <- get_data_fire(TimeFireStikli, TimeCovariatesStikli, RegionStikli )

write.csv(Stikli, "Stikli.csv", row.names = FALSE)


# Valencia - Spain
TimeFireValencia <- list("2023-11-02",
                         "2023-11-03")
TimeCovariatesValencia <- list("2023-10-30",
                               "2023-11-03")
RegionValencia <- list(west = -1.010814, east = -0.032674,
                       south = 38.839374, north = 39.542642)

Valencia <- get_data_fire(TimeFireValencia, TimeCovariatesValencia, 
                          RegionValencia)
write.csv(Valencia, "Valencia.csv", row.names = FALSE)


###############################
#joint dataset:

dataset_model <- rbind(Otero, Athen, Monchique, Lysychansk, 
                       Argeles, Valencia, Stikli, Drought, Winter)

#predictions:

#rf_model <- lm(F1 ~ . , data = sf_joined_final_data)

rf_model <- lm(F1 ~ . , data = dataset_model)

# Perform stepwise selection (both directions)
stepwise_model <- step(rf_model)
summary(stepwise_model)


#data_today <- data.frame(LST = 278.984, B03 = 0.4767059)
#predictions <- predict(rf_model, data_today)
saveRDS(rf_model, file = "FireHazardPred/rf_model_test.rds")

# ggplot(data = fire_band_table) +
#   geom_sf(aes(fill = F1)) +  # Fill polygons based on 'F1' values
#   theme_minimal() +           # Minimal theme for clean look
#   labs(title = "Polygon Plot", fill = "topo") 
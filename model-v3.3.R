
library(glmmTMB)
library(usdm)

set.seed(1)

data_in <- read.table("sporulation_weather_summary_atleast50%weatherdata_MINMAX.csv", header=TRUE, sep = ",")
head(data_in)
nrow(data_in)
spor_events_data <- data_in

colnames(spor_events_data)
spor_events_clean <- spor_events_data[ , c("spores_gram", "sector",
                                            "mean_temp_1daysbefore", "mean_humidity_1daysbefore",
                                            "mean_temp_3daysbefore", "mean_humidity_3daysbefore",
                                            "mean_temp_7daysbefore", "mean_humidity_7daysbefore", 
                                            "mean_temp_15daysbefore", "mean_humidity_15daysbefore",
                                            "mean_temp_30daysbefore", "mean_humidity_30daysbefore",
                                            "mean_temp_90daysbefore", "mean_humidity_90daysbefore",
                                            "min_temp_1daysbefore", "max_temp_1daysbefore",
                                            "min_humidity_1daysbefore", "max_humidity_1daysbefore",
                                            "min_temp_3daysbefore", "max_temp_3daysbefore",
                                            "min_humidity_3daysbefore")] # vars to keep

head(spor_events_clean)
spor_events_clean <- spor_events_clean[complete.cases(spor_events_clean), ]
nrow(spor_events_clean)

#spor_events_clean$sector <- as.factor(spor_events_clean$sector)
colnames(spor_events_clean)

#Identify collinear (redundant) variables
vif_results <- vifstep(spor_events_clean[ , -c(2)], th=5) # must remove sector to run

#Visually check which variables are redundant and should be excluded from the model
vif_results

data4model <- spor_events_clean[ , c("spores_gram", "sector",
                                     "mean_humidity_7daysbefore", 
                                     "mean_temp_90daysbefore",
                                     "mean_humidity_90daysbefore",
                                     "min_humidity_1daysbefore",
                                     "max_humidity_1daysbefore",
                                     "max_temp_3daysbefore",
                                     "min_temp_3daysbefore",
                                     "min_humidity_3daysbefore")]

#head(data4model)
colnames(data4model)
nrow(data4model)


# sample <- data4model
# sample <- sample(c(TRUE, FALSE), nrow(data4model), replace=TRUE, prob=c(0.7,0.3))
# train_set <- data4model[sample, ]
# test_set <- data4model[!sample, ]


#Calibrate the model
model_v3_3 <- glmmTMB(spores_gram ~ mean_humidity_7daysbefore +
                        mean_temp_90daysbefore + 
                        mean_humidity_90daysbefore +
                        min_humidity_1daysbefore +
                        max_humidity_1daysbefore +
                        max_temp_3daysbefore +
                        min_temp_3daysbefore +
                        min_humidity_3daysbefore +
                      (1|sector),
                      na.action = na.omit,
                      data = data4model, family = nbinom2)


#Check relationships with predictive variables
summary(model_v3_3)




# 
# 
# 
# 
# #Prever contagem de esporos para novos valores de temperatura e humidade 
# new_values <- data.frame(17, 90, "NA") #Valores de exemplo
# colnames(new_values) <- c("Temp_3day", "Hr_3day", "sector")
# 
# predict( model_v1, new_values, type = "response", allow.new.levels=TRUE)


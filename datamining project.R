data <- read.csv("C:/Users/polex/Downloads/2017_Green_Taxi_Trip_Data11.csv")
cor_matrix <- cor(data, use = "complete.obs")
high_corr_pairs <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) != 1, arr.ind = TRUE)
high_corr_pairs
means <- colMeans(data, na.rm = TRUE)
sds <- apply(data, 2, sd, na.rm = TRUE)
descriptive_stats <- data.frame(Mean = means, SD = sds)
print(descriptive_stats)
model1 <- lm(fare_amount ~ passenger_count + trip_distance + mta_tax + tip_amount, data = data)
model2 <- lm(fare_amount ~ passenger_count + trip_distance + mta_tax + tip_amount + extra, data = data)
model3 <- lm(fare_amount ~ passenger_count + trip_distance + mta_tax + tip_amount + extra, data = data)
models <- list(model1 = model1, model2 = model2, model3 = model3)
model_metrics <- data.frame(
  Model = names(models),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC),
  Adjusted_R2 = sapply(models, function(model) summary(model)$adj.r.squared)
)
sorted_by_aic <- model_metrics[order(model_metrics$AIC), ]
print(sorted_by_aic)
sorted_by_adj_r2 <- model_metrics[order(-model_metrics$Adjusted_R2), ]
print(sorted_by_adj_r2)
res<-cor.test(data$trip_distance,data$fare_amount,method='pearson')
res
##Plot of the 3rd plot ,residuals

par(mfrow=c(2,2))#plots for residuels qq
plot(model1)#
par(mfrow=c(2,2))#
plot(model2)#
par(mfrow=c(2,2))#
plot(model3)#

## Plot of box plot run this as 2nd plot
par(mfrow = c(1, 1))  
for (col in colnames(data)) {
  boxplot(data[[col]], main = col, ylab = "Value")
}
# Scatter plot 1.
num_cols <- unlist(lapply(data, is.numeric))
plot(data[,num_cols]) 


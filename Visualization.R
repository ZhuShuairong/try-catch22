library(ggplot2)
library(reshape2)
#Numerical
ggplot(data = cleaned_data, aes(x = totalAreaOfBuildingTransferredInSquareMeters, y = totalPriceRMB)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Total Building Area vs. Total Price", x = "Building Area (m²)", y = "Total Price (RMB)")

cor_matrix <- cor(cleaned_data[, sapply(cleaned_data, is.numeric)])  # Subset numeric columns
ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap")
#Categorical
ggplot(data = cleaned_data, aes(x = townshipAndCityDistrict, y = unitPriceRMBSqM)) +
  geom_boxplot() + 
  coord_flip() +
  labs(title = "Unit Price by Township", x = "Township", y = "Unit Price (RMB/m²)")

ggplot(data = cleaned_data, aes(x = totalAreaOfBuildingTransferredInSquareMeters, y = unitPriceRMBSqM)) +
  geom_point(alpha = 0.3) + 
  facet_wrap(~ townshipAndCityDistrict, ncol = 4) +
  labs(title = "Unit Price vs. Area by Township")

ggplot(data = cleaned_data, aes(x = elevator, y = totalPriceRMB)) +
  geom_bar(stat = "summary", fun = "median", errorbar_base = FALSE) +
  labs(title = "Median Total Price by Elevator Presence")

ggplot(data = cleaned_data, aes(x = totalAreaOfBuildingTransferredInSquareMeters, y = unitPriceRMBSqM)) +
  geom_hex(bins = 50) + 
  scale_fill_viridis_c() +
  labs(title = "Density of Unit Price vs. Building Area")

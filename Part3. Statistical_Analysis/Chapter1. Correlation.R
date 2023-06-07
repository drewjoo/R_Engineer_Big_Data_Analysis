# part3
# Chapter1. Correlation

# Loda data
data('airquality')
str(airquality)

# Create air data
air = airquality[,c(1:4)]
str(air)

# Pearson Correlation
cor(air, use = "pairwise.complete.obs", method = 'pearson')

# Kendal Correaltion
cor(air, use = "pairwise.complete.obs", method = 'kendall')

# Spearman Correlation
cor(air, use = 'pairwise.complete.obs', method = 'spearman')

# Correlation Matrix
air_cor = cor(air, use = 'pairwise.complete.obs')
air_cor

# Visualize with plot
pairs(air_cor)

# Correlation test
cor.test(air$Ozone, air$Wind, method = 'pearson')

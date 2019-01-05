WholesaleCustomersData = read.csv("WholesaleCustomersData.csv", header = TRUE)

names(WholesaleCustomersData) = c("Region", "Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicassen")

wholesale = WholesaleCustomersData[, -1]

data <- na.omit(wholesale) # Remove missing values (NA)
data <- scale(data) # Scale variables

#install Rlof

library(Rlof)

k = 20
cores = NULL

res.lof <- lof(data, k)

outliers <- order(res.lof, decreasing = T)[1:16]

data2 <- data[, 1:4]

n <- nrow(data2)

pch <- rep(".", n)
pch[outliers] <- "+"

col <- rep("black", n)
col[outliers] <- "red"

cex <- 2

pairs(data2, pch = pch, col = col, cex = cex)
#Multiple Analysis Regression
library(lmtest)
library(nortest)
library(ggplot2)
library(car)

# Mengimport data
df <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(153000, 112000, 176000, 139000, 125000, 212000, 229000, 205000, 234000, 245000, 257000, 268000),
  x2 = c(8300, 9700, 10700, 12500, 13200, 9900, 13500, 12800, 14700, 15900, 18000, 21000),
  x3 = c(5, 6, 8, 4, 5, 7, 9, 5, 2, 5, 6, 5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(28000, 21000, 34000, 33000, 39000, 20000, 21000, 31000, 49000, 42000, 57000, 66000),
  y = c(100, 110, 150, 145, 190, 160, 180, 220, 210, 220, 390, 320))

# Load the regression model
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = df) # Assuming 'data' is your dataset
summary(model)

# Uji Autokorelasi
dwtest(model)

# Uji Homoskedastisitas
bptest(model, studentize = T, data=df)

# Uji Normalitas
lillie.test(model$residuals)

# Uji Multikolinearitas
vif(model)

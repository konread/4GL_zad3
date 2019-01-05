library(FuzzyNumbers)
library(ggplot2)

wiekMlody <- TrapezoidalFuzzyNumber(0, 0, 16, 25)
wiekSredni <- TrapezoidalFuzzyNumber(18, 26, 55, 60)
wiekStary <- TrapezoidalFuzzyNumber(55, 60, 100, 100)

R1 <- vector()
for (n in c(0:60)) {
    value <- (1 / n) * evaluate(wiekMlody, n) * evaluate(wiekSredni, n)
    value <- replace(value, is.na(value), 0)
    R1 <- c(R1, value)
}

R2 <- vector()
for (n in c(0:60)) {
    value <- (evaluate(wiekMlody, n) * evaluate(wiekSredni, n)) / evaluate(wiekMlody, n)
    value <- replace(value, is.na(value), 0)
    R2 <- c(R2, value)
}

plot(wiekMlody, xlim = c(0, 100), ylim = c(0, 1), xlab = "Wiek", ylab = "Przynale¿noœæ", col = "red")
plot(wiekSredni, col = "blue", add = TRUE)
plot(wiekStary, col = "orange", add = TRUE)
legend(90, 0.5, legend = c("wiekMlody", "wiekSredni", "wiekStary"),
       col = c(" red ", " blue ", " orange "), lty = 1, cex = 0.8,
       title = "Legenda", text.font = 4, bg = 'lightblue')

plot(R1, col = "blue", xlab = "Wiek")
plot(R2, col = "green", xlab = "Wiek")
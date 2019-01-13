library(FuzzyNumbers)
library(ggplot2)

scaleFunction <- function(x) {(x - min(x)) / (max(x) - min(x)) }

R1 <- function(przynaleznosc1, przynaleznosc2) {
    R1 <- vector()
    s <- seq(0, 1, by = 0.01)
    for (i in s) {
        value <- (1 / i) * evaluate(przynaleznosc1, i) * evaluate(przynaleznosc2, i)
        value <- replace(value, is.na(value), 0)
        value <- replace(value, is.infinite(value), 0)
        R1 <- c(R1, value)
    }

    return (list("sum"=sum(R1)/sum(s),"array"=R1))
}

R2 <- function(przynaleznosc1, przynaleznosc2) {
    R2 <- vector()
    s <- seq(0, 1, by = 0.01)
    for (i in s) {
        value <- (evaluate(przynaleznosc1, i) * evaluate(przynaleznosc2, i)) / evaluate(przynaleznosc1, i)
        value <- replace(value, is.na(value), 0)
        value <- replace(value, is.infinite(value), 0)
        R2 <- c(R2, value)
    }

    return(list("sum" = sum(R2) / sum(s), "array" = R2))
}

wiekList <- c(0, 0, 16, 40, 18, 26, 55, 65, 30, 60, 100, 100)
wiekList <- scaleFunction(wiekList)

wiekMlody <- TrapezoidalFuzzyNumber(wiekList[1], wiekList[2], wiekList[3], wiekList[4])
wiekSredni <- TrapezoidalFuzzyNumber(wiekList[5], wiekList[6], wiekList[7], wiekList[8])
wiekStary <- TrapezoidalFuzzyNumber(wiekList[9], wiekList[10], wiekList[11], wiekList[12])

wzrostList <- c(0, 0, 120, 155, 110, 130, 165, 180, 145, 175, 200, 200)
wzrostList <- scaleFunction(wzrostList)

wzrostNiski <- TrapezoidalFuzzyNumber(wzrostList[1], wzrostList[2], wzrostList[3], wzrostList[4])
wzrostSredni <- TrapezoidalFuzzyNumber(wzrostList[5], wzrostList[6], wzrostList[7], wzrostList[8])
wzrostWysoki <- TrapezoidalFuzzyNumber(wzrostList[9], wzrostList[10], wzrostList[11], wzrostList[12])


plot(wiekMlody, xlim = c(0, tail(wiekList, n = 1)), ylim = c(0, 1), xlab = "Wiek", ylab = "Przynale¿noœæ", col = "red")
plot(wiekSredni, col = "blue", add = TRUE)
plot(wiekStary, col = "orange", add = TRUE)
legend(0, 0.5, legend = c("wiekMlody", "wiekSredni", "wiekStary"),
       col = c(" red ", " blue ", " orange "), lty = 1, cex = 0.6,
       title = "Legenda", text.font = 4, bg = "lightblue")

plot(wzrostNiski, xlim = c(0, tail(wzrostList, n = 1)), ylim = c(0, 1), xlab = "Wzrost", ylab = "Przynale¿noœæ", col = "red")
plot(wzrostSredni, col = "blue", add = TRUE)
plot(wzrostWysoki, col = "orange", add = TRUE)
legend(0, 0.5, legend = c("wzrostNiski", "wzrostSredni", "wzrostWysoki"),
       col = c(" red ", " blue ", " orange "), lty = 1, cex = 0.6,
       title = "Legenda", text.font = 4, bg = "lightblue")


r1 <- R1(wiekMlody, wzrostNiski)
print(r1$sum)
s <- seq(0, 1, by = 0.01)
plot(s, r1$array, xlab = "Step by [0.01]", ylab = "R1")

r2 <- R2(wiekMlody, wzrostNiski)
print(r2$sum)
s <- seq(0, 1, by = 0.01)
plot(s, r2$array, xlab = "Step by [0.01]", ylab = "R2")

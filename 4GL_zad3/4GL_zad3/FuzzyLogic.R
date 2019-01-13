library(FuzzyNumbers)
library(ggplot2)

R1 <- function(listCech1, listCech2) {
    R1 <- vector()
    s <- c(1:length(listCech1))
    for (i in s) {
        value <- listCech1[i] * listCech2[i]
        R1 <- c(R1, value)
    }
    res <- (1 / length(listCech1)) * sum(R1)
    return(res)
}

R2 <- function(listCech1, listCech2) {
    R2c1c2 <- vector()
    R2c1 <- vector()
    s <- c(1:length(listCech1))
    for (i in s) {
        value1 <- listCech1[i] * listCech2[i]
        value1 <- replace(value1, is.na(value1), 0)
        value1 <- replace(value1, is.infinite(value1), 0)
        R2c1c2 <- c(R2c1c2, value1)

        R2c1 <- c(R2c1, listCech1[i])
    }

    res <- sum(R2c1c2) / sum(R2c1)
    return(res)
}

wiekList <- c(0, 0, 16, 40, 18, 26, 55, 65, 30, 60, 100, 100)

wiekMlody <- TrapezoidalFuzzyNumber(wiekList[1], wiekList[2], wiekList[3], wiekList[4])
wiekSredni <- TrapezoidalFuzzyNumber(wiekList[5], wiekList[6], wiekList[7], wiekList[8])
wiekStary <- TrapezoidalFuzzyNumber(wiekList[9], wiekList[10], wiekList[11], wiekList[12])

wzrostList <- c(60, 60, 120, 155, 110, 130, 165, 180, 145, 175, 220, 220)

wzrostNiski <- TrapezoidalFuzzyNumber(wzrostList[1], wzrostList[2], wzrostList[3], wzrostList[4])
wzrostSredni <- TrapezoidalFuzzyNumber(wzrostList[5], wzrostList[6], wzrostList[7], wzrostList[8])
wzrostWysoki <- TrapezoidalFuzzyNumber(wzrostList[9], wzrostList[10], wzrostList[11], wzrostList[12])

'
randomWiek <- sample(0:100, 1000, replace = TRUE)
row <- list("Wiek", "Wiek Mlody", "Wiek Sredni", "Wiek Stary")
write.table(row, file = "Wiek.csv", sep = ";", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)

for (i in randomWiek) {
    row <- list(i, evaluate(wiekMlody, i), evaluate(wiekSredni, i), evaluate(wiekStary, i))
    write.table(row, file = "Wiek.csv", sep = ";", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}
'
'
randomWzrost <- sample(60:220, 1000, replace = TRUE)
row <- list("Wzrost", "Wzrost Niski", "Wzrost Sredni", "Wzrost Wysoki")
write.table(row, file = "Wzrost.csv", sep = ";", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)

for (i in randomWzrost) {
    row <- list(i, evaluate(wzrostNiski, i), evaluate(wzrostSredni, i), evaluate(wzrostWysoki, i))
    write.table(row, file = "Wzrost.csv", sep = ";", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}
'
'
plot(wiekMlody, xlim = c(0, tail(wiekList, n = 1)), ylim = c(0, 1), xlab = "Wiek", ylab = "Przynale¿noœæ", col = "red")
plot(wiekSredni, col = "blue", add = TRUE)
plot(wiekStary, col = "orange", add = TRUE)
legend(0, 0.5, legend = c("wiekMlody", "wiekSredni", "wiekStary"),
       col = c(" red ", " blue ", " orange "), lty = 1, cex = 0.6,
       title = "Legenda", text.font = 4, bg = "lightblue")

plot(wzrostNiski, xlim = c(60, tail(wzrostList, n = 1)), ylim = c(0, 1), xlab = "Wzrost", ylab = "Przynale¿noœæ", col = "red")
plot(wzrostSredni, col = "blue", add = TRUE)
plot(wzrostWysoki, col = "orange", add = TRUE)
legend(60, 0.5, legend = c("wzrostNiski", "wzrostSredni", "wzrostWysoki"),
       col = c(" red ", " blue ", " orange "), lty = 1, cex = 0.6,
       title = "Legenda", text.font = 4, bg = "lightblue")
'

WzrostData = read.csv("Wzrost.csv", header = TRUE, sep = ";")
WiekData = read.csv("Wiek.csv", header = TRUE, sep = ";")

print("WiekData$Wiek.Stary, WzrostData$Wzrost.Niski")
r1 <- R1(WiekData$Wiek.Stary, WzrostData$Wzrost.Niski)
print(r1)
r2 <- R2(WiekData$Wiek.Stary, WzrostData$Wzrost.Niski)
print(r2)

print("WiekData$Wiek.Stary, WzrostData$Wzrost.Sredni")
r1 <- R1(WiekData$Wiek.Stary, WzrostData$Wzrost.Sredni)
print(r1)
r2 <- R2(WiekData$Wiek.Stary, WzrostData$Wzrost.Sredni)
print(r2)

print("WiekData$Wiek.Stary, WzrostData$Wzrost.Wysoki")
r1 <- R1(WiekData$Wiek.Stary, WzrostData$Wzrost.Wysoki)
print(r1)
r2 <- R2(WiekData$Wiek.Stary, WzrostData$Wzrost.Wysoki)
print(r2)
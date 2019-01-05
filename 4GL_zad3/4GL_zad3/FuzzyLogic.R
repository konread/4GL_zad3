library(FuzzyNumbers)

wiekMlody <- TrapezoidalFuzzyNumber(0, 0, 16, 25)
wiekSredni <- TrapezoidalFuzzyNumber(18, 26, 55, 60)
wiekStary <- TrapezoidalFuzzyNumber(55, 60, 100, 100)

'
plot(wiekMlody, xlim = c(0, 100))
plot(wiekSredni, add = TRUE)
plot(wiekStary, add = TRUE)
'

'
R1 <- vector()
for (n in c(0:60)) {
    value <- (1 / n) * evaluate(wiekMlody, n) * evaluate(wiekSredni, n)
    value <- replace(value, is.na(value), 0)
    R1 <- c(R1, value)
}
plot(R1)
'

R2 <- vector()
for (n in c(0:60)) {
    value <- (evaluate(wiekMlody, n) * evaluate(wiekSredni, n)) / evaluate(wiekMlody, n)
    value <- replace(value, is.na(value), 0)
    R2 <- c(R2, value)
}
plot(R2)
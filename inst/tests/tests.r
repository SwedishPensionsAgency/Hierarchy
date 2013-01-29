# test

data <- data.frame(id = c("1", "1.1", "1.1.1", "1.1.2", "1.12", "1.12.1", "1.12.2"), value = c(NA, NA, 5, 10, NA, 2, 3), value2 = c(NA, NA, 5333, 10, NA, 2, 3))
h <- Hierarchy(data, id = "id", metrics = c("value", "value2"))
calc(h, "mean", "1.1")
as.list(calc(h, "mean", "1.1"))
as.json(calc(h))  # default "sum".

# test

data <- data.frame(id = c("1", "1.1", "1.1.1", "1.1.2", "1.12", "1.12.1", "1.12.2"), value = c(NA, NA, 5, 10, NA, 2, 3), value2 = c(NA, NA, 5333, 10, NA, 2, 3))
h <- Hierarchy(data, id = "id", metrics = c("value", "value2"))
h["^1.1"]
aggr(h,"^1.1")
as.data.frame(t(aggr(h,"^1.1$", TRUE)))
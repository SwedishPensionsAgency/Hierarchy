# test

data <- data.table(id = c("1", "1.1", "1.1.1", "1.1.2", "1.2", "1.2.1", "1.2.2"), value = c(NA, NA, 5, 10, NA, 2, 3), value2 = c(NA, NA, 5, 10, NA, 2, 3))

h <- Hierarchy(data, id = "id", metrics = c("value", "value2"))
h["1.1.*"]
aggr(h["1.1.*"])

#subs(h, "1.1.*")

# data[grepl("\\d.", data$id)]

#x <- c("supp", "dose") 
#ddply(ToothGrowth, x, function(df) mean(df$len)) 
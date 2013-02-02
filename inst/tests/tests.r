# test
dt <- data.table(
    id = c("1", "1.1", "1.1.1", "1.1.2", "1.12", "1.12.1", "1.12.2"), 
    value = c(NA, NA, 5, 10, NA, 2, 3), 
    weight = c(NA, NA, 5333, 10, NA, 2, 3))
a <- Hierarchy$new(dt)

a$data()
a$descendants_ids("1.1")
a$descendants("1.1")

a$children_ids("1")
a$children("1")

a$parent_id("1.1.12")
a$parent("1.1.12")

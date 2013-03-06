
### EXAMPLE 1 ###
# ex1 <- aggr_by(x, ids = "1.1.1.2.2", end = 2)
# cast(ex1, Id + Label_en ~ variable, sum)

### EXAMPLE 2 ###
# ip <- aggr_by(x, ids = "1.1.1.2.2", end = 2)
# ip$forman <- "IP"
# pp <- aggr_by(x, ids = "1.2.1.2.2", end = 2)
# pp$forman <- "PP"
# combo <- rbind(ip, pp)
# combo$forman_ar <- paste(combo$forman, combo$variable)
# cast(combo, Label_en ~ forman_ar, sum)

### Example 3: subset & to_json ###
# a <- Hierarchy:::path_enum$new(data = x, metrics = c("value"))
# a$data(x = a$aggregate(), variable == "Y2011", select = c("Id", "Label_en", "value"))
# head(a$data())
# cat(a$to_json("1.1.1.2"))

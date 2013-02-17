require(Hierarchy)
require(reshape)
require(plyr)

x <- read.csv2("data/noter.csv")
x <- melt(x)

# Aggregate (add as wrapper function in package)
aggr_by <- function(data, path = colnames(data)[1], metrics = "value", ids = "1", by = "variable", ..., fun = function(x) sum(x, na.rm = TRUE)) {
    ddply(data, by, function(x) {
        a <- Hierarchy:::path_enum$new(data = x, path = path, metrics = metrics)
        a$aggregate(a$descendants_ids(ids, ...), fun)
    })
}

# EXAMPLE 1
ex1 <- aggr_by(x, ids = "1.1.1.2.2", end = 2)
cast(ex1, Id + Label_en ~ variable, sum)

# EXAMPLE 2
ip <- aggr_by(x, ids = "1.1.1.2.2", end = 2)
ip$forman <- "IP"
pp <- aggr_by(x, ids = "1.2.1.2.2", end = 2)
pp$forman <- "PP"
combo <- rbind(ip, pp)
combo$forman_ar <- paste(combo$forman, combo$variable)
cast(combo, Label_en ~ forman_ar, sum)



### TO JSON ###

data <- x
data$ndims <- sapply(gregexpr(sprintf("[^%s*]", "\\."), data$Id), length) - 1






require(RJSONIO)
makeList<-function(x){
    if(ncol(x)>3){
        listSplit<-split(x[-1],x[1],drop=T)
        lapply(names(listSplit),function(y){list(name=y,children=makeList(listSplit[[y]]))})
    }else{
        lapply(seq(nrow(x[1])),function(y){list(name=x[,1][y],value=x[,2][y],weight=x[,3][y])})
    }
}

jsonOut<-toJSON(list(name="MyData",children=makeList(x)))
sink("cpi.json")
cat(jsonOut)
sink()



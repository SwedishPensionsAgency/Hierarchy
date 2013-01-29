
# Convert data.frame to list
lst <- lst_fun(df, "id")

# Convert list to json
s <- RJSONIO::toJSON(lst)


# make makkink table

# load csv file with makkink per crop per month
dt <- fread('dev/bln_makkink.csv',header = TRUE)

# colnames for the 12 months
cols <- colnames(dt)[-1]

# replace all missing values with the value 0.36 for braak soil
dt[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),0.36,x)),.SDcols = cols]

# write file to data
bln_makkink <- copy(dt)

# write file to data
usethis::use_data(bln_makkink, overwrite = TRUE)

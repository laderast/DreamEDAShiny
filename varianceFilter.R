# Open Data File
data <- read.table("data/ViralChallenge_ExpressionRMA_early_timepoints.tsv")

# Calculate variance for each probe
var.by.row <- apply(data, MARGIN=1, FUN=var)
quantile(var.by.row,probs=seq(0, 1, .1))

# Calculate the range for each probe
range.by.row <- apply(data, MARGIN=1, FUN=range)
quantile(range.by.row,probs=seq(0, 1, .1))

# Function normalizes for max signal and then returns the amount of variance for the normalized row
normVar <- function(chunk) {
  return(var(chunk/max(chunk)))
}

normvar.by.row <- apply(data, MARGIN=1, FUN=normVar)
quantile(normvar.by.row,probs=seq(0, 1, .1))

# Filter using the variance metrics
normvar.filtered <- data[normvar.by.row > quantile(normvar.by.row,probs=seq(0, 1, .1))[9],]
dim(normvar.filtered)

var.filtered <- data[var.by.row > quantile(var.by.row,probs=seq(0, 1, .1))[9],]
dim(var.filtered)

# takes any probe making both normalized variance and raw variance lists in the top 20%
dual.filtered <- data[var.by.row > quantile(var.by.row,probs=seq(0, 1, .1))[9] & 
                        normvar.by.row > quantile(normvar.by.row,probs=seq(0, 1, .1))[9],]
dim(dual.filtered)

# takes any probe making either one of normalized variance and raw variance lists in the top 20% of their respective list
inclusive.filtered <- data[var.by.row > quantile(var.by.row,probs=seq(0, 1, .1))[9] | 
                             normvar.by.row > quantile(normvar.by.row,probs=seq(0, 1, .1))[9],]

# write output to file (substitute preferred set)
write.table(inclusiveFilter, "ViralChallenge_ExpressionRMA_varFilter_timepoints.tsv", quote=F, sep="\t")
# 1 PATHS
# Set parameters
set.seed(2025)

n <- 100
t <- seq(1, 550, 1)
lambda <- 0.591

# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(NA, nrow = n, ncol = length(t))

for (i in 1:n) {
	cval <- rpois(length(t), lambda)  
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]

# Reset graphics device to avoid layout issues
if (dev.cur() > 1) dev.off()

# Set up layout: Histogram (small, upper right) + Main plot
layout(matrix(c(1, 2), ncol = 2), widths = c(3, 1), heights = c(4, 1))  

# Plot the main accrual time series
plot(t,  cval_cum_matrix[1,], 
		 type="n", 
		 main = "Accrual of 100 studies", 
		 xlab = "Time", 
		 ylab = "Count")

lines(t, lambda*t)
lines(t,  cval_cum_matrix[1,], col = "lightgray")


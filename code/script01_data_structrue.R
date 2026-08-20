## Clayton Glasgow, BIO 708 Day 2
## data structure

# scalar
a <- 2.0 # numeric
b <- 4L # integer
d <- "aquatic"

a
b
d

# vector
va <- c(1.0, 2.2, 3) # numeric vector
vb <- c("a", "b", "c") # character vector
vc <- c("1", "2", "3.5") # character vector

# vd <- c(1.0, "b") -- character vector -- DON'T DO THIS

va
vb
vc

# matrix -- two dimensional organization of vector
va <- c(1.0, 2.2, 3) # numeric vector
vb <- c(3, 2, 5.6) # numeric vector

ma  <- cbind(va, vb) # vectors must have the same length
mb <- rbind(va, vb)

mc <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)

ma
mb
mc

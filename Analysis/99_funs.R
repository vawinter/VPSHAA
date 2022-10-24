# Ch 1: Function for combining lists together
# 2/9/2022

# create function to combine and rename
cat_lists <- function(x, y) {  
  
  keys <- unique(c(names(x), names(y)))
  map2(x[keys], y[keys], c) %>% 
    set_names(keys)  
  
}

# Let's write a function that will take our centroid coordinates and return an
# extent that covers 25 sq. km.
# Edit 1/12: 10 x 10 buffer (100 sq. km.)

# x: x-coordinate
# y: y-coordinate
# half_side: number giving half the length of a side of the square of the
# final exent. E.g., to get a 25 sq. km extent, each side of the square is
# 5 km, so half a side is 2.5 km = 2500 m
# 1/12: 10 km, half side is 5 km = 5000 m 
make_extent <- function(x, y, half_side = 50000) {
  xmin <- x - half_side
  xmax <- x + half_side
  ymin <- y - half_side
  ymax <- y + half_side
  # Create 'extent' object 
  ext <- extent(xmin, xmax, ymin, ymax)
  # Return
  return(ext)
}

# Scaling fun 
scale_co <- function(x){
  (x - mean(x))/ sd(x)
}

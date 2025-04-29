min_max_normalizacija <- function(x) {
  if (!is.numeric(x)) {
    stop("Ulaz mora biti numerički vektor.")
  }
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  
  if (min_x == max_x) {
    warning("Sve vrednosti su iste. Vraća se vektor nula.")
    return(rep(0, length(x)))
  }
  
  (x - min_x) / (max_x - min_x)
}


df$Fare_normalized <- min_max_normalizacija(df$Fare)
normalizovani_podaci <- min_max_normalizacija(podaci)
print(normalizovani_podaci)
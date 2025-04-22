m1 <- matrix(c(3,-1,2, 0, 4,-3), nrow = 2, ncol = 3, byrow = TRUE)
m2 <- matrix(c(1,0,-2,4,3,-1,5,0,-3,2, 2, 1, 3, 0, -1), nrow = 3, ncol = 5, byrow = TRUE)

m3 <- m1%*%m2

m4 <- t(m3)

pomnozi_matrice <- function(m1, m2) {
  if (ncol(m1) != nrow(m2)) {
    stop("Greška: Matrice se ne mogu pomnožiti. Broj kolona prve matrice nije jednak broju redova druge matrice.")
  } else {
    rezultat <- m1 %*% m2
    dimenzije <- dim(rezultat)
    poruka <- sprintf("Dimenzije nove matrice su: %d x %d", dimenzije[1], dimenzije[2])
    cat(poruka, "\n")
    return(rezultat)
  }
}


m3 <- m1%*%m2
m3

m4 <- t(m3)
m4
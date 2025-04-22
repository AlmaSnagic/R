Zivotinja <- function(ime, vrsta, godiste) {
  obj <- list(
    ime = ime,
    vrsta = vrsta,
    godiste = godiste,
    posjete = list()
  )
  class(obj) <- "Zivotinja"
  return(obj)
}


dodaj_posjetu.Zivotinja <- function(zivotinja, datum) {
  zivotinja$posjete <- append(zivotinja$posjete, list(datum))
  return(zivotinja)
}


prikazi.Zivotinja <- function(zivotinja) {
  cat(sprintf("Ime: %s\nVrsta: %s\nGodina rođenja: %d\nBroj posjeta: %d\n",
              zivotinja$ime, zivotinja$vrsta, zivotinja$godiste, length(zivotinja$posjete)))
  cat("Posjete: ", paste(zivotinja$posjete, collapse = ", "), "\n")
}

moj_ljubimac <- Zivotinja("Rex", "pas", 2018)



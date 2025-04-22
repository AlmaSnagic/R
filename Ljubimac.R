library(R6)

Ljubimac <- R6Class("Ljubimac",
  public = list(
    ime = NULL,
    vrsta = NULL,
    god_rodjenja = NULL,
    datum_pregleda = NULL,
    
  initialize = function(ime, vrsta, god_rodjenja, datum_pregleda) {
    self$ime <- ime
    self$vrsta <- vrsta
    self$god_rodjenja <- god_rodjenja
    self$datum_pregleda <- datum_pregleda
  },

  izracunaj_godine <- function(god_rodjenja) {
    danas <- Sys.Date()
    trenutna_godina <- as.integer(format(danas, "%Y"))
    trenutni_mjesec <- as.integer(format(danas, "%m"))
    trenutni_dan <- as.integer(format(danas, "%d"))
    
    godine <- trenutna_godina - god_rodjenja
    
    cat(sprintf("Osoba rođena %d. godine ima %d godina na današnji dan (%s).\n",
                god_rodjenja, godine, danas))
    
    return(godine)
  },
  
  pozdrav_ljubimac <- function(vrsta, ime) {
    poruka <- sprintf("Pozdrav od %s po imenu %s!", vrsta, ime)
    return(poruka)
  },
  
  dodaj_posjetu <- function(p) {
    danas <- Sys.Date()
    Ljubimac$datum_pregleda <- append(list(danas), Ljubimac$datum_pregleda)
    return(p)
  },
  
  )
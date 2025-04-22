df <- data.frame(
  ime = c("A", "B", "C"),
  prezime = c("xx", "YY", "ZZ"),
  godiste = c(2010, 2015, 2018),
  grad = c("Sarajevo", "Visoko", "Zenica")
)

trenutna_godina <- as.integer(format(Sys.Date(), "%Y"))
df$broj_godina <- trenutna_godina - df$godiste


print(df)
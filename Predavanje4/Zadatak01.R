min_max_norm = function(v){
  min_v = min(v, na.rm= TRUE)
  max_v = max(v, na.rm=TRUE)
  razliku = max_v - min_v
  novi_v = v-min_v
  novi_v = novi_v / razliku
  return(novi_v)
}

max_abs_norm = function(v){
  max_v = abs(max(v, na.rm = TRUE))
  return((v/max_v))
}

library(outliers)
grubbs.test(df$Fare)

df <- df[df$Fare < 512, ]

grubbs.test(df$Age)


z_scores <- scale(df$Fare)
outliers <- df$Fare[abs(z_scores) >3]


metrike = function(p, a){
  # p=prediction
  # a= actual
  
  tp = 0
  tn = 0
  fp = 0
  fn = 0
  
  for (i in 1:length(p)){
    if((p[i] == a[i]) & (p[i] == 0)){
      tn <- tn+1
    } else if ((p[i] == a[i]) & (p[i] == 1)){
      tp <- tp+1
    } else if ((p[i] != a[i]) & (p[i] == 1)){
      fp <- fp+1
    } else {
      fn <- fn+1
    } 
  }
  
  acc <- (tp+tn) / (length(a))
  prec <- tp / (tp+fp)
  recall <- tp / (tp+fn)
  f1 <- (2*prec*recall) / (prec+recall)
  
  return(list(
    accuracy = acc,
    precision = prec,
    recall = recall,
    f1_score = f1
  ))
}
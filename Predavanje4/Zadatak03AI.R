eval_metrics <- function(y_true, y_pred) {
  if (length(y_true) != length(y_pred)) {
    stop("Vektori y_true i y_pred moraju biti iste dužine.")
  }
  
  if (!all(y_true %in% c(0,1)) || !all(y_pred %in% c(0,1))) {
    stop("Vektori moraju sadržati samo binarne vrednosti: 0 i 1.")
  }
  
  TP <- sum(y_true == 1 & y_pred == 1)
  TN <- sum(y_true == 0 & y_pred == 0)
  FP <- sum(y_true == 0 & y_pred == 1)
  FN <- sum(y_true == 1 & y_pred == 0)
  
  accuracy  <- (TP + TN) / (TP + TN + FP + FN)
  precision <- if ((TP + FP) == 0) 0 else TP / (TP + FP)
  recall    <- if ((TP + FN) == 0) 0 else TP / (TP + FN)
  f1_score  <- if ((precision + recall) == 0) 0 else 2 * precision * recall / (precision + recall)
  
  return(list(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score
  ))
}


y_true <- c(1, 0, 1, 1, 0, 1, 0, 0)
y_pred <- c(1, 0, 1, 0, 0, 1, 1, 0)

rezultat <- eval_metrics(y_true, y_pred)
print(rezultat)


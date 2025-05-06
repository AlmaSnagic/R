get_mode <- function(df, col_name)
MojDF <- r6Class(
  "MojDF",
  public = list(
    initialize = function(csv_path){
      df <- read.csv(csv_path)
      return(df)
    },
    print_size = function((df){
      print(sprintf("Broj kolona %s",))
    },
    count_na = function(df, col_name){
      return(sum(is.na(df[col_name])))
    },
    fill_na = function(df, col_name, na_val){
      if (self$count_na(df, co_namel)>0){
        df[is.na(df[col_name], col_name] <- na-val
      }
      return df
    )
df <- df %>%
  group_by(Sex) %>% 
  mutate(Age = ifelse(
  is.na(Age), mean(Age, na.rm=TRUE), Age
)) %>%
  ungroup()


df <- df %>% mutate(
  Cabin_rank = case_when(
    str_sub(Cabin, 1, 1) == "A" ~ 1,
    str_sub(Cabin, 1, 1) == "B" ~ 2,
    str_sub(Cabin, 1, 1) == "C" ~ 3,
    str_sub(Cabin, 1, 1) == "D" ~ 4,
    str_sub(Cabin, 1, 1) == "E" ~ 5,
    str_sub(Cabin, 1, 1) == "F" ~ 6,
    str_sub(Cabin, 1, 1) == "G" ~ 7,
    TRUE ~ 10
    
  )
)

df <- df %>% select(-Cabin)

df <- df %>% mutate(
  Sex = case_when(
    Sex == 'male' ~ 0,
    TRUE ~ 1
  )
)

df <- df %>% mutate(
  Embarked = ifelse(
    is.na(Embarked) | Embarked =="", "Q", Embarked
  ))

df <- dummy_cols(df, select_columns = "Embarked",
                  remove_selected_columns = TRUE)

prepare_for_regression <- function(data) {
  
  column_names <- data %>% colnames()
  character_column_filter <- function(x) { x == "character" }
  character_columns <- column_names[data %>% sapply(class) %>% sapply(character_column_filter)]
  character_column_count <- character_columns %>% length()
  number_column_filter <- function(x) { x == "numeric" || x == "integer" }
  number_columns <- column_names[data %>% sapply(class) %>% sapply(number_column_filter)]
  number_column_count <- number_columns %>% length()
  
  adjusted_data <- data
  
  if(character_column_count > 0)
    for(i in 1:character_column_count) {
      adjusted_data[[character_columns[i]]] <-
        adjusted_data[[character_columns[i]]] %>%
        factor()
    }
  
  n <- data %>% nrow()
  if(number_column_count > 0)
    for(i in 1:number_column_count) {
      v <-
        adjusted_data[[number_columns[i]]] %>%
        unique() %>%
        length()
      if(v > 1) {
        if(n >= 8) {
          adjusted_data[[number_columns[i]]] <-
            adjusted_data[[number_columns[i]]] %>%
            apply_bcskew0()
        }
        if(n >= 2) {
          adjusted_data[[number_columns[i]]] <-
            adjusted_data[[number_columns[i]]] %>%
            scale() %>%
            .[,1]
      }}}
  
  adjusted_data
}
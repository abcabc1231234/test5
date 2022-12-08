# Function to clean column names, remove empty rows and/or columns, remove column called "comments" and columns starting with "delta" 

cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

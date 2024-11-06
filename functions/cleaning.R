## ---------------------------
##
## Script name: Cleaning.r
##
## Purpose of script: 
##      # A file of functions for cleaning the Palmer Penguins dataset
##
## Author: Dr. Lydia France
##
## Date Created: 2024-10-01 
##
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------



# A function to make sure the column names are cleaned up, 
# eg lower case and snake case
clean_column_names <- function(penguins_data) {
    penguins_data %>%
        clean_names()
}

# A function to remove columns based on a vector of column names
remove_columns <- function(penguins_data, column_names) {
    penguins_data %>%
        select(-starts_with(column_names))
}

# A function to make sure the species names are shortened
shorten_species <- function(penguins_data) {
    penguins_data %>%
        mutate(species = case_when(
            species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
            species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
            species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
        ))
}

# A function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_data) {
    penguins_data %>%
        remove_empty(c("rows", "cols"))
}


# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
    penguins_data %>%
        na.omit()
}



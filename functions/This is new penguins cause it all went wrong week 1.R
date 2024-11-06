library(tidyverse)
library(palmerpenguins)
library(janitor)
library(here)
source(here("functions", "cleaning.R"))
penguins_raw <- read.csv(here("data","penguins_raw.csv"))
cleaning_penguin_columns <- function (raw_data){
     print("tell me more xxx (should say what this is doing)")
       raw_data %>% 
         clean_names() %>% 
         remove_empty(c("rows","cols")) %>% 
         select(-comments) %>% 
         select(-starts_with("delta"))
}
penguins_clean<-cleaning_penguin_columns(penguins_raw)
view(penguins_clean)
view(penguins_raw)
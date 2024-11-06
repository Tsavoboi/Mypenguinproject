#Keeping data safe, sane, and workable. We are trying to get our code to run on other peoples computers. 
#Need good code. What is reproducible research. Results and evidence strengthened by reproducibility. 
#Current reproducibility crisis. Fake data from Alzheimer lost. People even struggle their own work. 
#Combat this directly. We want things to be replicable, robust and reproducible. Need code people can plug in. 
#Make sure we can understand what we've done. Also, need version control, formalised way of keeping track. 
#Computers allow a more open research world. 
#Means we want things to be transparent and available. Often becoming mandatory to release code for publishing. 
#Prevent mistakes. 

#Today we use penguin data. 
#Set up a project, then a script, then add the data with the new folder thing. 
#Installing libraries/packages. Do not start code with install.packages() in the script, use it in the terminal part. 
#Load libraryise with Library
library(tidyverse)
library(palmerpenguins)
library(janitor)
library(here)
#do not use setwd(), first step is seeing where the code is. Setwd() is very specific to our computer, so it introduces bugs and all. 
#Instead use here.This tells me where I am. 
here() 
#palmer penguins has data
head(penguins_raw) #This has nightmare columns
colnames(penguins_raw)
#Tempting at this point to open excel and change all the column names manually. Don't, this is unreproducible. Do these steps in the script. 
#First create a safe copy of our data with a csv file. Here tells us where the file is going and what its name is.  
write.csv (penguins_raw, here("data","penguins_raw.csv"))
#Bad code time. 
#We can remove columns. This is bad code. As it will crash as overwrites itself
penguins_raw <- select(penguins_raw,-Comments) #removes colname Comments, - removes.
colnames(penguins_raw)
penguins_raw <- select(penguins_raw,-starts_with("Delta")) 
colnames(penguins_raw)
#can get rid of the two delta columns in one go with starts with
#Can load data again. 
penguins_raw <-read.csv(here("data","penguins_raw.csv")) #Gets everything back again, and safe. 
colnames(penguins_raw)
#better code but still bad, as it is still overwriting itself. 
penguins_clean<- select(penguins_raw, -Comments)
penguins_clean<- select(penguins_clean, -starts_with("Delta"))
#There's a much better way of doing this, using piping. This prevents the data overwriting itself. 
#This is needed, do ctrl, shift M. %>% And uses tidyverse. 
penguins_clean <- penguins_raw %>% 
              select(-Comments) %>% 
              select(-starts_with("Delta")) %>% 
              clean_names()# this is in janitor, and removes capitals, full stops, brackets, makes it very much better, no need for manual things
colnames (penguins_clean)
#this has made our column names computer readable. Not necessarily human readable. Clearly explain the variable name. 
#piping uses %>% as a and then clause. Coding is necessary.  
#Writing functions, to make reusable code. We use them all the time, and can produce them. 
#don't paste code, make a function. E.g.
penguins_cleaning_columns <- function(raw_data){raw_data %>% 
                                                  clean_names() %>% 
                                                   remove_empty(c("rows","cols")) %>% 
                                                    select(-starts_with("delta")) %>% 
                                                     select(-comments) %>% 
                                                                   na.omit() %>% 
                                                                     mutate(species = case_when( species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
                                                                     species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
                                                                     species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
                                                                   ))}
#clean names means capitals are gone. Remove empty removes the row and columns without date.
#running the function. This is cleaning code, a bit transparant so add a print function.(Didn't work) Also added a lot of things from cleaning.R
penguins_raw <-read.csv(here("data","penguins_raw.csv"))
penguins_clean<-penguins_cleaning_columns(penguins_raw)
print("removed empty columns and cleaned column names and removed comments and delta columns") 
#now we'd like to save the clean code. 
write.csv(penguins_clean, here("data","penguins_clean.csv"))
#we can take the cleaning function and move it to a separate script. Like the tools we're making. 
#Make a brand new subfolder
#dir.create(here("functions"))
#file.create(here("functions","cleaning.R")) Both used to place the cleaning code into a place, then used source to pull into scrpit. 
source(here("functions","cleaning.R"))
view(penguins_clean)
#RENV is a package that initilises and 
#load package renv
#renv::init()
#renv:: snapshot() 
#renv::restore() uses the renv folder to install all the right libraries
#renv::restore()
#renv::snapshot() saves all the packages that are added to the renv file. 
#Checking renv :: diagnostics () Anything with double colons shouldn't be in code but in console? 
#Renv keeps track of what we need, and the versions they are. 
#get an RENV folder, and a renv.lock (this it the file we send to ther people with the libraries they need, and the versions they are)
#Figures. 
source(here("functions", "cleaning.R"))
penguins_clean <- read_csv(here("data","penguins_clean.csv"))
#here we make a boxplot in my own personal hell. 
#The read.Csv step is useful for anominity 
#
Species_colours <- c("Adelie"= "darkorange","Chinstrap" = "purple", "Gentoo"= "cyan4")

Flipper_boxplot <- ggplot( 
  data = penguins_clean, 
  aes(x = species, y = flipper_length_mm)) + geom_boxplot((aes(color=species)), show.legend=FALSE)+ geom_jitter(alpha = 0.3, show.legend=FALSE, position = position_jitter (width = 0.2, seed = 2)) +scale_color_manual(values=Species_colours) + labs(x="species :P",y="Flipper Length (mm)")
Flipper_boxplot
#making a function for boxplotting
#never finished plot_boxplot <- function (data,x_column,y_column,x_label,y_label, colour_mapping){no_nan_data <- data %>%  drop_na}
#Jitter gives them random x values so they can be seen, we dont want that. Can unrandom it. 
#can also change transparency via alpha variable. 
#apparently we have N/A values in the boxplots, so we have to remove some of the NA values in our data. First we should subset the columns first. 
#penguins_flippers <- select(penguins_clean, c("species","flipper_length_mm"))
#this has taken the two columns we are interested in for the boxplot, the c says take both. 
#penguins_flippers <- drop_na(penguins_flippers) #bad code as overwriting things, as if you run this out of order things go wrong. 
#fix the above code by nesting things like drop_na(select)
penguins_flippers <- penguins_clean %>% 
  select(c("species","flipper_length_mm")) %>% 
  drop_na()
#this is how you pipe it to make it better. 
#we are adding something new? I am sad...
#We are loading more libaries in the consol. And now we're changing the size and all.
#git remote add origin git@github.com:Tsavoboi/Mypenguinproject.git this adds it to my git project 

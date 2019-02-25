# finding crossover baby names i.e., those names that exist for both genders

# Q1.  What is the highest frequency of crossover names?
# Q2.  Are crossover names greatest from female to male, or visa versa?
# Q3.  In which state do they (their parents) live?


# A talk at Trenton R User group meeting
# Dec 2018


#########   DOWNLOAD THE DATA

# source (by state)
# https://www.ssa.gov/oact/babynames/state/namesbystate.zip

library(tidyverse)

# download source data
# download.file("https://www.ssa.gov/oact/babynames/state/namesbystate.zip", "babynames_by_state.zip")
# unzip("babynames_by_state.zip")




#########   READ THE SOURCE DATA FILES


source_dir <- "C:/Users/MY_USER_NAME/Documents/R/DATA_WRANGLING TALK AT TRU/BABY NAMES/DATA_FILES"

fils <- list.files(pattern = ".TXT", path = source_dir)
fils

fils <- list.files(pattern = ".TXT", path = source_dir, full.names = TRUE)
fils

# using base R 
read.csv(fils[1]) 
read.csv(fils[1]) %>% str()
#   Problem #1:  no header row
#   Problem #2:  wrong data type
#   Problem #3:  to many rows shown, scrolls off screen
#   Problem #4:  no info on data types

# using the tidyverse
read_csv(fils[1])

# add header and specify the data type for each column
read_csv(fils[1], 
         col_names = c("state", "gender", "year", "name", "freq"),
         col_types = "ccici")


# create a function for ONE file
read_and_add_headers <- function(filename){
  read_csv(filename, 
           col_names = c("state", "gender", "year", "name", "freq"),
           col_types = "ccici")  
}


# do it for ALL files
all <- 
  map_df(fils, read_and_add_headers) 

all
View(all)



#########  DATA WRANGLING OF DATA.FRAMES USING DPLYR

# filter()
all %>% 
  filter(name == "Alex")

all %>% 
  filter(year > 1980 & freq == 1000 & name == "Victor")


# select columns
all %>% 
  select(gender, year)


# create new calculated columns
all %>% 
  mutate(year10 = year * 10)

all %>% 
  transmute(year10 = year * 10)


# group and summarize   (yields fewer rows)
all %>% 
  group_by(name) %>% 
  summarise(total = sum(freq))


all %>% 
  group_by(year) %>% 
  summarise(total = sum(freq))

# remove rows with bad years
all <- 
  all %>% 
  filter(year >= 1910)


# sorting a data.table
all %>% 
  group_by(year) %>% 
  summarise(total = sum(freq)) %>% 
  arrange(total)          # <-- sorted by population



# getting unique values
all %>% 
  distinct(name)


# find crossover names i.e., names by BOTH boys and girls
all %>% 
  group_by(name, gender) %>%        # group by name and gender
  summarise(total = sum(freq)) %>%  # count them    
  mutate(nsex = n()) %>%            # how many ROWS WITHIN EACH GROUP
  filter(nsex > 1) %>%              # exclude names whose don't have both male and female
  arrange(desc(name)) %>%           # sort in reverse order
  View()                         



#  Pick "Alexis" then plot
all %>% 
  filter(name == "Alexis") %>% 
  group_by(year, gender) %>% 
  summarise(n = sum(freq)) %>% 
  ggplot(aes(year, n)) + geom_point(aes(color = gender)) 

#  Pick "Hillary" then plot
all %>% 
  filter(name == "Hillary") %>% 
  group_by(year) %>% 
  summarise(n = sum(freq)) %>% 
  ggplot(aes(year, n)) + geom_point() 



#  Trends in baby names over time
all %>% 
  filter(name %in% c("John", "Jonathon", "Johnson", "Johnnie", "Johnny", "Joe", NA)) %>% 
  group_by(name, year) %>% 
  summarise(count = sum(freq)) %>% 
  ggplot(aes(year, count)) + geom_line() + facet_wrap(~name)



#########      JOINS: MERGING DATA.FRAMES

# very common task when dealing with relational databases

# example:  1. There is a table of "types"
#           2. There is a table of "sub-types"
#     There is a one-to-many relationship:one type has many sub-types
#     Each table has a primary (identifier) ID:       ("PK_ID")
#     Each "downstream" table has a foreign key ID:   ("FK_ID")


# CREATE SOME EXAMPLE DATA
types <- data_frame(
  PK_ID = 1:6,
  type_name = c("veg", "fruit", "cars", "countries", "trees", "cities")
)
types

subtypes <- data_frame(
  PK_ID = 1:10,
  subtype_name = c("apples", "bananas", "oranges", "USA", "United Kingdon", "India", "Ford", "Toyota", "Lexus", NA),
  FK_ID = c(2L,2L,2L,4L,4L,4L,3L,3L,3L, NA)
)
subtypes


# NOW JOIN THEM TOGETHER
right_join(types, subtypes, by = c("PK_ID" = "FK_ID"))
left_join(subtypes, types, by = c("FK_ID" = "PK_ID"))



#########      RESHAPING DATA.FRAMES

mtcars

mtcars %>% 
  spread(key = cyl, value = sum(mpg))



all %>%
  group_by(name, gender) %>% 
  summarize(freq = sum(freq)) %>% 
  spread(key = gender, value = sum(freq))
# 
# all %>%  
#   spread(key = name, value = sum(freq))
# 
# all %>%  
#   spread(key = state, value = sum(freq))

# getting rid of year
all %>%  
  group_by(state, name) %>% 
  summarize(freq = sum(freq)) %>% 
  spread(key = state, value = sum(freq))

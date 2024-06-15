# Data processing for Triangulating iconicity

# Packages
list.of.packages <- c("tidyverse","readxl","writexl","ggthemes","gghalves","ggbeeswarm","viridis","lme4","VGAM","cowplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# useful functions
`%notin%` <- function(x,y) !(x %in% y) 
mean.na <- function(x) mean(x, na.rm = T)
sd.na <- function(x) sd(x, na.rm = T)


# Load data ---------------------------------------------------------------


#Here we load the ground truth version of the coded data and add the independently collected guessability scores from the 2016 Collabra and Language papers.

# get consensus coding data
d = read_excel("data/ideophones_coded.xlsx") |> arrange(filename)

# add guessability scores from the Collabra and Language studies.

d.scores = read_excel("data/ideophones_guessability.xlsx") |>
  dplyr::select(-category)
d <- left_join(d,d.scores,by=c("ideophone","language","study" = "paper"))

# add logodds (for when we run stats: it's more sensible to predict against logodds than raw proportion correct)
d <- d |>
  group_by(study) |>
  mutate(logodds = probitlink(score))

# add Z score to make scores more comparable in plots across studies
d <- d |>
  group_by(study) |>
  mutate(score_z = scale(score,center=T,scale=T))

# get ratings data
d.ratings <- read_xlsx("data/ideophones_rated_means.xlsx") |>
  dplyr::select(-category,-list,-item)
d <- left_join(d,d.ratings,by=c("filename","study","language")) |>
  mutate(filename = gsub("_org","",filename))

# add z score for ratings
d$rating_z <- scale(d$rating,center=T,scale=T)


# Write data --------------------------------------------------------------


# write this dataset as the fullest dataset
write.csv(d,file="data/ideophones_coded_guessed_rated.csv",fileEncoding = "UTF-8",row.names=FALSE)
write_xlsx(d,path="data/ideophones_coded_guessed_rated.xlsx")

# load and merge the full ratings data for some of the reporting in the paper
ideophones_filename <- d |> ungroup() |> select(ideophone,filename) |>
  mutate(filename = str_to_lower(filename))

d.ratings.full <- read_xlsx("data/ideophones_rated.xlsx") |>
  mutate(list = gsub(".*([A-Z])$", "\\1",item)) |>
  mutate(filename = gsub("^(.*)_.*$", "\\1", str_to_lower(item))) |>
  mutate(filename = gsub("Coll_","", filename)) |>
  select(-item) 

# match the data to ideophones for easier lookup
d.ratings.full <- left_join(d.ratings.full,
                            ideophones_filename,by="filename",copy=TRUE)

write.csv(d.ratings.full, file="data/ideophones_rated.csv",row.names = FALSE)

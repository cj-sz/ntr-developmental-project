library(plyr) 
library(readr)
library(stringr)
library(dplyr)
library(data.table)
library(readxl)

# Obtain and mark age bins for all applicable words 
AoA_measures <- read_xlsx("AoA_measures.xlsx", sheet = "Sheet2") # read in all AoA measures

AoA_measures <- AoA_measures[!duplicated(AoA_measures$WORD), ] # filter duplicate words (takes the first learned meaning)
AoA_measures <- AoA_measures[!is.na(AoA_measures$MEANING), ] # filter words with unavailable meanings

rowsupdate <- which(!is.na(AoA_measures[,5])) # filter: if a value exists for CDI or Morr, void AoAtestbased to control for floor effects
AoA_measures[rowsupdate,3] <- NA
rowsupdate2 <- which(AoA_measures[,6] <7)
AoA_measures[rowsupdate2,3] <- NA

numerical_data <- AoA_measures[ -c(1:2) ]; # get just numerical data
AGE <- rowMeans(numerical_data, na.rm=TRUE) # average age of acquisition for each word
age_bins <- data.frame(AGE, bin=cut(AGE,c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23), include.lowest=TRUE)) # get one year age bins

AoA_data <- cbind(AoA_measures$WORD, AoA_measures$MEANING, age_bins) # final AoA dataset
colnames(AoA_data) [1] <- "STRING"
colnames(AoA_data) [2] <- "MEANING"
AoA_data <- data.frame(AoA_data)

# forR_input is filtered and marked based on the age bins obtained and the words in them
forR_input <- read_csv("forR_input.csv", col_types = cols(.default = "c")) # CRITICAL: read all columns as characters, or "t"'s may be read as TRUE!
# This is the input that needs to be changed get each age bin individually 
vals <- c('(1,2]', '(2,3]', '(3,4]', '(4,5]', '(5,6]', '(6,7]', '(7,8]', '(8,9]', '(9,10]', '(10,11]', '(11,12]', '(12,13]', '(13,14]', '(14,15]', '(15,16]', '(16,17]') #add bin for every iteration of AoA
AoA <- filter(AoA_data, bin %in% vals)
forR_input <- merge(forR_input, AoA, by.x = 'STRING')
forR_input <- mutate_all(forR_input,.funs=tolower)
library(plyr) 
library(readr)
library(stringr)
library(dplyr)
library(data.table)
library(readxl)

###
# Functions
###

# Calcualtes and prepares age-of-acquisition data to be used for filtering
prepare_aoa_data <- function() {
    # read in all AoA measures
    AoA_measures <- read_xlsx("AoA_measures.xlsx", sheet = "Sheet2")

    # filter duplicate words (takes first) and filter words with unavailable meanings
    AoA_measures <- AoA_measures[!duplicated(AoA_measures$WORD), ]
    AoA_measures <- AoA_measures[!is.na(AoA_measures$MEANING), ]

    # filter: if a value exists for CDI or Morr, void AoAtestbased to control for floor effects
    rowsupdate <- which(!is.na(AoA_measures[, 5]))
    AoA_measures[rowsupdate, 3] <- NA
    rowsupdate2 <- which(AoA_measures[, 6] < 7)
    AoA_measures[rowsupdate2, 3] <- NA

    # obtain average age of acquisition for each word
    numerical_data <- AoA_measures[-c(1:2)]
    AGE <- rowMeans(numerical_data, na.rm = TRUE)

    # one-year age bins
    age_bins <- data.frame(AGE, bin = cut(AGE, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23), include.lowest = TRUE))
    AoA_data <- cbind(AoA_measures$WORD, AoA_measures$MEANING, age_bins) # final AoA dataset
    colnames(AoA_data)[1] <- "STRING"
    colnames(AoA_data)[2] <- "MEANING"
    return(data.frame(AoA_data))
}

###
# Global values used in main loop
###

# Age-of-Acquisition data used for filtering
AoA_data <- prepare_aoa_data()

forR_input <- read_csv("forR_input.csv",col_types = cols(.default = "c")) # CRITICAL: read all columns as characters, or "t"'s may be read as TRUE!
forR_input <- mutate_all(forR_input,.funs=tolower)

# This is the input that needs to be changed get each age bin individually
# Need to mkke this process iterative (reduce unnecessary computation/duplocate computation)
vals <- c("(1,2]", "(2,3]", "(3,4]", "(4,5]", "(5,6]", "(6,7]", "(7,8]", "(8,9]", "(9,10]", "(10,11]", "(11,12]", "(12,13]", "(13,14]", "(14,15]", "(15,16]", "(16,17]") # add bin for every iteration of AoA

# Used for naming; indicates lower bound of age bin (i.e. 1 means bin (1,2])
bin_index <- 1

# Data frames: kept track of here as these are accumulated across the age bins
# Word-initial data frame
wordinitial_df <- data.frame(P = character(), G = character(), totalPG = character(), stringsAsFactors = FALSE) # accumulated at each iteration

for (b in vals) {
    print(b)

    parent_folder <- paste("age_measures/bin", bin_index, "-", bin_index + 1, "/", sep="")

    # Filter to just the age bin interested in (highest bin) and all data from earlier bins
    AoA <- filter(AoA_data, bin == b)

    # bin_data (previously forR_input) is filtered and marked based on the current age bin and the words in it
    bin_data <- merge(forR_input, AoA, by.x = 'STRING')
    bin_data <- mutate_all(bin_data,.funs=tolower)
    # TODO remove this is used for debugging only, currently checking 
    write.csv(bin_data, "bin_data_check.csv")

    # TODO wrap this into a function
    # TODO needs fix 
    # Find indices where....
    # there are non-linear mappings for E to schwa, as in ABLE (the written order is L-E but the phonological order is E-L)...
    # there are non-linear mappings for E to ʌ (as in ONE, where O --> /w/, N--> /n/, and E --> ʌ but out of order)...
    # there are non-linear mappings for E to rhotic (as in THEATRE)
    # replace the blanks, which come from Excel, with an underscore, just as a placeholder...note that these need to go in the column to the left of the mappings in question
    # Uncommenting the below lines is not necessary

    # combined_condition <- bin_data == "ə+_" | bin_data == "ʌ+_" | bin_data == "ɚ+_"
    # indices <- which(combined_condition, arr.ind = TRUE)

    # # Replace blanks with underscores in the column to the left of the condition
    # for (i in 1:nrow(indices)) {
    #     bin_data[indices[i, 1], indices[i, 2] - 1] <- "_"
    # }

    ##############################
    ####WORD-INITIAL MEASURES#####
    ##############################
    # Empty list for the current bin data
    wordinitial_list <- list()

    ##word-initial for loop:
    for(i in 1:length(bin_data$STRING)){
        print("in")
        wordinitial_temp <- t(matrix(cbind(colnames(bin_data)[c(2:19)][!is.na(bin_data[i,c(2:19)])],bin_data[i,c(2:19)][!is.na(bin_data[i,c(2:19)])])[1:3,2]))
        wordinitial_list[[length(wordinitial_list)+1]] <- wordinitial_temp
        print("out")
    }
    print("out completely")
    # Accumulate to the word initial data frame for the age bin
    wordinitial_df <- rbind(do.call(rbind, wordinitial_list), wordinitial_df)
    print("err before")
    wordinitial_df$G <-str_extract(wordinitial_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme
    print("err here?")

    # Now create all of the proportion tables and store these in the parent folder
    wordinitial_PG_prop <- prop.table(table(wordinitial_df$P,wordinitial_df$G),margin=1)
    print("or here")
    wordinitial_GP_prop <- t(prop.table(table(wordinitial_df$P,wordinitial_df$G),margin=2))
    print("or potentialy here")
    wordinitial_FREQ <- t(table(wordinitial_df$P,wordinitial_df$G))
    print("final check")

    write.csv(wordinitial_PG_prop, file=paste(parent_folder, "wordinitial_PG.csv", sep=""))
    write.csv(wordinitial_GP_prop, file=paste(parent_folder, "wordinitial_GP.csv", sep=""))
    write.csv(wordinitial_FREQ_prop, file=paste(parent_folder, "wordinitial_FREQ.csv", sep=""))

    bin_index <- bin_index + 1
}

# NOTES
# accumulated lists and data frames with every loop
# update forR input each loop (used) to contain only for next bin; append/concat/what is done to current data
# output all each iter 
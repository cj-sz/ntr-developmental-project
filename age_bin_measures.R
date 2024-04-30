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

# Computes wordinitial measures and outputs the results to csvs based on the bin.
# Returns the accumulated wordinitial_acc to be stored.
# Takes in information for the current bin as bin_data, along with accumulated data from all previous bins, and the lower bin index.
compute_wordinitial_measures <- function(bin_data, wordinitial_df_acc, bin_index) {
    # Empty list for the current bin data
    wordinitial_list <- list()
    wordinitial_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

    ## Word-initial for loop:
    for(i in 1:length(bin_data$STRING)) {
        wordinitial_temp <- t(matrix(
            cbind(
                colnames(bin_data)[c(2:19)][!is.na(bin_data[i, c(2:19)])],
                bin_data[i, c(2:19)][!is.na(bin_data[i, c(2:19)])]
            )[1:3, 2]
        ))
        wordinitial_list[[length(wordinitial_list) + 1]] <- wordinitial_temp
    }

    # Convert these contents to a data frame with the same columns as the accumulator
    tdf <- do.call(rbind,wordinitial_list)
    colnames(tdf) <- tdf[1,]
    tdf <- as.data.frame(tdf[-1,])

    # Add these contents to the accumulator for the age bins
    wordinitial_df_acc <- rbind(wordinitial_df_acc, tdf)

    # Copy the accumulated data
    wordinitial_df <- wordinitial_df_acc
    wordinitial_df$G <-str_extract(wordinitial_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme

    # Compute proability tables
    wordinitial_PG_prop <- prop.table(table(wordinitial_df$P,wordinitial_df$G),margin=1)

    ##word initial G->P proportion table (note: will have many zero cells, but that can be removed later for ease of display)
    wordinitial_GP_prop <- t(prop.table(table(wordinitial_df$P,wordinitial_df$G),margin=2))

    ##word initial FREQ table (note: will have many zero cells, but that can be removed later for ease of display)
    wordinitial_FREQ <- t(table(wordinitial_df$P,wordinitial_df$G))

    # Output all of the word intial data to csv's
    sbin_identifier <- paste0("tables/bin", bin_index, "_", bin_index + 1)
    sbin_identifier_csv <- paste0(bin_index, "_", bin_index + 1, ".csv")

    if (!dir.exists(sbin_identifier)) {
        dir.create(sbin_identifier, recursive=TRUE)
    }

    write.csv(wordinitial_PG_prop, paste0(sbin_identifier, "/wordinitial_PG_prop", sbin_identifier_csv))
    write.csv(wordinitial_GP_prop, paste0(sbin_identifier, "/wordinitial_GP_prop", sbin_identifier_csv))
    write.csv(wordinitial_FREQ, paste0(sbin_identifier, "/wordinitial_FREQ", sbin_identifier_csv))

    return(wordinitial_df_acc)
}

# Computes syllable initial measures and outputs the results to csvs based on the bin.
# Returns the accumulated syllableinitial_acc to be stored.
# Takes in information for the current bin as bin_data, along with accumulated data from all previous bins, and the lower bin index.
compute_syllable_initial_measures <- function(bin_data, syllableinitial_df_acc, bin_index) {
    # Empty list for current bin data
    syllableinitial_list <- list()
    syllableinitial_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

    # TODO its possible this loop can be sped up further
    ##syllable-initial for loop for the SECOND through SIXTH syllables only!:
    ##special note: must exclude WORD-FINAL mappings, they will be treated as word final even if they are also syllable-initial (e.g, the Y in joe-Y)

    #create a new corpus where you turn into NA the final mappings from each word so they'll end up being excluded from the syllable-initial lists:
    newcorpus <- bin_data

    for( i in 1:nrow(newcorpus)){
        mycols <-max.col(!is.na(newcorpus[i,]),ties.method="last")  #figure out which columns will be the final mappings
        newcorpus[i,c((mycols-2):mycols)] <- "NA"
    }

    for(i in 1:length(newcorpus$STRING)){
        #second syllable
        syllableinitial_temp <- t(matrix(tryCatch(
                                        cbind(colnames(newcorpus)[c(20:37)][!is.na(newcorpus[i,c(20:37)])],newcorpus[i,c(20:37)][!is.na(newcorpus[i,c(20:37)])])[1:3,2],
                                        error=function(err) NA)))
        syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
        
        #third syllable
        syllableinitial_temp <- t(matrix(tryCatch(
                                        cbind(colnames(newcorpus)[c(38:52)][!is.na(newcorpus[i,c(38:52)])],newcorpus[i,c(38:52)][!is.na(newcorpus[i,c(38:52)])])[1:3,2],
                                        error=function(err) NA)))
        syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
        
        #fourth syllable
        syllableinitial_temp <- t(matrix(tryCatch(
                                        cbind(colnames(newcorpus)[c(53:64)][!is.na(newcorpus[i,c(53:64)])],newcorpus[i,c(53:64)][!is.na(newcorpus[i,c(53:64)])])[1:3,2],
                                        error=function(err) NA)))
        syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
        
        #fifth syllable
        syllableinitial_temp <- t(matrix(tryCatch(
                                        cbind(colnames(newcorpus)[c(65:76)][!is.na(newcorpus[i,c(65:76)])],newcorpus[i,c(65:76)][!is.na(newcorpus[i,c(65:76)])])[1:3,2],
                                        error=function(err) NA)))
        syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
        
        #sixth syllable
        syllableinitial_temp <- t(matrix(tryCatch(
                                        cbind(colnames(newcorpus)[c(77:88)][!is.na(newcorpus[i,c(77:88)])],newcorpus[i,c(77:88)][!is.na(newcorpus[i,c(77:88)])])[1:3,2],
                                        error=function(err) NA)))
        syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
    }

    #replace empty matrices with NAs of 1x3 so that they can be bound with the non-empty matrices
    for(i in 1:length(syllableinitial_list)){
        ifelse(all(is.na(syllableinitial_list[[i]])),syllableinitial_list[[i]] <- matrix(NA,nrow=1,ncol=3),syllableinitial_list[[i]]<-syllableinitial_list[[i]])
    }

    tdf <- do.call(rbind, syllableinitial_list)
    colnames(tdf) <- tdf[1,]
    tdf <- as.data.frame(tdf[-1,])

    # Add these contents to the accumulator for the age bins
    syllableinitial_df_acc <- rbind(syllableinitial_df_acc, tdf)

    # Create a copy
    syllableinitial_df <- syllable_initial_df_acc
    syllableinitial_df$G <-str_extract(syllableinitial_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme

    # Create all probabilty tables and output them

    ##syllable initial P->G proportion table (note: will have many zero cells, but that can be removed later for ease of display)
    syllableinitial_PG_prop <- prop.table(table(syllableinitial_df$P,syllableinitial_df$G),margin=1)

    ##syllable initial G->P proportion table (note: will have many zero cells, but that can be removed later for ease of display
    syllableinitial_GP_prop <- t(prop.table(table(syllableinitial_df$P,syllableinitial_df$G),margin=2))

    ##syllable initial FREQ table (note: will have many zero cells, but that can be removed later for ease of display)
    syllableinitial_FREQ <- t(table(syllableinitial_df$P,syllableinitial_df$G))

    # Output all of the word intial data to csv's
    sbin_identifier <- paste0("tables/bin", bin_index, "_", bin_index + 1)
    sbin_identifier_csv <- paste0(bin_index, "_", bin_index + 1, ".csv")

    if (!dir.exists(sbin_identifier)) {
        dir.create(sbin_identifier, recursive=TRUE)
    }

    write.csv(syllableinitial_PG_prop, paste0(sbin_identifier, "/syllableinitial_PG_prop", sbin_identifier_csv))
    write.csv(syllableinitial_GP_prop, paste0(sbin_identifier, "/syllableinitial_GP_prop", sbin_identifier_csv))
    write.csv(syllableinitial_FREQ, paste0(sbin_identifier, "/syllableinitial_FREQ", sbin_identifier_csv))

    # Ultimately return the accumulor with the new data
    return(syllableinitial_df_acc)
}

# Computes syllable and word final measures and outputs the results to csvs based on the bin.
# Returns the accumulated wordfinal and syllablefinal accumulators to be stored.
# Takes in information for the current bin as bin_data, along with accumulated data for syllable and word from all previous bins, and the lower bin index.
compute_final_measures <- function(bin_data, syllablefinal_df_acc, wordfinal_df_acc, bin_index) {
    # Initialize empty lists for this bin
    wordfinal_list <- list()
    wordfinal_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

    syllablefinal_list <- list()
    syllablefinal_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

    ##syllable-final for loop, begins with 1st syllable, appending mappings...then 2nd syllable, etc. before going on to the ith+1 word
    for(i in 1:length(bin_data$STRING)) { 
        #first syllable
        syllable_length1 <- length(cbind(colnames(bin_data)[c(2:19)][!is.na(bin_data[i,c(2:19)])],
                                        bin_data[i,c(2:19)][!is.na(bin_data[i,c(2:19)])])[-c(1:3),2])
        #second syllable
        syllable_length2 <- length(cbind(colnames(bin_data)[c(20:37)][!is.na(bin_data[i,c(20:37)])],
                                        bin_data[i,c(20:37)][!is.na(bin_data[i,c(20:37)])])[,2])
        #third syllable
        syllable_length3 <- length(cbind(colnames(bin_data)[c(38:52)][!is.na(bin_data[i,c(38:52)])],
                                        bin_data[i,c(38:52)][!is.na(bin_data[i,c(38:52)])])[,2])
        #fourth syllable
        syllable_length4 <- length(cbind(colnames(bin_data)[c(53:64)][!is.na(bin_data[i,c(53:64)])],
                                        bin_data[i,c(53:64)][!is.na(bin_data[i,c(53:64)])])[,2])
        #fifth syllable
        syllable_length5 <- length(cbind(colnames(bin_data)[c(65:76)][!is.na(bin_data[i,c(65:76)])],
                                        bin_data[i,c(65:76)][!is.na(bin_data[i,c(65:76)])])[,2])
        #sixth syllable
        syllable_length6 <- length(cbind(colnames(bin_data)[c(77:88)][!is.na(bin_data[i,c(77:88)])],
                                        bin_data[i,c(77:88)][!is.na(bin_data[i,c(77:88)])])[,2])

        holdword<-rbind(
        cbind(colnames(bin_data)[c(2:19)][!is.na(bin_data[i,c(2:19)])],
                                                                    bin_data[i,c(2:19)][!is.na(bin_data[i,c(2:19)])])[-c(1:3),2][(syllable_length1-2):syllable_length1],
        cbind(colnames(bin_data)[c(20:37)][!is.na(bin_data[i,c(20:37)])],
                                                                bin_data[i,c(20:37)][!is.na(bin_data[i,c(20:37)])])[,2][(syllable_length2-2):syllable_length2],
        cbind(colnames(bin_data)[c(38:52)][!is.na(bin_data[i,c(38:52)])],
                                                                bin_data[i,c(38:52)][!is.na(bin_data[i,c(38:52)])])[,2][(syllable_length3-2):syllable_length3],
        cbind(colnames(bin_data)[c(53:64)][!is.na(bin_data[i,c(53:64)])],
                                                                bin_data[i,c(53:64)][!is.na(bin_data[i,c(53:64)])])[,2][(syllable_length4-2):syllable_length4],
        cbind(colnames(bin_data)[c(65:76)][!is.na(bin_data[i,c(65:76)])],
                                                                bin_data[i,c(65:76)][!is.na(bin_data[i,c(65:76)])])[,2][(syllable_length5-2):syllable_length5],
        cbind(colnames(bin_data)[c(77:88)][!is.na(bin_data[i,c(77:88)])],
                                                                bin_data[i,c(77:88)][!is.na(bin_data[i,c(77:88)])])[,2][(syllable_length6-2):syllable_length6]
        )

        syllablefinal_list[[length(syllablefinal_list)+1]] <-   as.data.frame(head(holdword,-1)) #pull out non-word-final
        wordfinal_list[[length(wordfinal_list)+1]] <-   as.data.frame(tail(holdword,1)) #pull out just the word-final                                                 
    }

    # Syllablefinal: add to the accumulators
    stdf <- do.call(rbind, syllablefinal_list)
    colnames(stdf) <- stdf[1,]
    stdf <- as.data.frame(stdf[-1,])
    syllablefinal_df_acc <- rbind(syllablefinal_df_acc, stdf)

    # Wordfinal: add to the accumulators
    wtdf <- do.call(rbind,wordfinal_list)
    colnames(wtdf) <- wtdf[1,]
    wtdf <- as.data.frame(wtdf[-1,])
    wordfinal_df_acc <- rbind(wordfinal_df_acc, wtdf)

    # Compute probabilities
    syllablefinal_df <- syllablefinal_df_acc
    syllablefinal_df$G <-str_extract(syllablefinal_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme
    wordfinal_df <- wordfinal_df_acc
    wordfinal_df$G <-str_extract(wordfinal_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme

    ##syllable final P->G proportion table (note: will have many zero cells, but that can be removed later for ease of display)
    syllablefinal_PG_prop <- prop.table(table(syllablefinal_df$P,syllablefinal_df$G),margin=1)
    ##syllable final G->P proportion table (note: will have many zero cells, but that can be removed later for ease of display
    syllablefinal_GP_prop <- t(prop.table(table(syllablefinal_df$P,syllablefinal_df$G),margin=2))
    ##syllable final FREQ table (note: will have many zero cells, but that can be removed later for ease of display)
    syllablefinal_FREQ <- t(table(syllablefinal_df$P,syllablefinal_df$G))

    ##word final P->G proportion table (note: will have many zero cells, but that can be removed later for ease of display)
    wordfinal_PG_prop <- prop.table(table(wordfinal_df$P,wordfinal_df$G),margin=1)

    ##word final G->P proportion table (note: will have many zero cells, but that can be removed later for ease of display
    wordfinal_GP_prop <- t(prop.table(table(wordfinal_df$P,wordfinal_df$G),margin=2))

    ##word final FREQ table (note: will have many zero cells, but that can be removed later for ease of display)
    wordfinal_FREQ <- t(table(wordfinal_df$P,wordfinal_df$G))

    # Output to csvs
    sbin_identifier <- paste0("tables/bin", bin_index, "_", bin_index + 1)
    sbin_identifier_csv <- paste0(bin_index, "_", bin_index + 1, ".csv")

    if (!dir.exists(sbin_identifier)) {
        dir.create(sbin_identifier, recursive=TRUE)
    }

    write.csv(syllablefinal_PG_prop, paste0(sbin_identifier, "/syllablefinal_PG_prop", sbin_identifier_csv))
    write.csv(syllablefinal_GP_prop, paste0(sbin_identifier, "/syllablefinal_GP_prop", sbin_identifier_csv))
    write.csv(syllablefinal_FREQ, paste0(sbin_identifier, "/syllablefinal_FREQ", sbin_identifier_csv))
    write.csv(wordfinal_PG_prop, paste0(sbin_identifier, "/wordfinal_PG_prop", sbin_identifier_csv))
    write.csv(wordfinal_GP_prop, paste0(sbin_identifier, "/wordfinal_GP_prop", sbin_identifier_csv))
    write.csv(wordfinal_FREQ, paste0(sbin_identifier, "/wordfinal_FREQ", sbin_identifier_csv))

    return(list(syllablefinal_df_acc = syllablefinal_df_acc, wordfinal_df_acc = wordfinal_df_acc))
}

###################################
# Global values used in main loop #
###################################

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
wordinitial_df_acc <- data.frame(P = character(), G = character(), totalPG = character(), stringsAsFactors = FALSE)
# Syllable initial data frame
syllableinitial_df_acc <- data.frame(P = character(), G = character(), totalPG = character(), stringsAsFactors = FALSE)
# Syllable final data frame
syllablefinal_df_acc <- data.frame(P = character(), G = character(), totalPG = character(), stringsAsFactors = FALSE)
# Word final data frame
wordfinal_df_acc <- data.frame(P = character(), G = character(), totalPG = character(), stringsAsFactors = FALSE)



for (b in vals) {
    print(b)
    val <- b

    parent_folder <- paste("age_measures/bin", bin_index, "-", bin_index + 1, "/", sep="")

    # Filter to just the age bin interested in (highest bin) and all data from earlier bins
    AoA <- filter(AoA_data, bin %in% val)

    # bin_data (previously forR_input) is filtered and marked based on the current age bin and the words in it
    bin_data <- merge(forR_input, AoA, by.x = 'STRING')
    bin_data <- mutate_all(bin_data,.funs=tolower)

    # TODO wrap this into a function
    # TODO remove?
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

    # Compute word initial measures and return the accumulated result
    wordinitial_df_acc <- compute_wordinitial_measures(bin_data, wordinitial_df_acc, bin_index)

    # Compute syllable initial measures and return the result
    syllable_initial_df_acc <- compute_syllable_initial_measures(bin_data, syllableinitial_df_acc, bin_index)

    # Compute syllable and word final measures and return the results
    finals <- compute_final_measures(bin_data, syllablefinal_df_acc, wordfinal_df_acc, bin_index)
    syllablefinal_df_acc <- finals$syllablefinal_df_acc
    wordfinal_df_acc <- finals$wordfinal_df_acc

    ##################################
    ####SYLLABLE-INITIAL MEASURES#####
    ##################################

    # Empty list for current bin data
    syllableinitial_list <- list()
    syllableinitial_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

    ##syllable-initial for loop for the SECOND through SIXTH syllables only!:
    ##special note: must exclude WORD-FINAL mappings, they will be treated as word final even if they are also syllable-initial (e.g, the Y in joe-Y)

    #create a new corpus where you turn into NA the final mappings from each word so they'll end up being excluded from the syllable-initial lists:


    ###################################
    ####EXAMPLES FROM ORIGINAL CODE####
    ###################################

    # #example of pulling out one P->G mapping odds (just replace the "i" with the IPA symbol for the desired sound#
    # wordinitial_PG_prop[rownames(wordinitial_PG_prop)=="i",][wordinitial_PG_prop[rownames(wordinitial_PG_prop)=="i",]>0]
    # #example of pulling out one G->P mapping odds (just replace the "c" with the desired graphemes#
    # wordinitial_GP_prop[rownames(wordinitial_GP_prop)=="c",][wordinitial_GP_prop[rownames(wordinitial_GP_prop)=="c",]>0]
    # #example of pulling out one P->G mapping odds (just replace the "i" with the IPA symbol for the desired sound#
    # syllableinitial_PG_prop[rownames(syllableinitial_PG_prop)=="i",][syllableinitial_PG_prop[rownames(syllableinitial_PG_prop)=="i",]>0]
    # #example of pulling out one G->P mapping odds (just replace the "c" with the desired graphemes#
    # syllableinitial_GP_prop[rownames(syllableinitial_GP_prop)=="c",][syllableinitial_GP_prop[rownames(syllableinitial_GP_prop)=="c",]>0]
    # #example of pulling out one P->G mapping odds (just replace the "k" with the IPA symbol for the desired sound#
    # syllablefinal_PG_prop[rownames(syllablefinal_PG_prop)=="k",][syllablefinal_PG_prop[rownames(syllablefinal_PG_prop)=="k",]>0]
    # #example of pulling out one G->P mapping odds (just replace the "k" with the desired graphemes#
    # syllablefinal_GP_prop[rownames(syllablefinal_GP_prop)=="c",][syllablefinal_GP_prop[rownames(syllablefinal_GP_prop)=="c",]>0]
    # #example of pulling out one P->G mapping odds (just replace the "i" with the IPA symbol for the desired sound#
    # wordfinal_PG_prop[rownames(wordfinal_PG_prop)=="i",][wordfinal_PG_prop[rownames(wordfinal_PG_prop)=="i",]>0]
    # #example of pulling out one G->P mapping odds (just replace the "c" with the desired graphemes#
    # wordfinal_GP_prop[rownames(wordfinal_GP_prop)=="c",][wordfinal_GP_prop[rownames(wordfinal_GP_prop)=="c",]>0]
    
    # Increment index
    bin_index <- bin_index + 1
}
library(tm)
library(SnowballC)
library(caTools)
library(randomForest)

# Create a Document Term Matrix (DFM)
corpus_dtm <- function(complete_dataset) {
    cli_h3("Creating corpus")
    cli_text("Please wait, this could take several minutes.")
    cli_text("")

    # The VectorSource is the column of the dataset from which we want to
    # work with
    cli_text("Transforming data.")
    corpus <- VCorpus(VectorSource(complete_dataset$Text))

    # Remove stop words
    cli_text("Removing stop words.")
    stopwords <- as.character(
        read.csv(here('Settings/stop_words.csv'), head = FALSE)$V1)
    stopwords <- unique(c(stopwords, stopwords()))

    # Lowercase all the text data of out corpus
    cli_text("Transforming to lowercase.")
    corpus <- tm_map(corpus, content_transformer(tolower))

    # If needed: remove numbers
    cli_text("Removing numbers.")
    corpus <- tm_map(corpus, removeNumbers)

    # Remove punctuation
    cli_text("Removing punctuation marks.")
    corpus <- tm_map(corpus, removePunctuation)

    # Remove stop Words
    cli_text("Removing stop words.")
    corpus <- tm_map(corpus, removeWords, stopwords(kind = 'en'))
    corpus <- tm_map(corpus, removeWords, stopwords)

    # Conduct the stemming process: to reduce a word to its root.
    # e.g.: Reading -> read, playing -> play
    cli_text("Stemming document.")
    corpus <- tm_map(corpus, stemDocument)

    # Eliminate multiple white spaces
    cli_text("Removing extra white spaces.")
    corpus <- tm_map(corpus, stripWhitespace)

    # Create the 'Bag of Words' model -- Document Term Matrix (DTM)
    cli_text("Creating the Document Term Matrix (DTM).")
    dtm <- DocumentTermMatrix(corpus)
    cli_progress_done()
    return(dtm)
}

# Transform the data into a dataframe and codify the SDGs as factors
dataset_DF <- function(dtm_data, complete_dataset) {
    dataset <- as.data.frame(as.matrix(dtm_data))
    dataset$Target <- complete_dataset$Target

    return(dataset)
}

# Create the complete data set and codify SDG as factors
codify <- function(dtm_data, complete_dataset){
    # Create working data set
    data_set_to_work <- dataset_DF(dtm_data, complete_dataset)

    # Codify the variable to use as a factor
    data_set_to_work$Target <- factor(data_set_to_work$Target)

    # Review the levels/factors: SDG targets
    levels(data_set_to_work$SDG)

    return(data_set_to_work)
}


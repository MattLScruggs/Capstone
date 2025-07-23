# Sample text data
text <- "The H1N1 flu is not limited to humans and has been reported in herds in Canada, Norway, and sporadically in the United States, defining this virus as zoonosis, or rather humano-zoonotic. In this article, we describe some of the epidemiologic features of H1N1 and make some predictions regarding its second wave. Influenza viruses are members of the orthomyxovirus viruses that encode a segmented RNA genome. There are three groups of influenza virus: influenza A, influenza B, and influenza C (1, 2). Influenza B and C viruses are associated with low-level sporadic disease and limited outbreaks and are never causes of pandemic influenza (2). On the other hand, influenza A is responsible for most seasonal influenza and all known pandemics (2). Only influenza A is discussed here."

# Preprocess the text
text <- tolower(text)  # Convert to lowercase
text <- str_replace_all(text, "[[:punct:]]", "")  # Remove punctuation
words <- unlist(str_split(text, "\\s+"))  # Split into words

# Define n-gram size
n <- 2  # For bigrams

# Create n-grams
ngrams <- lapply(1:(length(words) - n + 1), function(i) {
  paste(words[i:(i + n - 1)], collapse = " ")
})

ngram_vector<-unlist(ngrams)

# Create a transition matrix
transition_matrix <- matrix(0, nrow = length(ngram_vector), ncol = length(ngram_vector))
rownames(transition_matrix) <- ngram_vector
colnames(transition_matrix) <- ngram_vector

# Populate the transition matrix
for (i in 1:(length(ngram_vector) - 1)) {
  current_ngram <- ngram_vector[i]
  next_ngram <- ngram_vector[i + 1]
  transition_matrix[current_ngram, next_ngram] <- transition_matrix[current_ngram, next_ngram] + 1
}

# Normalize the matrix to probabilities
transition_matrix <- transition_matrix / rowSums(transition_matrix, na.rm = TRUE)

# Generate text
generate_text <- function(start, n_words, transition_probs) {
  current <- start
  result <- current
  
  for (i in 1:(n_words - 1)) {
    next_word <- sample(colnames(transition_probs), 1, prob = transition_probs[current, ])
    result <- paste(result, next_word)
    current <- next_word
  }
  
  return(result)
}


# Example usage
start <- "influenza viruses"  # Starting bigram
generated_text <- generate_text(start, 10, transition_probs)
cat("Generated Text:", generated_text)





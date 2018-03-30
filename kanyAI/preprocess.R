# Keras LSTM text generation based on this example: 
# https://keras.rstudio.com/articles/examples/lstm_text_generation.html

## LOAD LIBRARIES -------------------------------------------------------------

library(keras)
library(readr)
library(stringr)
library(purrr)
library(tokenizers)
library(reticulate)


## FUNCTIONS -------------------------------------------------------------

path <- "./data/lyrics.txt"
maxlen <- 40

# Load, collapse, and tokenize text
text <- read_lines(path) %>%
  str_to_lower() %>%
  str_c(collapse = "\n") %>%
  tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE)

chars <- text %>%
  unique() %>%
  sort()

sample_mod <- function(preds, temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
}

generateText <- function(model, num_words = 400, diversity = 0.2) {
  start_index <- sample(1:(length(text) - maxlen), size = 1)
  sentence <- text[start_index:(start_index + maxlen - 1)]
  generated <- ""
  
  for(i in 1:num_words){
    
    x <- sapply(chars, function(x){
      as.integer(x == sentence)
    })
    x <- array_reshape(x, c(1, dim(x)))
    
    preds <- predict(model, x)
    next_index <- sample_mod(preds, diversity)
    next_char <- chars[next_index]
    
    generated <- str_c(generated, next_char, collapse = "")
    sentence <- c(sentence[-1], next_char)
    
  }
  
  generated
}

## LOAD MODEL -------------------------------------------------------------

# model <- reticulate::py_run_string("import keras; keras.models.load_model('./data/model.h5')")
model <- keras::load_model_hdf5("./data/model.h5")

X <- readRDS("./data/X.rds")
y <- readRDS("./data/y.rds")
  
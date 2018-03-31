get_next_text <- function( input_text, output_chars, diversity = 1){
  
  sample_mod <- function(preds, temperature = 1){
    preds <- log(preds)/temperature
    exp_preds <- exp(preds)
    preds <- exp_preds/sum(exp(preds))
    
    rmultinom(1, 1, preds) %>% 
      as.integer() %>%
      which.max()
  }
  sentence <- input_text
  
  #get the file path for the saves model file
  model <- load_model_hdf5('input/kanye')
  chars <- readRDS('input/kanye_chars.rds')
  
  #truncate of pad left to make 40 character
  if(str_length(sentence)>40){
    sentence <- str_sub(sentence, end = -40)
  }
  
  sentence <- str_pad(string = input_text, width = 40, side = "left") %>% 
    tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE)
  generated <- ""
  
  for(i in 1:output_chars){

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
  
  # prefix sentence seed if given
  generated <- paste0(input_text, generated)
  
  generated
}

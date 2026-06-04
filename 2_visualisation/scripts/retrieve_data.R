library(here)
library(base)
library(dplyr)
library(tidyverse)

# this function prepares a corpus data frame
make_df_from_one_file <-
  
  function(input_path, enc = 'utf-8'){
  
  # read txt a file
  data_lines <- readLines(input_path, encoding = enc)
  
  # convert lines to utf-8
  if(enc != 'utf-8'){
    data_lines <- iconv(
      data_lines, from = enc, to = 'utf-8', sub = ''
      )
  }
  
  # remove irregular lines
  data_lines <- data_lines[grepl('\t', data_lines)]
  data_lines <- data_lines[!grepl('(@|%|<|>)', data_lines)]

  # split strings by tabs
  split_lines <- strsplit(data_lines, split = '\t')
  split_lines <- lapply(split_lines, function(x){
    length(x) <- 4
    return(x)
  })
  
  # list -> data frame
  df_corp <- do.call(rbind.data.frame, split_lines)
  colnames(df_corp) <- c('doc_id', 'word', 'lemma', 'pos')
  rownames(df_corp) <- 1:(nrow(df_corp))
  
  # return a data frame
  return(df_corp)
}

make_df_from_one_dir <-
  
  function(input_dir, enc = 'utf-8'){
    
    # get file names
    paths <- list.files(
      path = input_dir,
      pattern = r'(.*.txt$)',
      full.names = TRUE)
    df_list <- vector('list', length(paths))
    
    # prepare data frame for each txt file
    for(i in 1:(length(paths))){
      cat('Processing: ', basename(paths[i]), '\n')
      df_list[[i]] <- make_df_from_one_file(paths[i], enc)
    }
    
    return(df_list)
  }

# this function binds corpus data frames
bind_multi_dfs <-
  
  function(df_list){
    
    # bind data frames
    cat('Binding data frames...\n')
    df_bound <- bind_rows(df_list)
    
    # add a new column: sent_id
    cat('Adding sent_id...\n')
    is_punct <- df_bound$word %in% c('.', '?', '!')
    df_bound$sent_id <- 
      cumsum(c(0, is_punct[-length(is_punct)])) + 1
    df_bound <- df_bound[, c(1, 5, 2, 3, 4)]
    
    return(df_bound)
  }

# retrieve corpus data from ICLE
df_icle_list <- make_df_from_one_dir(
  here('corp', 'icle_ja'), 'utf-8'
  )
df_icle <- bind_multi_dfs(df_icle_list)
write_tsv(df_icle, here('data', 'corp_icle_ja.tsv'))

# retrieve corpus data from COCA
df_coca_list <- make_df_from_one_dir(
  here('corp', 'coca_sample'), 'latin1'
  )
df_coca <- bind_multi_dfs(df_coca_list)
write_tsv(df_coca, here('data', 'corp_coca_sample.tsv'))
library(here)
library(tidyverse)
library(mclm)

# make freqlist for all words
make_df_freq_all <- function(tsv_path){
  
  df_tsv <- read_tsv(tsv_path)
  df_tsv_sel <- df_tsv[, 3:5]
  df_tsv_sel <- df_tsv_sel |>
    mutate(pos_simple = case_when(
      str_detect(pos, r'--[^(n[^u]|pn|pp).+]--') ~ 'n',
      str_detect(pos, r'--[^v[^m].+]--') ~ 'v',
      TRUE ~ 'x'
    ))
  
  text_full <- apply(df_tsv_sel, 1, 
                     function(row) paste(row, collapse = '\t'))
  text_as_string <- paste(text_full, collapse = '\n')
  
  RE_TOKEN_SPLITTER <- r'--[\n+]--'
  RE_TOKEN_PATTERN <- r'--[^([^\t\n]+)\t([^\t\n]+)\t([^\t\n]+)\t([^\t\n]+)$]--'
  RE_TOKEN_TRANSF_OUT <- r'--[\2/\4]--'
  
  # prepare freqlist for all words
  freq_all <- text_as_string |>
    freqlist(re_token_splitter = RE_TOKEN_SPLITTER,
             re_token_extractor = RE_TOKEN_PATTERN,
             re_token_transf_in = RE_TOKEN_PATTERN,
             token_transf_out = RE_TOKEN_TRANSF_OUT,
             as_text = TRUE
    )
  
  # put the result into the data frame
  df_freq_raw <- as_tibble(freq_all)
  
  # lemma/pos -> lemma, pos (different columns)
  df_freq_all <- df_freq_raw |> 
    separate(type, into = c('lemma', 'pos'),
             sep = r'--[/(?=[^/]+$)]--') # separate by the final slash
  
  # remove invalid lemmas
  df_freq_all <- df_freq_all |>
    filter(str_detect(lemma, r'--[^[A-Za-z]+$]--'))
  
  # remove the column 'rank'
  df_freq_all<- df_freq_all |>
    select(-rank)
  
  return(df_freq_all)
}

# make freqlist for a specific pos tag
make_df_freq_pos <- function(df_freq_all, pos_input, min_freq = 1){
  
  # select the raws for specific pos tags
  df_freq_raw <- df_freq_all |>
    filter(pos == pos_input)
  
  # aggregate according to lemma
  df_freq <- df_freq_raw |>
    group_by(lemma) |>
    summarize(pos = first(pos), 
              abs_freq = sum(abs_freq),
              nrm_freq = sum(nrm_freq)) |>
    arrange(desc(abs_freq))
  
  # select only words that has min freq or more
  df_freq <- df_freq |>
    filter(abs_freq >= min_freq)
  
  return(df_freq)
}

# ICLE data set

df_icle_all <- make_df_freq_all(
  here('data', 'corp_icle_ja.tsv')
)

df_freq_icle_verb <- make_df_freq_pos(df_icle_all, 'v', 3)
df_freq_icle_noun <- make_df_freq_pos(df_icle_all, 'n', 3)

# COCA data set

df_coca_all <- make_df_freq_all(
  here('data', 'corp_coca_sample.tsv')
)

df_freq_coca_verb <- make_df_freq_pos(df_coca_all, 'v', 5)
df_freq_coca_noun <- make_df_freq_pos(df_coca_all, 'n', 5)

# merge the dataframes

df_freq_verb <- merge(df_freq_coca_verb, df_freq_icle_verb,
                 by = 'lemma', all = TRUE,
                 suffixes = c('_n', '_l'))

df_freq_noun <- merge(df_freq_coca_noun, df_freq_icle_noun,
                      by = 'lemma', all = TRUE,
                      suffixes = c('_n', '_l'))

write_tsv(df_freq_verb, here('data', 'freq_verb.tsv'))
write_tsv(df_freq_noun, here('data', 'freq_noun.tsv'))
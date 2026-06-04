library(here)
library(tidyverse)
library(mclm)

# retrieve freq list
df_freq_verb <- read_tsv(here('data', 'freq_verb.tsv'))
df_freq_noun <- read_tsv(here('data', 'freq_noun.tsv'))

# prepare corpus as text string
transform_into_corpus_text <- function(tsv_path){
  df_tsv <- read_tsv(tsv_path)
  df_tsv_sel <- df_tsv[, 3:5]
  df_tsv_sel <- df_tsv_sel |>
    mutate(pos_simple = case_when(
      str_detect(pos, r'--[^(n[^u]|pn|pp).+]--') ~ 'n',
      str_detect(pos, r'--[^v[^m].+]--') ~ 'v',
      word %in% c('.', '?', '!') ~ 'p',
      TRUE ~ 'x'
    ))
  df_tsv_sel <- df_tsv_sel |>
    filter(
      str_detect(word, r'--[[a-zA-Z0-9]]--') |
        str_detect(pos_simple, 'p'))
  
  text_full <- apply(df_tsv_sel, 1, 
                     function(row) paste(row, collapse = '\t'))
  text_as_string <- paste(text_full, collapse = '\n')
  
  return(text_as_string)
}

# make a collocates data frame from one target word

make_df_col_from_tar <- function(
    corpus_text, target_lemma, target_pos, min_freq = 1){
  
  RE_NODE <- paste0(target_lemma, '/', target_pos)
  RE_BOUNDARY  <- r'--[^.*/p$]--'
  RE_TOKEN_SPLITTER <- r'--[\n+]--'
  RE_TOKEN_PATTERN <- r'--[^([^\t\n]+)\t([^\t\n]+)\t([^\t\n]+)\t([^\t\n]+)$]--'
  TOKEN_TRANSF <- r'--[\2/\4]--'

  if (target_pos == 'v'){
    collocate_pos <- 'n'
    W_LEFT <- 0
    W_RIGHT <- 5
  } else if (target_pos == 'n'){
    collocate_pos <- 'v'
    W_LEFT <- 5
    W_RIGHT <- 0
    }
  
  cooc <- corpus_text |> 
    surf_cooc(RE_NODE,
              w_left = W_LEFT,
              w_right = W_RIGHT,
              re_boundary = RE_BOUNDARY,
              re_token_splitter = RE_TOKEN_SPLITTER,
              re_token_extractor = RE_TOKEN_PATTERN,
              re_token_transf_in = RE_TOKEN_PATTERN,
              token_transf_out = TOKEN_TRANSF
    )

  scores <- 
    assoc_scores(cooc, min_freq = min_freq)
  
  df_scores <- as_tibble(scores)
  
  df_col_all <- df_scores |>
    separate(type, into = c('lemma', 'pos'),
             sep = r'--[/(?=[^/]+$)]--') # separate by the final slash
  
  df_col_pos <- df_col_all |>
    filter(pos == collocate_pos) |>
    filter(dir == 1) |>
    filter(PMI >= 1)
  
  df_col_pos$tar_lemma <- target_lemma
  df_col_pos$tar_pos <- target_pos
  
  df_col_result <- df_col_pos |>
    mutate(tar_col = paste0(tar_lemma, '/', lemma)) |>
    mutate(k = a + c) |>
    mutate(N = a + b + c + d) |>
    select(tar_col, tar_lemma, tar_pos, lemma, pos, a, k, N, PMI, G_signed)
  
  cat(paste0('finish working on the target: ', target_lemma, '\n'))
  
  return(df_col_result)
  
}

# icle data set

corpus_text_icle <- transform_into_corpus_text(
  here('data', 'corp_icle_ja.tsv')
)

df_col_verb_icle <- map_dfr(
  df_freq_verb$lemma,
  ~ make_df_col_from_tar(
    corpus_text = corpus_text_icle,
    target_lemma = .x,
    target_pos = 'v',
    min_freq = 3)
  )
write_tsv(df_col_verb_icle, here('data', 'col_verb_icle_ja.tsv'))

df_col_noun_icle <- map_dfr(
  df_freq_noun$lemma,
  ~ make_df_col_from_tar(
    corpus_text = corpus_text_icle,
    target_lemma = .x,
    target_pos = 'n',
    min_freq = 3)
)
write_tsv(df_col_noun_icle, here('data', 'col_noun_icle_ja.tsv'))

# coca data set

corpus_text_coca <- transform_into_corpus_text(
  here('data', 'corp_coca_sample.tsv')
)

df_col_verb_coca <- map_dfr(
  df_freq_verb$lemma,
  ~ make_df_col_from_tar(
    corpus_text = corpus_text_coca,
    target_lemma = .x,
    target_pos = 'v',
    min_freq = 5)
)
write_tsv(df_col_verb_coca, here('data', 'col_verb_coca.tsv'))

df_col_noun_coca <- map_dfr(
  df_freq_noun$lemma,
  ~ make_df_col_from_tar(
    corpus_text = corpus_text_coca,
    target_lemma = .x,
    target_pos = 'n', 
    min_freq = 5)
)
write_tsv(df_col_noun_coca, here('data', 'col_noun_coca.tsv'))
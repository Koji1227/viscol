# library(here)
# library(tidyverse)
# library(data.table)

# df_corp_coca <- read_tsv(here('data', 'corp_coca_sample.tsv'))
# df_corp_coca <- df_corp_coca |>
#   mutate(token_id = row_number()) |>
#   relocate(token_id, .after = sent_id) |>
#   mutate(pos_simple = case_when(
#     str_detect(pos, r'--[^(n[^u]|pn|pp).+]--') ~ 'n',
#     str_detect(pos, r'--[^v[^m].+]--') ~ 'v',
#     word %in% c('.', '?', '!') ~ 'p',
#     TRUE ~ 'x'
#   )) |>
#   filter(
#     str_detect(word, r'--[[a-zA-Z0-9]]--') |
#       str_detect(pos_simple, 'p'))

# this is the code M365 Copilot rewrote using a data.frame
# my original code can be found below

make_dt_conc <- function(pos_tar, target, collocate,
                         df_corp_coca, max_line = NULL){
  
  dt <- as.data.table(df_corp_coca)
  
  if (pos_tar == 'v'){
    pos_col <- 'n'
  } else if (pos_tar == 'n'){
    pos_col <- 'v'
  }
  
  dt_tar <- dt[(lemma == target) & (pos_simple == pos_tar),
               .(sent_id, doc_id, token_id)]
  
  dt_col <- dt[(lemma == collocate) & (pos_simple == pos_col),
               .(sent_id, doc_id, token_id)]
  
  cooc <- dt_tar[dt_col, on = 'sent_id', allow.cartesian = TRUE]
  
  cooc[, dist := i.token_id - token_id]
  
  if (pos_tar == 'v'){
    cooc <- cooc[(0 < dist) & (dist <= 5)]
    setorder(cooc, dist)
  } else if (pos_tar == 'n') {
    cooc <- cooc[(-5 <= dist) & (dist < 0)]
    setorder(cooc, -dist)
  }

  if (!is.null(max_line) && nrow(cooc) >= max_line) {
    cooc <- cooc[1:max_line]
  }
  
  corp_split <- split(dt, by = 'doc_id', keep.by = FALSE)
  
  make_one <- function(TOKEN_ID, DOC_ID) {
    
    doc <- corp_split[[as.character(DOC_ID)]]
    
    kw <- doc[token_id >= TOKEN_ID - 12 & token_id <= TOKEN_ID + 12,
              .(word, lemma, token_id)]
    
    kw[, role := fcase(
      lemma == target, 'tar',
      lemma == collocate, 'col',
      default = 'other'
    )]
    
    kw[, .(word, role)]
  }

  result <- Map(make_one, cooc$token_id, cooc$i.doc_id)
  
  return(result)
}

# my original code with data frame
# it works, but it takes a lot of time to show concordance

# make_df_conc <- function(pos_tar, target, collocate, 
#                          df_corp_coca, max_line = NULL){
#   
#   if(pos_tar == 'v'){
#     pos_col <- 'n'
#   } else if (pos_tar == 'n'){
#     pos_col <- 'v'
#   }
#   
#   df_corp_tar <- df_corp_coca |>
#     filter(lemma == target, pos_simple == pos_tar)
#   
#   df_corp_col <- df_corp_coca |>
#     filter(lemma == collocate, pos_simple == pos_col)
#   
#   df_corp_cooc <- inner_join(df_corp_tar, df_corp_col, by = 'sent_id',
#                              suffix = c('_tar', '_col'),
#                              relationship = 'many-to-many') |>
#     mutate(dist = token_id_col - token_id_tar)
#   
#   if (pos_tar == 'v'){
#     df_corp_cooc <- df_corp_cooc |>
#       filter(between(dist, 0, 5)) |>
#       arrange(dist)
#   } else if (pos_tar == 'n'){
#     df_corp_cooc <- df_corp_cooc |>
#       filter(between(dist, -5, 0)) |>
#       arrange(desc(dist))
#   }
#   
#   make_conc_df <- function(TOKEN_ID, DOC_ID, df_corp_coca,
#                            target, collocate){
#     df_conc_result <- df_corp_coca |>
#       filter(between(token_id, TOKEN_ID - 12, TOKEN_ID + 12) &
#                (doc_id == DOC_ID)) |>
#       mutate(role = case_when(
#         lemma == target ~ 'tar',
#         lemma == collocate ~ 'col',
#         TRUE ~ 'other'
#       )) |>
#       select(word, role)
#     return(df_conc_result)
#   }
#   
#   if(!is.null(max_line)){
#     if(nrow(df_corp_cooc) >= max_line){
#       df_corp_cooc <- df_corp_cooc[1:max_line, ]
#     }
#   }
#   
#   df_conc <- map2(
#     df_corp_cooc$token_id_tar,
#     df_corp_cooc$doc_id_tar,
#     ~ make_conc_df(.x, .y, df_corp_coca, target, collocate)
#   )
#   
#   return(df_conc)
# }

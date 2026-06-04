# library(here)
# library(tidyverse)
# library(visNetwork)
# 
# df_col_verb_icle <- read_tsv(here('data', 'col_verb_icle_ja.tsv'))
# df_col_verb_coca <- read_tsv(here('data', 'col_verb_coca.tsv'))
# 
# df_col_noun_icle <- read_tsv(here('data', 'col_noun_icle_ja.tsv'))
# df_col_noun_coca <- read_tsv(here('data', 'col_noun_coca.tsv'))
# 
# df_freq_noun <- read_tsv(here('data', 'freq_noun.tsv'))
# df_freq_verb <- read_tsv(here('data', 'freq_verb.tsv'))

make_network <- function(pos, target) {
  
  if (pos == 'v') {
    df_col_sel_icle <- df_col_verb_icle
    df_col_sel_coca <- df_col_verb_coca
    df_freq <- df_freq_noun
    df_tar <- df_freq_verb
  } else if (pos == 'n') {
    df_col_sel_icle <- df_col_noun_icle
    df_col_sel_coca <- df_col_noun_coca
    df_freq <- df_freq_verb
    df_tar <- df_freq_noun
  }
  
  df_col_icle <- df_col_sel_icle |>
    filter(tar_lemma == target)
  df_col_coca <- df_col_sel_coca |>
    filter(tar_lemma == target)
  df_freq <- df_freq |>
    select(lemma, abs_freq_n, nrm_freq_n, abs_freq_l, nrm_freq_l)
  df_tar <- df_tar |>
    filter(lemma == target)
  
  df_col_both <- inner_join(df_col_coca, df_col_icle,
                            by = 'tar_col',
                            suffix = c('_n', '_l'))
  df_col_onlyn <- anti_join(df_col_coca, df_col_icle,
                            by = 'tar_col')
  df_col_onlyl <- anti_join(df_col_icle, df_col_coca,
                            by = 'tar_col')
  
  df_col_both <- left_join(df_col_both, df_freq,
                           by = c('lemma_n' = 'lemma'))
  df_col_onlyn <- left_join(df_col_onlyn, df_freq,
                            by = 'lemma')
  df_col_onlyl <- left_join(df_col_onlyl, df_freq,
                            by = 'lemma')
  
  color_from_value <- function(x) {
    limit <- 2
    x_clipped <- pmin(pmax(x, -limit), limit)
    idx <- round((x_clipped + limit) / (2 * limit) * 255)
    pal <- colorRampPalette(c('blue', 'red'))(256)
    return(pal[idx + 1])
  }
  
  # the target node
  
  df_tar <- df_tar |>
    mutate(x = 0) |>
    mutate(y = 0) |>
    mutate(c_value = case_when(
      !is.na(nrm_freq_l) & !is.na(nrm_freq_n) ~ 
        log2(nrm_freq_l / nrm_freq_n),
      is.na(nrm_freq_l) & !is.na(nrm_freq_n) ~ 2,
      !is.na(nrm_freq_l) & is.na(nrm_freq_n) ~ -2
    )) |>
    mutate(color.background = color_from_value(c_value)) |>
    mutate(size = pmax(0.1, (3 * log2(1000000 * nrm_freq_n)))) |>
    mutate(width = NA)
  
  nodes_tar <- df_tar |>
    select(lemma, x, y, size, width, color.background)
  
  # the collocate nodes, category (i) (both corpora)
  
  df_col_both <- df_col_both |>
    mutate(lemma = lemma_n) |>
    mutate(x = 1500 / PMI_n) |>
    mutate(y_raw = 250 * (log2((nrm_freq_l) / (nrm_freq_n)))) |>
    mutate(y = case_when(
      y_raw <= 500 & y_raw >= -500 ~ y_raw,
      y_raw > 500 ~ 500,
      y_raw < -500 ~ -500
    )) |>
    mutate(c_value = log2((a_l / N_l) / (a_n / N_n))) |>
    mutate(color.background = color_from_value(c_value)) |>
    mutate(size = pmax(0.1, (3 * log2(1000000 * a_n / N_n)))) |>
    mutate(width = pmax(0.1, (log10(G_signed_n))))
  
  nodes_col_i <- df_col_both |>
    select(lemma, x, y, size, width, color.background)
  
  # the collocate nodes, category (ii) (only in native)
  
  df_col_onlyn <- df_col_onlyn |>
    mutate(x = 1500 / PMI) |>
    mutate(y_raw = case_when(
      !is.na(nrm_freq_l) & !is.na(nrm_freq_n) ~ 
        250 * log2(nrm_freq_l / nrm_freq_n),
      is.na(nrm_freq_l) & !is.na(nrm_freq_n) ~ -500,
      !is.na(nrm_freq_l) & is.na(nrm_freq_n) ~ 500
    )) |>
    mutate(y = case_when(
      y_raw <= 500 & y_raw >= -500 ~ y_raw,
      y_raw > 500 ~ 500,
      y_raw < -500 ~ -500
    )) |>
    mutate(c_value = -2) |>
    mutate(color.background = color_from_value(c_value)) |>
    mutate(size = pmax(0.1, 3 * log2(1000000 * a / N))) |>
    mutate(width = pmax(0.1, log10(G_signed) / 1))
  
  df_col_onlyn <- df_col_onlyn |>
    mutate(eval = PMI * size) |>
    slice_max(eval, n = 10)
  
  nodes_col_ii <- df_col_onlyn |>
    select(lemma, x, y, size, width, color.background)
  
  # the collocate nodes, category (iii) (only in learner)
  
  df_col_onlyl <- df_col_onlyl |>
    mutate(x = 1500 / PMI) |>
    mutate(y_raw = case_when(
      !is.na(nrm_freq_l) & !is.na(nrm_freq_n) ~ 
        250 * log2(nrm_freq_l / nrm_freq_n),
      is.na(nrm_freq_l) & !is.na(nrm_freq_n) ~ -500,
      !is.na(nrm_freq_l) & is.na(nrm_freq_n) ~ 500
    )) |>
    mutate(y = case_when(
      y_raw <= 500 & y_raw >= -500 ~ y_raw,
      y_raw > 500 ~ 500,
      y_raw < -500 ~ -500
    )) |>
    mutate(c_value = 2) |>
    mutate(color.background = color_from_value(c_value)) |>
    mutate(size = pmax(0.1, (3 * log2(1000000 * a / N)))) |>
    mutate(width = NA)
  
  df_col_onlyl <- df_col_onlyl |>
    mutate(eval = PMI * size) |>
    slice_max(eval, n = 10)

  nodes_col_iii <- df_col_onlyl |>
    select(lemma, x, y, size, width, color.background)
  
  # bind all nodes
  nodes_all <- bind_rows(
    'tar' = nodes_tar,
    'i' = nodes_col_i,
    'ii' = nodes_col_ii,
    'iii' = nodes_col_iii,
    .id = 'category'
  )
  
  nodes <- nodes_all |>
    mutate(id = ifelse(category == 'tar', '_target_', lemma)) |>
    mutate(label = lemma) |>
    mutate(color = map(color.background, function(b) {
      list(
        background = b,
        border = 'white',
        highlight = list(
          background = b,
          border = 'yellow')
        )})
      ) |>
    select(id, label, x, y, size, color)
  
  nodes_with_edge <- nodes_all |>
    filter(category == 'i' | category == 'ii')
  
  N_EDGE <- nrow(nodes_with_edge)
  list_width <- nodes_with_edge$width
  edges <- data.frame(
    from = rep('_target_', N_EDGE),
    to   = nodes_with_edge$lemma,
    width = list_width,
    color = rep('#C0C0C0', N_EDGE)
  )
  
  network <- visNetwork(nodes, edges, 
                        width = '100%', height = '100vh') |>
    visNodes(fixed = TRUE,
             font = list(
               size = 20,
               color = 'black',
               strokeWidth = 3,
               strokeColor = 'white'
             )) |>
    visEdges(dashes = FALSE) |>
    visPhysics(enabled = FALSE) |>
    visOptions(highlightNearest = TRUE) |>
    visInteraction(selectable = TRUE,
                   multiselect = FALSE) |>
    visEvents(
      doubleClick = "
    function(params) {
      Shiny.setInputValue('net_double', params.nodes);
    }"
    )
  
  return(network)
  
}
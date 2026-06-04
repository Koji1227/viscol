# library(here)
# library(tidyverse)

# df_col_verb_icle <- read_tsv(here('data', 'col_verb_icle.tsv'))
# df_col_verb_coca <- read_tsv(here('data', 'col_verb_coca.tsv'))
# 
# df_col_noun_icle <- read_tsv(here('data', 'col_noun_icle.tsv'))
# df_col_noun_coca <- read_tsv(here('data', 'col_noun_coca.tsv'))
# 
# df_freq_noun <- read_tsv(here('data', 'freq_noun.tsv'))
# df_freq_verb <- read_tsv(here('data', 'freq_verb.tsv'))

make_lists <- function(pos, target, N = 50) {
  
  if (pos == 'v') {
    df_col_icle <- df_col_verb_icle
    df_col_coca <- df_col_verb_coca
  } else if (pos == 'n') {
    df_col_icle <- df_col_noun_icle
    df_col_coca <- df_col_noun_coca
  }
  
  df_col_sel_icle <- df_col_icle |>
    filter(tar_lemma == target) |>
    rename(PMI_l = PMI, G_l = G_signed) |>
    mutate(nrm_freq_l = 1000000 * a / N) |>
    select(tar_col, nrm_freq_l, PMI_l, G_l)
  
  df_col_sel_coca <- df_col_coca |>
    filter(tar_lemma == target) |>
    rename(PMI_n = PMI, G_n = G_signed) |>
    mutate(nrm_freq_n = 1000000 * a / N) |>
    select(tar_col, nrm_freq_n, PMI_n, G_n)
  
  color_from_value <- function(x) {
    limit <- 2
    x_clipped <- pmin(pmax(x, -limit), limit)
    idx <- round((x_clipped + limit) / (2 * limit) * 255)
    pal <- colorRampPalette(c('#A0A0FF', '#FFA0A0'))(256)
    return(pal[idx + 1])
  }
  
  df_col_sel_both <- inner_join(df_col_sel_icle, df_col_sel_coca,
                                by = 'tar_col', suffix = c ('', ''))
  
  df_col_sel_both <- df_col_sel_both |>
    mutate(c_value = log2(nrm_freq_l / nrm_freq_n)) |>
    select(tar_col, c_value)
  
  df_col_sel_icle <- left_join(df_col_sel_icle, df_col_sel_both,
                               by = 'tar_col', suffix = c ('', ''),)
  df_col_sel_icle <- df_col_sel_icle |>
    mutate(c_value = ifelse(is.na(c_value), 3, c_value)) |>
    mutate(color = color_from_value(c_value))
  
  df_col_sel_coca <- left_join(df_col_sel_coca, df_col_sel_both,
                               by = 'tar_col', suffix = c ('', ''),)
  df_col_sel_coca <- df_col_sel_coca |>
    mutate(c_value = ifelse(is.na(c_value), -3, c_value)) |>
    mutate(color = color_from_value(c_value))  
  
  df_col_top_icle <- df_col_sel_icle |>
    arrange(desc(PMI_l)) |>
    slice_head(n = N)
  
  df_col_top_coca <- df_col_sel_coca |>
    arrange(desc(PMI_n)) |>
    slice_head(n = N)
  
  return(list(
    df_icle = df_col_top_icle,
    df_coca = df_col_top_coca))
}
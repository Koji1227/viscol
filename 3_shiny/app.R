# rsconnect::deployApp(appName = 'viscol')
# rsconnect::showLogs()

library(shiny)
library(shinycssloaders)
library(tidyverse)
library(data.table)
library(visNetwork)
library(rsconnect)
# library(here)

# install data

# df_col_verb_icle <- read_tsv('data/col_verb_icle_ja.tsv')
# df_col_verb_coca <- read_tsv('data/col_verb_coca.tsv')
df_col_verb_icle <- readRDS(
  url('https://raw.githubusercontent.com/Koji1227/viscol/main/2_visualisation/data/col_verb_icle_ja.rds',
      'rb')
)
df_col_verb_coca <- readRDS(
  url('https://raw.githubusercontent.com/Koji1227/viscol/main/2_visualisation/data/col_verb_coca.rds',
      'rb')
)

# df_col_noun_icle <- read_tsv('data/col_noun_icle_ja.tsv')
# df_col_noun_coca <- read_tsv('data/col_noun_coca.tsv')
df_col_noun_icle <- readRDS(
  url('https://raw.githubusercontent.com/Koji1227/viscol/main/2_visualisation/data/col_noun_icle_ja.rds',
      'rb')
)
df_col_noun_coca <- readRDS(
  url('https://raw.githubusercontent.com/Koji1227/viscol/main/2_visualisation/data/col_noun_coca.rds',
      'rb')
)

# df_freq_noun <- read_tsv('data/freq_noun.tsv')
# df_freq_verb <- read_tsv('data/freq_verb.tsv')
df_freq_noun <- readRDS(
  url('https://raw.githubusercontent.com/Koji1227/viscol/main/2_visualisation/data/freq_noun.rds',
      'rb')
)
df_freq_verb <- readRDS(
  url('https://raw.githubusercontent.com/Koji1227/viscol/main/2_visualisation/data/freq_verb.rds',
      'rb')
)

# df_corp_coca <- read_tsv('data/corp_coca_sample.tsv')
df_corp_coca <- readRDS(
  url('https://raw.githubusercontent.com/Koji1227/viscol/main/2_visualisation/data/corp_coca_short.rds',
      'rb')
  )

df_corp_coca <- df_corp_coca |>
  mutate(token_id = row_number()) |>
  relocate(token_id, .after = sent_id) |>
  mutate(pos_simple = case_when(
    str_detect(pos, r'--[^(n[^u]|pn|pp).+]--') ~ 'n',
    str_detect(pos, r'--[^v[^m].+]--') ~ 'v',
    word %in% c('.', '?', '!') ~ 'p',
    TRUE ~ 'x'
  )) |>
  filter(
    str_detect(word, r'--[[a-zA-Z0-9]]--') |
      str_detect(pos_simple, 'p'))

# network.R

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

# list_comparison.R

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

# concordance.R

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

# app.R

# most of user interface (HTML) parts are based on
# the discussion with M365 Copilot
# I describe the system design, then Copilot suggested HTML
# then, I modified the code

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    width = 2,
    
    tags$div(
      style = paste0(
        'font-family: "Segoe UI", Arial, sans-serif; ',
        'font-size: 24px; ',
        'font-weight: bold; ',
        'margin-bottom: 10px;'
      ),
      
      HTML(
        paste0(
          '<span style = "color: #333;">Vis</span>',
          '<span style = "color: #00008B;">Col.</span>'
        )
      )
    ),
    
    selectInput(
      'pos',
      'POS of the target word',
      choices = c('verb' = 'v', 'noun' = 'n'),
      selected = 'v'
    ),
    
    textInput('target', 'Target word', value = 'take'),
    
    
    tags$div(
      style = '
    display: flex;
    gap: 5px;
    flex-wrap: wrap;
  ',
  
  actionButton('see_net', 'See the network', style = 'width: 140px; white-space: normal;'),
  
  actionButton('see_list', 'See the lists', style = 'width: 140px; white-space: normal;')
    ),
  
  # actionButton('see_net', 'See the network',
  #              style = 'white-space: normal'),
  #
  # div(style = 'height: 10px;'),
  #
  # actionButton('see_list', 'See the lists',
  #              style = 'white-space: normal'),
  
  div(
    style = paste0(
      'margin-top: 20px; ',
      'border-top: 2px solid #aaa;',
      'padding-top: 10px;'
    )
  ),
  
  uiOutput('col_input'),
  
  tags$div(
    style = '
    display: flex;
    gap: 5px;
    flex-wrap: wrap;
  ',
  
  actionButton('see_conc', 'See the examples', style = 'width: 140px; white-space: normal;'),
  
  actionButton('swap', 'Target <-> Collocate', style = 'width: 140px; white-space: normal;')
  ),
  
  # actionButton('see_conc', 'See the examples',
  #              style = 'white-space: normal'),
  #
  # div(style = 'height: 10px;'),
  #
  # actionButton('swap', 'Target <--> Collocate',
  #              style = 'white-space: normal')
  
  ),
  
  mainPanel(width = 10, uiOutput('main_view'))
))

server <- function(input, output, session) {
  output$col_input <- renderUI({
    req(input$pos)
    
    q <- parseQueryString(session$clientData$url_search)
    
    val <- if (!is.null(q$col))
      q$col
    else
      ''
    
    
    if (input$pos == 'v') {
      textInput('col', 'Collocate word (noun)', value = val)
    } else if (input$pos == 'n') {
      textInput('col', 'Collocate word (verb)', value = val)
    }
  })
  
  clean_target <- function(x) {
    x |>
      tolower() |>
      trimws()
  }
  
  query <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  trigger <- reactiveVal(0)
  
  mode <- reactiveVal(NULL) # mode: 'net' 'list' 'conc'
  
  selected_pair <- reactiveVal(NULL)
  
  net_height <- reactive({
    width <- session$clientData$output_net_width
    
    if (is.null(width))
      return("600px")
    
    height <- round(width * 2 / 3)
    
    paste0(height, "px")
  })
  
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(q$pos)) {
      updateSelectInput(session, 'pos', selected = q$pos)
    }
    
    if (!is.null(q$target)) {
      updateTextInput(session, 'target', value = q$target)
    }
    
    if (!is.null(q$type)) {
      mode(q$type)
    }
    
    if (!is.null(q$col)) {
      updateTextInput(session, 'col', value = q$col)
    }
    
    trigger(trigger() + 1)
  })
  
  
  make_url <- function(pos,
                       target,
                       col = '',
                       type,
                       l1 = 'ja') {
    paste0(
      '?pos=',
      pos,
      '&target=',
      URLencode(target),
      '&col=',
      URLencode(col),
      '&type=',
      type,
      '&l1=',
      l1
    )
  }
  
  observeEvent(input$see_net, {
    target_clean <- clean_target(input$target)
    
    mode('net')
    
    updateQueryString(make_url(input$pos, target_clean, '', 'net', 'ja'), mode = 'push')
    
    trigger(trigger() + 1)
  })
  
  observeEvent(input$see_list, {
    target_clean <- clean_target(input$target)
    
    mode('list')
    
    updateQueryString(make_url(input$pos, target_clean, '', 'list', 'ja'), mode = 'push')
    
    trigger(trigger() + 1)
  })
  
  observeEvent(input$see_conc, {
    target_clean <- clean_target(input$target)
    col_clean <- clean_target(input$col)
    
    mode('conc')
    
    updateQueryString(make_url(input$pos, target_clean, col_clean, 'conc', 'ja'),
                      mode = 'push')
    
    trigger(trigger() + 1)
  })
  
  # network data
  net_data <- reactive({
    trigger()
    
    req(input$pos, input$target)
    
    target_clean <- clean_target(input$target)
    make_network(input$pos, target_clean)
  })
  
  
  # list data
  list_data <- reactive({
    trigger()
    
    req(input$pos, input$target)
    
    target_clean <- clean_target(input$target)
    make_lists(input$pos, target_clean)
  })
  
  
  # main view
  output$main_view <- renderUI({
    if (!is.null(mode()) && mode() == 'net') {
      net <- net_data()
      
      return(tagList(
        tags$div(
                 style = '
                 position: relative;
                 width: 100%;
                 overflow: hidden;
                 ',
                 
                 # network
                 visNetworkOutput('net', height = net_height()),
          
                 tags$div(
                          style = '
                          position: absolute;
                          top: 10px;
                          width: 100%;
                          display: grid;
                          grid-template-columns: 1fr auto 1fr;
                          align-items: center;
                          font-weight: bold;
                          font-size: 14px;
                          pointer-events: none;
                          ',
  
                          # left
                          tags$div(
                                   style = '
                                   text-align: left;
                                   padding-left: 20px;
                                   color: blue;
                                   ',
                                    '↑ Native corpus (COCA)'
                                   ),
  
                          # centre
                          tags$div(
                                   style = '
                                   text-align: center;
                                   white-space: wrap;
                                   ',
                                   'High PMI ← → Low PMI'
                                   ),
  
                          # right (blank)
                          tags$div("")
                          ),
  
                 # bottom
                 tags$div(
                          style = '
                          position: absolute;
                          bottom: 0;
                          left: 0;
                          width: 100%;
                          text-align: left;
                          color: red;
                          font-weight: bold;
                          font-size: 14px;
                          pointer-events: none;
                          ',
                          '↓ Learner corpus (ICLE)'
                          )
                )
           )
      )
      
    } else if (!is.null(mode()) && mode() == 'list') {
      return(uiOutput('tables_with_lines'))
      
    } else if (!is.null(mode()) && mode() == 'conc') {
      return(withSpinner(
        uiOutput('concordance'),
        type = 3,
        color.background = 'white'
      ))
      
    }
  })
  
  # draw the network
  output$net <- renderVisNetwork({
    req(mode() == 'net')
    net <- net_data()
    net |>
      visEvents(
        select = "function(nodes) {
        if (nodes.nodes.length > 0) {
          Shiny.setInputValue('net_click', nodes.nodes[0], {priority: 'event'})
        }
      }"
      )
    
  })
  
  # draw the lists
  output$tables_with_lines <- renderUI({
    dat <- list_data()
    df_coca <- dat$df_coca |>
      mutate(i = row_number())
    df_icle <- dat$df_icle |>
      mutate(j = row_number())
    
    pairs <- inner_join(df_coca |> select(tar_col, i),
                        df_icle |> select(tar_col, j),
                        by = 'tar_col')
    
    row_height <- 32
    n_max <- max(nrow(df_icle), nrow(df_coca))
    container_height <- (n_max + 2) * row_height
    
    
    # lines between two tables
    make_lines <- function() {
      lapply(seq_len(nrow(pairs)), function(k) {
        i <- pairs$i[k]
        j <- pairs$j[k]
        
        y1 <- (i + 2 - 0.5) * row_height
        y2 <- (j + 2 - 0.5) * row_height
        
        tags$line(
          x1 = '40%',
          y1 = y1,
          x2 = '60%',
          y2 = y2,
          stroke = 'gray',
          `stroke-width` = 1
        )
      })
    }
    
    # make tables
    make_table <- function(df, side) {
      title <- if (side == 'coca') {
        "Native corpus (COCA)"
      } else if (side == "icle") {
        "Learner corpus (ICLE)"
      }
      
      title_div <- tags$div(
        style = paste0(
          'min-height: ',
          row_height,
          'px;',
          'display: flex;',
          'align-items: center;',
          'font-weight: bold;',
          'font-size: 16px;',
          'word-break: break-word;',
          'background-color: ',
          if (side == 'coca') {
            '#A0A0FF'
          } else if (side == 'icle') {
            '#FFA0A0'
          }
        ),
        title
      )
      
      cols <- c('target/collocate', 'Freq (per M)', 'PMI', 'G')
      
      header <- tags$div(
        style = paste0(
          'min-height: ',
          row_height,
          'px;',
          'display: flex;',
          'font-weight: bold;',
          'border-bottom: 2px solid black;',
          'align-items: center;',
          'word-break: break-word;',
          'flex-wrap: wrap;'
        ),
        tags$div(style = 'width: 40%; padding-left: 5px;', cols[1]),
        tags$div(style = 'width: 20%; text-align: right;', cols[2]),
        tags$div(style = 'width: 20%; text-align: right;', cols[3]),
        tags$div(style = 'width: 20%; text-align: right;', cols[4]),
      )
      
      rows <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        
        values <- if (side == 'coca') {
          c(row$nrm_freq_n, row$PMI_n, row$G_n)
        } else if (side == 'icle') {
          c(row$nrm_freq_l, row$PMI_l, row$G_l)
        }
        
        is_selected <- !is.null(selected_pair()) &&
          row$tar_col == selected_pair()
        
        tags$div(
          style = paste0(
            'min-height: ',
            row_height,
            'px;',
            'display: flex;',
            'align-items: center;',
            'flex-wrap: wrap;',
            'word-break: break-word;',
            'align-items: center;',
            'border-bottom: 1px solid #ddd;',
            'background-color: ',
            row$color,
            ';',
            if (is_selected) {
              'border: 2px solid yellow; font-weight: bold;'
            } else {
              ''
            }
            
          ),
          
          ondblclick = sprintf(
            "Shiny.setInputValue('list_double', '%s')",
            row$tar_col
          ),
          
          tags$div(
            style = paste0(
              'width: 40%; ',
              'padding-left: 5px; ',
              'white-space: normal; ',
              'word-break: break-word;'
            ),
            onclick = sprintf(
              "Shiny.setInputValue('list_click', '%s', {priority: 'event'})",
              row$tar_col
            ),
            
            row$tar_col
          ),
          
          tags$div(
            style = paste0(
              'width: 20%; text-align: right;',
              'white-space: normal;',
              'word-break: break-word;'
            ),
            sprintf('%.1f', values[1])
          ),
          tags$div(
            style = paste0(
              'width: 20%; text-align: right;',
              'white-space: normal;',
              'word-break: break-word;'
            ),
            sprintf('%.2f', values[2])
          ),
          tags$div(
            style = paste0(
              'width: 20%; text-align: right;',
              'white-space: normal;',
              'word-break: break-word;'
            ),
            sprintf('%.2f', values[3])
          )
        )
      })
      
      tags$div(style = 'width: 40%;', title_div, header, rows)
    }
    
    tags$div(
      style = paste0(
        'position: relative;',
        'height:',
        container_height,
        'px;',
        'display: flex;'
      ),
      
      tags$svg(
        width = '100%',
        height = container_height,
        style = paste0(
          'position: absolute; ',
          'left: 0; ',
          'top: 0; ',
          'pointer-events:none;'
        ),
        make_lines()
      ),
      
      make_table(df_coca, 'coca'),
      tags$div(style = 'width: 20%;'),
      make_table(df_icle, 'icle')
    )
  })
  
  # prepare concordance data
  conc_data <- reactive({
    trigger()
    
    req(mode() == 'conc')
    
    req(input$pos, input$target, input$col)
    
    target_clean <- clean_target(input$target)
    col_clean <- clean_target(input$col)
    
    make_dt_conc(input$pos, target_clean, col_clean, df_corp_coca, 100)
  })
  
  format_conc <- function(df_line) {
    words <- df_line$word
    
    for (i in seq_along(words)) {
      if (df_line$role[i] %in% c('tar', 'col')) {
        words[i] <- paste0('<span style = "background-color: #c8f7c5;">',
                           words[i],
                           '</span>')
      }
    }
    
    text <- paste(words, collapse = ' ')
    text <- gsub(' ([.,!?;:\']|n\'t)', '\\1', text)
    
    return(text)
  }
  
  # show concordance
  output$concordance <- renderUI({
    conc_list <- conc_data()
    
    if (is.null(conc_list) || length(conc_list) == 0) {
      return(tags$div("No data"))
    }
    
    rows <- lapply(seq_along(conc_list), function(i) {
      df_line <- conc_list[[i]]
      formatted <- format_conc(df_line)
      
      bg <- if (i %% 2 == 0)
        '#fafafa'
      else
        'white'
      
      tags$tr(
        style = paste0('background-color:', bg, ';'),
        
        tags$td(style = '
                border-bottom: 1px solid #ddd; 
                padding: 6px;
                text-align: right;
                color: #555;',
                i),
        
        tags$td(style = '
                border-bottom: 1px solid #ddd;
                padding: 6px;
                font-family: "Segoe UI", "Helvetica Neue", Arial, sans-serif;
                font-size: 14px;',
                HTML(formatted)
                )
      )
    })
    
    tags$div(
      style = 'max-height: container_height; overflow-y: auto;',
      
      tags$table(style = '
                 border-collapse: collapse; width: 100%;
                 ',
                 tags$thead(tags$tr(
                   tags$th('#', style = '
                           text-align: right;
                           padding: 6px;
                           border-bottom: 2px solid #aaa;
                           '),
                   tags$th('Examples from native corpus (COCA)',
                           style = '
                           padding: 6px;border-bottom: 
                           2px solid #aaa;
                           ')
      )), tags$tbody(rows))
    )
  })
  
  # swap button
  observeEvent(input$swap, {
    if (input$col == '')
      return()
    
    tar <- input$target
    col <- input$col
    
    if (is.null(tar) ||
        is.null(col) || tar == '' || col == '')
      return()
    
    updateTextInput(session, 'target', value = col)
    updateTextInput(session, 'col', value = tar)
    
    new_pos <- ifelse(input$pos == 'v', 'n', 'v')
    updateSelectInput(session, 'pos', selected = new_pos)
    
    updateQueryString(make_url(new_pos, col, tar, mode(), 'ja'), mode = 'push')
    
  })
  
  # single click in network
  observeEvent(input$net_click, {
    new_col <- clean_target(input$net_click)
    
    if (is.null(new_col) || new_col == '')
      return()
    if (new_col == '_target_')
      return()
    
    updateTextInput(session, 'col', value = new_col)
  })
  
  
  # double click in network
  observeEvent(input$net_double, {
    new_target <- input$net_double
    
    if (length(new_target) == 0)
      return()
    new_target <- clean_target(new_target[[1]])
    
    if (new_target == clean_target(input$target))
      return()
    
    new_pos <- ifelse(input$pos == 'n', 'v', 'n')
    
    updateTextInput(session, 'target', value = new_target)
    updateSelectInput(session, 'pos', selected = new_pos)
    
    updateQueryString(make_url(new_pos, new_target, '', 'net', 'ja'), mode = 'push')
    
  })
  
  # single click in lists
  observeEvent(input$list_click, {
    pair <- input$list_click
    
    if (is.null(pair) || pair == '')
      return()
    
    selected_pair(pair)
    
    parts <- strsplit(pair, '/')[[1]]
    
    new_tar <- parts[1]
    new_col <- parts[2]
    
    updateTextInput(session, 'target', value = new_tar)
    updateTextInput(session, 'col', value = new_col)
  })
  
  
  # double click in lists
  observeEvent(input$list_double, {
    new_target <- sub('.*/', '', input$list_double)
    
    if (new_target == clean_target(input$target))
      return()
    
    new_pos <- ifelse(input$pos == 'n', 'v', 'n')
    
    updateTextInput(session, 'target', value = new_target)
    updateSelectInput(session, 'pos', selected = new_pos)
    
    updateQueryString(make_url(new_pos, new_target, '', 'list', 'ja'), mode = 'push')
    
  })
}

shinyApp(ui, server)
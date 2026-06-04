library(shiny)
library(shinycssloaders)
library(tidyverse)
library(data.table)
library(here)
library(visNetwork)

source(here('scripts', 'network.R'))
source(here('scripts', 'list_comparison.R'))
source(here('scripts', 'concordance.R'))

# install data

df_col_verb_icle <- read_tsv(here('data', 'col_verb_icle_ja.tsv'))
df_col_verb_coca <- read_tsv(here('data', 'col_verb_coca.tsv'))

df_col_noun_icle <- read_tsv(here('data', 'col_noun_icle_ja.tsv'))
df_col_noun_coca <- read_tsv(here('data', 'col_noun_coca.tsv'))

df_freq_noun <- read_tsv(here('data', 'freq_noun.tsv'))
df_freq_verb <- read_tsv(here('data', 'freq_verb.tsv'))

df_corp_coca <- read_tsv(here('data', 'corp_coca_sample.tsv'))
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

# app

# most of user interface (HTML) parts are based on
# the discussion with M365 Copilot
# I describe the system design, then Copilot suggested HTML
# then, I modified the code

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      tags$div(
        style = paste0(
          'font-family: "Segoe UI", Arial, sans-serif; ',
          'font-size: 24px; ',
          'font-weight: bold; ',
          'margin-bottom: 10px;'
          ),
        
        HTML(paste0(
          '<span style = "color: #333;">Vis</span>',
          '<span style = "color: #00008B;">Col.</span>'
        ))
        ),

      selectInput('pos', 'POS of the target word',
                  choices = c('verb' = 'v', 'noun' = 'n'),
                  selected = 'v'),
      
      textInput('target', 'Target word', value = 'take'),
      
      actionButton('see_net', 'See the network',
                   style = 'white-space: normal'),
      
      div(style = 'height: 10px;'),
      
      actionButton('see_list', 'See the lists',
                   style = 'white-space: normal'),
      
      div(style = paste0('margin-top: 20px; ',
                         'border-top: 2px solid #aaa;',
                         'padding-top: 10px;')
          ),
      
      uiOutput('col_input'),
      
      actionButton('see_conc', 'See the examples',
                   style = 'white-space: normal'),
      
      div(style = 'height: 10px;'),
      
      actionButton('swap', 'Target <--> Collocate',
                   style = 'white-space: normal')
      
    ),
    
    mainPanel(
      width = 10,
      
      uiOutput('main_view') 
    )
  )
)

server <- function(input, output, session) {
  
  output$col_input <- renderUI({
    
    req(input$pos)
    
    q <- parseQueryString(session$clientData$url_search)
    
    val <- if (!is.null(q$col)) q$col else ''

    
    if (input$pos == 'v'){
      textInput('col', 'Collocate word (noun)', value = val)
    } else if (input$pos == 'n'){
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
  
  
  make_url <- function(pos, target, col = '', type, l1 = 'ja') {
    paste0(
      '?pos=', pos,
      '&target=', URLencode(target),
      '&col=', URLencode(col),
      '&type=', type,
      '&l1=', l1
    )
  }
  
  observeEvent(input$see_net, {
    
    target_clean <- clean_target(input$target)
    
    mode('net')
    
    updateQueryString(
      make_url(input$pos, target_clean, '', 'net', 'ja'),
      mode = 'push'
    )
    
    trigger(trigger() + 1)
  })
  
  observeEvent(input$see_list, {
    
    target_clean <- clean_target(input$target)
    
    mode('list')
    
    updateQueryString(
      make_url(input$pos, target_clean, '', 'list', 'ja'),
      mode = 'push'
    )
    
    trigger(trigger() + 1)
  })
  
  observeEvent(input$see_conc, {
    
    target_clean <- clean_target(input$target)
    col_clean <- clean_target(input$col)
    
    mode('conc')
    
    updateQueryString(
      make_url(input$pos, target_clean, col_clean, 'conc', 'ja'),
      mode = 'push'
    )
    
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
      
      return(
        tagList(
          tags$div(
            style = "
            display: flex;
            flex-direction: column;
            height: 100vh;
          ",
          
          # upper part
          tags$div(
            style = "
              display: grid;
              grid-template-columns: 1fr auto 1fr;
              align-items: center;
              padding: 6px 10px;
              font-size: 14px;
            ",
            
            tags$div(
              style = "justify-self: start; color: blue; font-weight: bold;",
              "↑ Native corpus (COCA)"
            ),
            
            tags$div(
              style = "justify-self: center; color: black; font-weight: bold;",
              "High PMI ← → Low PMI"
            ),
            
            tags$div("")
          ),
          
          # mian part
          tags$div(
            style = "flex:1;",
            visNetworkOutput("net", height = "100%")
          ),
          
          # bottom part
          tags$div(
            style = "
              padding: 6px 10px;
              font-size: 14px;
              color: red;
              font-weight: bold;
            ",
            "↓ Learner corpus (ICLE)"
          )
          )
        )
      )
    } else if (!is.null(mode()) && mode() == 'list') {
      
      return(uiOutput('tables_with_lines'))
      
    }
    
    else if (!is.null(mode()) && mode() == 'conc') {
      
      return(
        withSpinner(
          uiOutput('concordance'),
          type = 3,
          color.background = 'white'
        )
      )
      
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
    
    pairs <- inner_join(
      df_coca |> select(tar_col, i),
      df_icle |> select(tar_col, j),
      by = 'tar_col'
    )
    
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
          x1 = '40%', y1 = y1,
          x2 = '60%', y2 = y2,
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
          'height: ', row_height, 'px;',
          'display: flex;',
          'align-items: center;',
          'font-weight: bold;',
          'font-size: 16px;',
          'background-color: ',
          if (side == 'coca'){
            '#A0A0FF'
          } else if (side == 'icle'){
            '#FFA0A0'
          }
        ),
        title
      )
      
      cols <- c('target/collocate', 'Freq (per M)', 'PMI', 'G')
      
      header <- tags$div(
        style = paste0(
          'height: ', row_height, 'px;',
          'display: flex;',
          'font-weight: bold;',
          'border-bottom: 2px solid black;',
          'align-items: center;'
        ),
        tags$div(style = 'width: 40%; padding-left: 5px;',
                 cols[1]),
        tags$div(style= 'width: 20%; text-align: right;',
                 cols[2]),
        tags$div(style= 'width: 20%; text-align: right;',
                 cols[3]),
        tags$div(style= 'width: 20%; text-align: right;',
                 cols[4]),
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
            'height: ', row_height, 'px;',
            'display: flex;',
            'align-items: center;',
            'border-bottom: 1px solid #ddd;',
            'background-color: ', row$color, ';',
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
          
          tags$div(style = 'width: 20%; text-align: right;',
                   sprintf('%.1f', values[1])),
          tags$div(style = 'width: 20%; text-align: right;',
                   sprintf('%.2f', values[2])),
          tags$div(style = 'width: 20%; text-align: right;',
                   sprintf('%.1f', values[3]))
        )
      })
      
      tags$div(style = 'width: 40%;', title_div, header, rows)
    }
    
    tags$div(
      style = paste0(
        'position: relative;',
        'height:', container_height, 'px;',
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
    
    make_dt_conc(
      input$pos,
      target_clean,
      col_clean,
      df_corp_coca,
      100
    )
  })
  
  format_conc <- function(df_line) {
    
    words <- df_line$word
    
    for (i in seq_along(words)) {
      if (df_line$role[i] %in% c('tar', 'col')) {
        words[i] <- paste0(
          '<span style = "background-color: #c8f7c5;">',
          words[i],
          '</span>'
        )
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
      
      bg <- if (i %% 2 == 0) "#fafafa" else "white"
      
      tags$tr(
        style = paste0("background-color:", bg, ";"),
        
        tags$td(
          style = "border-bottom: 1px solid #ddd; padding: 6px; text-align: right; color: #555;",
          i
        ),
        
        tags$td(
          style = paste0(
            "border-bottom: 1px solid #ddd; ",
            "padding: 6px; ",
            "font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif; ",
            "font-size: 14px;"
          ),
          HTML(formatted)
        )
      )
    })
    
    tags$div(
      style = paste0(
        'max-height: container_height; overflow-y: auto;'
        ),
      
      tags$table(
        style = "border-collapse: collapse; width: 100%;",
        
        tags$thead(
          tags$tr(
            tags$th(
              '#',
              style = "text-align: right; padding: 6px; border-bottom: 2px solid #aaa;"
            ),
            tags$th(
              'Examples from native corpus (COCA)',
              style = "padding: 6px; border-bottom: 2px solid #aaa;"
            )
          )
        ),
        
        tags$tbody(rows)
      )
    )
  })
  
  # swap button
  observeEvent(input$swap, {
    
    if (input$col == '') return()
    
    tar <- input$target
    col <- input$col
    
    if (is.null(tar) || is.null(col) || tar == '' || col == '') return()
    
    updateTextInput(session, 'target', value = col)
    updateTextInput(session, 'col', value = tar)
    
    new_pos <- ifelse(input$pos == 'v', 'n', 'v')
    updateSelectInput(session, 'pos', selected = new_pos)
    
    updateQueryString(
      make_url(new_pos, col, tar, mode(), 'ja'),
      mode = 'push'
    )
    
  })
  
  # single click in network
  observeEvent(input$net_click, {
    
    new_col <- clean_target(input$net_click)
    
    if (is.null(new_col) || new_col == '') return()
    if (new_col == '_target_') return()
    
    updateTextInput(session, 'col', value = new_col)
  })
  
  
  # double click in network
  observeEvent(input$net_double, {
    
    new_target <- input$net_double
    
    if (length(new_target) == 0) return()
    new_target <- clean_target(new_target[[1]])
    
    if (new_target == clean_target(input$target)) return()
    
    new_pos <- ifelse(input$pos == 'n', 'v', 'n')
    
    updateTextInput(session, 'target', value = new_target)
    updateSelectInput(session, 'pos', selected = new_pos)
    
    updateQueryString(
      make_url(new_pos, new_target, '', 'net', 'ja'),
      mode = 'push'
    )
    
  })
  
  # single click in lists
  observeEvent(input$list_click, {
  
    pair <- input$list_click
    
    if (is.null(pair) || pair == '') return()
    
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
    
    if (new_target == clean_target(input$target)) return()
    
    new_pos <- ifelse(input$pos == 'n', 'v', 'n')
    
    updateTextInput(session, 'target', value = new_target)
    updateSelectInput(session, 'pos', selected = new_pos)
    
    updateQueryString(
      make_url(new_pos, new_target, '', 'list', 'ja'),
      mode = 'push'
    )
    
  })
}

shinyApp(ui, server)

# make a mini corpus for shiny app
# cut off approx. 1 M lines at the document boundary
# M365 Copilot generated this code

# library(dplyr)
# 
# df <- read_tsv(here('data', 'corp_coca_sample.tsv'))
# 
# target_row <- min(1000000, nrow(df))
# target_id <- df$doc_id[target_row]
# 
# cut_row <- df |>
#   mutate(row = row_number()) |>
#   filter(row >= target_row, doc_id != target_id) |>
#   slice(1) |>
#   pull(row)
# 
# if (length(cut_row) == 0) {
#   cut_row <- nrow(df)
# } else {
#   cut_row <- cut_row - 1
# }
# 
# df_subset <- df[1:cut_row, ]
# 
# write_tsv(df_subset, here('data', 'corp_coca_short.tsv'))
# saveRDS(df_subset, here('data', 'corp_coca_short.rds'))

# convert from tsv file to rds file for shiny app
# to compress the files

# df <- read_tsv(here('data', 'col_verb_coca.tsv'))
# saveRDS(df, here('data', 'col_verb_coca.rds'))
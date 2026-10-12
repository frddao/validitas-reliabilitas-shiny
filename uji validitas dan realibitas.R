library(shiny)
library(psych)
library(readxl)
library(writexl)
library(DT)
library(shinyjs)


# ================= HELPER =================
to_numeric_safe <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

read_any <- function(path, name) {
  if (grepl("\\.csv$", name, ignore.case = TRUE)) {
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    df <- read_excel(path)
    as.data.frame(df, check.names = FALSE)
  }
}

# ================= UI =================
ui <- fluidPage(
  useShinyjs(),
  
  tags$style(HTML("

body {
  background-color:#f4f6f9;
  font-family: 'Segoe UI', sans-serif;
}

h3{
  font-weight:600;
  margin-top:5px;
}

.panel-card{
  background:white;
  border-radius:10px;
  padding:18px;
  margin-bottom:20px;
  box-shadow:0 2px 6px rgba(0,0,0,0.08);
}

.panel-info{
  background:#f8fbff;
  border-left:4px solid #2C7BE5;
  padding:10px 12px;
  margin-bottom:10px;
  font-size:13px;
  color:#444;
}

.sidebar-box{
  background:white;
  border-radius:10px;
  padding:18px;
  box-shadow:0 2px 6px rgba(0,0,0,0.08);
}

.btn-primary{
  background-color:#2C7BE5;
  border:none;
}

table.dataTable td{
  font-size:12px;
  padding:4px !important;
}

table.dataTable th{
  font-size:12px;
}

table.dataTable thead tr.filter-row{
  background:#f1f3f5;
}

table.dataTable select{
  width:100%;
  font-size:11px;
  border-radius:4px;
}

.dataTables_wrapper .dataTables_paginate .paginate_button{
  padding:2px 6px;
}

")),
  
  titlePanel(
    div(
      style="font-weight:600;",
      "Dashboard Analisis Validitas & Reliabilitas Instrumen"
    )
  ),
  
  fluidRow(
    
    # ================= SIDEBAR =================
    column(
      4,
      id = "sidebar_all",
      div(class="sidebar-box",
          
          helpText(
            tags$b("Langkah penggunaan:"),
            tags$ol(
              tags$li("Upload data kuesioner"),
              tags$li("Pilih huruf awal item"),
              tags$li("Assign item ke unsur"),
              tags$li("Klik Analyze"),
              tags$li("Download hasil")
            )
          )),
      
      fileInput("file", "Upload Data (CSV/Excel)", accept = c(".csv", ".xlsx")),
      selectInput("item_prefix", "Huruf Awal Nama Item", choices = NULL),
      
      textInput("unsur_names", "Daftar unsur (pisahkan dengan koma)",
                value = "Unsur 1, Unsur 2, Unsur 3"),
      
      fileInput("map_file", "Upload Mapping (CSV/Excel)", accept = c(".csv", ".xlsx")),
      
      selectizeInput("assign_to", "Assign item ke unsur:", choices = NULL),
      
      selectizeInput(
        "selected_items",
        "Pilih Item",
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Ketik nama item...",
          plugins = list("remove_button")
        )
      ),
      
      tags$br(),
      
      tags$div(
        style = "display:flex; flex-direction:column; gap:8px;",
        
        tags$div(
          style="display:flex; align-items:center; gap:12px;",
          actionButton("select_all", "Select All", width="140px"),
          tags$small(style="color:#777;",
                     "Memilih seluruh item")
        ),
        
        tags$div(
          style="display:flex; align-items:center; gap:12px;",
          actionButton("assign_btn", "Assign", width="140px"),
          tags$small(style="color:#777;",
                     "Masukkan item ke unsur")
        ),
        
        tags$div(
          style="display:flex; align-items:center; gap:12px;",
          actionButton("unassign_btn", "Unassign", width="140px"),
          tags$small(style="color:#777;",
                     "Hapus item dari unsur")
        ),
        
        tags$div(
          style="display:flex; align-items:center; gap:12px;",
          actionButton("clear_all", "Clear", width="140px"),
          tags$small(style="color:#777;",
                     "Kosongkan pilihan")
        ),
        
        tags$div(
          style="display:flex; align-items:center; gap:12px;",
          actionButton("reset_map", "Reset Mapping", width="140px"),
          tags$small(style="color:#777;",
                     "Mulai ulang mapping")
        )
      ),
      
      tags$hr(),
      
      numericInput("alpha_level", "Nilai Alpha (α):",
                   value = 0.05, min = 0.001, max = 0.20, step = 0.01),
      
      actionButton("analyze", "Analyze", class = "btn-primary")
    ),
    
    
    # ================= RESULT PANEL =================
    column(
      8,
      id="result_panel",
      
      div(
        class="panel-card",
        
        h3("Mapping Item → Unsur"),
        DTOutput("mapping_table"),
        
        tags$br(),
        
        h4("Informasi Item Belum Di-Assign"),
        verbatimTextOutput("unassigned_info")
      ),
      
      fluidRow(
        
        column(
          6,
          div(
            class="panel-card",
            
            h3("Hasil Uji Validitas"),
            
            div(
              class="panel-info",
              "Uji validitas digunakan untuk menilai apakah setiap item pertanyaan mampu mengukur konstruk yang dimaksud.
          Item dinyatakan valid apabila nilai korelasi item (r hitung) ≥ r tabel, dan tidak valid apabila r hitung < r tabel."
            ),
            
            DTOutput("validity_table")
          )
        ),
        
        column(
          6,
          div(
            class="panel-card",
            
            h3("Reliabilitas"),
            
            div(
              class="panel-info",
              "Uji reliabilitas digunakan untuk menilai konsistensi internal antar item dalam suatu unsur menggunakan Cronbach’s Alpha.
          Instrumen dinyatakan reliabel apabila nilai Cronbach’s Alpha ≥ 0,70, sedangkan nilai di bawah 0,70 menunjukkan bahwa instrumen belum reliabel."
            ),
            
            DTOutput("alpha_table")
          )
        )
        
      ),
      
      tags$br(),
      
      div(
        class="panel-card",
        
        actionButton("edit_again","Edit Mapping"),
        tags$span("  "),
        downloadButton("download_results","Download Hasil (Excel)")
      )
    )
  )
)

server <- function(input, output, session) {
  
  #1 DATA
  data <- reactive({
    req(input$file)
    read_any(input$file$datapath, input$file$name)
  })
  
  #2 Update Pilihan Prefix
  observeEvent(data(), {
    prefixes <- unique(substr(names(data()), 1, 1))
    updateSelectInput(session, "item_prefix",
                      choices = c("all", sort(prefixes)),
                      selected = "all")
  })
  
  #3 Ambil Kolom Based on Prefix
  item_cols <- reactive({
    req(data())
    if (tolower(input$item_prefix) == "all") {
      names(data())
    } else {
      cols <- names(data())
      cols[startsWith(toupper(cols), toupper(input$item_prefix))]
    }
  })
  
  #4 Update pilihan item ke selectize
  observeEvent(item_cols(), {
    updateSelectizeInput(session, "selected_items",
                         choices = item_cols(),
                         server = TRUE)
  })
  
  unsur_list <- reactive({
    trimws(unlist(strsplit(input$unsur_names, ",")))
  })
  
  observeEvent(unsur_list(), {
    updateSelectizeInput(session, "assign_to",
                         choices = unsur_list(),
                         selected = unsur_list()[1])
  })
  
  #5 Mapping storage: data.frame(Indikator, Unsur) =====
  mapping <- reactiveVal(NULL)
  
  observeEvent(item_cols(), {
    mapping(data.frame(
      Indikator = item_cols(),
      Unsur = NA_character_,
      stringsAsFactors = FALSE
    ))
  })
  
  observeEvent(input$map_file, {
    req(input$map_file, item_cols())
    
    base_map <- data.frame(
      Indikator = item_cols(),
      Unsur = NA_character_,
      stringsAsFactors = FALSE
    )
    
    m <- read_any(input$map_file$datapath, input$map_file$name)
    nms <- names(m)
    
    ind_col <- which(tolower(nms) %in% c("indikator", "item", "kolom", "pertanyaan"))
    unsur_col <- which(tolower(nms) %in% c("unsur", "dimensi", "konstruk", "construct"))
    
    if (length(ind_col) == 0 || length(unsur_col) == 0) {
      if (ncol(m) < 2) {
        showNotification("File mapping minimal punya 2 kolom: kolom-1 Indikator, kolom-2 Unsur.", 
                         type = "error", duration = 8)
        return(NULL)
      }
      ind_col <- 1
      unsur_col <- 2
    } else {
      ind_col <- ind_col[1]
      unsur_col <- unsur_col[1]
    }
    
    if (is.numeric(ind_col) && ind_col == 1 && unsur_col == 2 &&
        !(tolower(nms[1]) %in% c("indikator", "item", "kolom", "pertanyaan") &&
          tolower(nms[2]) %in% c("unsur", "dimensi", "konstruk", "construct"))) {
      showNotification("Header mapping bebas. Dipakai urutan kolom: (1) Indikator, (2) Unsur.", 
                       type = "message", duration = 8)
    }
    
    map_up <- data.frame(
      Indikator = trimws(as.character(m[[ind_col[1]]])),
      Unsur = trimws(as.character(m[[unsur_col[1]]])),
      stringsAsFactors = FALSE
    )
    
    map_up <- map_up[nchar(map_up$Indikator) > 0 & nchar(map_up$Unsur) > 0, , drop = FALSE]
    
    base_map$Unsur <- map_up$Unsur[match(base_map$Indikator, map_up$Indikator)]
    mapping(base_map)
  })
  
  #6 Assign / Unassign
  observeEvent(input$assign_btn, {
    map <- mapping()
    map$Unsur[map$Indikator %in% input$selected_items] <- input$assign_to
    mapping(map)
  })
  
  observeEvent(input$unassign_btn, {
    map <- mapping()
    map$Unsur[map$Indikator %in% input$selected_items] <- NA
    mapping(map)
  })
  
  observeEvent(input$reset_map, {
    mapping(data.frame(
      Indikator = item_cols(),
      Unsur = NA_character_,
      stringsAsFactors = FALSE
    ))
  })
  
  observeEvent(input$select_all, {
    updateSelectizeInput(session, "selected_items",
                         selected = item_cols())
  })
  
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "selected_items",
                         selected = character(0))
  })
  
  output$mapping_table <- renderDT({
    req(mapping())
    datatable(mapping(), rownames = FALSE,
              options = list(pageLength = 10))
  })
  
  output$unassigned_info <- renderPrint({
    req(mapping())
    unassigned <- mapping()$Indikator[is.na(mapping()$Unsur)]
    cat("Jumlah item belum di-assign:", length(unassigned))
  })
  
  #7 Analysis
  result <- eventReactive(input$analyze, {
    req(mapping(), data())
    
    map2 <- mapping()[!is.na(mapping()$Unsur), ]
    split_items <- split(map2$Indikator, map2$Unsur)
    
    lapply(names(split_items), function(u) {
      df <- data()[, split_items[[u]], drop = FALSE]
      df <- as.data.frame(lapply(df, to_numeric_safe))
      
      a <- psych::alpha(df)
      
      n <- nrow(df)
      dfree <- n - 2
      alpha_val <- input$alpha_level
      t_crit <- qt(1 - alpha_val/2, dfree)
      r_tabel <- t_crit / sqrt(t_crit^2 + dfree)
      
      list(
        unsur = u,
        alpha = a$total$raw_alpha,
        n = n,
        stats = a$item.stats,
        r_tabel = r_tabel
      )
    })
  })
  
  #8 Validity table (dengan dropdown filter per kolom: Unsur & Keputusan)
  output$validity_table <- renderDT({
    req(result())
    
    valid_df <- do.call(rbind, lapply(result(), function(res) {
      data.frame(
        Unsur = res$unsur,
        Indikator = rownames(res$stats),
        Korelasi_Item = round(res$stats$r.drop, 4),
        r_Tabel = round(res$r_tabel, 4),
        Keputusan = ifelse(res$stats$r.drop >= res$r_tabel, "Valid", "Tidak Valid"),
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(
      valid_df,
      rownames = FALSE,
      extensions = "Responsive",
      options = list(
        autoWidth = TRUE,
        scrollX = FALSE,
        pageLength = 10,
        initComplete = JS(
          "function() {",
          "  var api = this.api();",
          "  var table = api.table();",
          "  var header = $(table.header());",
          "  header.find('tr.filter-row').remove();",
          "  var filterRow = $('<tr class=\"filter-row\"></tr>').appendTo(header);",
          "  header.find('tr:first th').each(function() {",
          "    filterRow.append('<th></th>');",
          "  });",
          "  api.columns().every(function(i) {",
          "    var column = this;",
          "    var cell = header.find('tr.filter-row th').eq(i);",
          "    var select = $('<select style=\"width:100%\"><option value=\"\">All</option></select>')",
          "      .appendTo(cell)",
          "      .on('change', function() {",
          "        var val = $.fn.dataTable.util.escapeRegex($(this).val());",
          "        column.search(val ? '^'+val+'$' : '', true, false).draw();",
          "      });",
          "    column.data().unique().sort().each(function(d) {",
          "      if(d !== null && d !== '') {",
          "        select.append('<option value=\"'+d+'\">'+d+'</option>');",
          "      }",
          "    });",
          "  });",
          "}"
        )
      )
    ) %>%
      formatStyle(
        'Keputusan',
        target = 'row',
        backgroundColor = styleEqual(
          c("Valid", "Tidak Valid"),
          c("#d4edda", "#f8d7da")
        ),
        color = styleEqual(
          c("Valid", "Tidak Valid"),
          c("#155724", "#721c24")
        )
      )
  })
  
  
  #9 Reliabilitas (NEW - was missing)
  output$alpha_table <- renderDT({
    req(result())
    
    alpha_df <- do.call(rbind, lapply(result(), function(res) {
      data.frame(
        Unsur = res$unsur,
        Jumlah_Item = nrow(res$stats),
        Responden = res$n,
        Cronbach_Alpha = round(res$alpha, 4),
        Keputusan = ifelse(res$alpha >= 0.70, "Reliabel", "Belum Reliabel"),
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(
      alpha_df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      )
    ) %>%
      formatStyle(
        'Keputusan',
        target = 'row',
        backgroundColor = styleEqual(
          c("Reliabel", "Belum Reliabel"),
          c("#d4edda", "#fff3cd")
        ),
        color = styleEqual(
          c("Reliabel", "Belum Reliabel"),
          c("#155724", "#856404")
        )
      )
  })
  
  observeEvent(input$analyze, {
    shinyjs::hide("sidebar_all")
    runjs("$('#result_panel').removeClass('col-sm-8').addClass('col-sm-12');")
  })
  
  observeEvent(input$edit_again, {
    shinyjs::show("sidebar_all")
    runjs("$('#result_panel').removeClass('col-sm-12').addClass('col-sm-8');")
  })
  
  #10 Download hasil excel
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("Hasil_Uji_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(result())
      
      validitas <- do.call(rbind, lapply(result(), function(res) {
        data.frame(
          Unsur = res$unsur,
          Indikator = rownames(res$stats),
          Korelasi_Item = round(res$stats$r.drop, 4),
          r_Tabel = round(res$r_tabel, 4),
          Keputusan = ifelse(
            res$stats$r.drop >= res$r_tabel,
            "Valid",
            "Tidak Valid"
          )
        )
      }))
      
      reliabilitas <- do.call(rbind, lapply(result(), function(res) {
        data.frame(
          Unsur = res$unsur,
          Responden = res$n,
          Cronbach_Alpha = round(res$alpha, 4),
          Keputusan = ifelse(res$alpha >= 0.70, "Reliabel", "Belum Reliabel")
        )
      }))
      
      write_xlsx(list(
        Validitas = validitas,
        Reliabilitas = reliabilitas
      ), file)
    }
  )
}

shinyApp(ui, server)
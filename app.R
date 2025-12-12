options(shiny.error = browser)
options(shiny.trace = TRUE)
print("APP STARTING ---->")
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Avoid optional heavy dependencies (raster, terra, sf)
Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false")
Sys.setenv("PKG_CONFIG_PATH" = "")

# Prevent leaflet from trying to install sf/raster
install.packages("leaflet", dependencies = FALSE)

# Install normal packages
install.packages(c(
  "shiny", "RPostgres", "DBI", "dplyr", "ggplot2", "plotly",
  "DT", "bslib", "shinyWidgets", "thematic", 
  "shinyjs", "lubridate", "shinycssloaders", "tools"
))
install.packages("leaflet", dependencies = FALSE)





###############################################################################
# PACKAGES
###############################################################################
library(shiny)
library(RPostgres)
library(DBI)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(bslib)
library(shinyWidgets)
library(thematic)
library(shinyjs)
library(lubridate)
library(shinycssloaders)

thematic_shiny(font = "auto")

options(shiny.host = "0.0.0.0")
options(shiny.port = as.numeric(Sys.getenv("PORT", 3838)))


###############################################################################
# THEME + CSS
###############################################################################
light_theme <- bs_theme(
  bootswatch = "cosmo",
  bg = "#F4F7FC",
  fg = "#212529",
  primary = "#007bff",
  secondary = "#17A589",
  base_font = font_google("Poppins")
)
bs_global_set(light_theme)

custom_css <- "
  body { background: linear-gradient(135deg, #E8F1F8, #FDFBF4); }

  .chart-card { 
    background:#154360; color:white; border-radius:16px; padding:12px;
    box-shadow:0 4px 12px rgba(0,0,0,0.25); animation: fadeInUp 0.7s ease;
  }

  .kpi-box {
    font-size:20px;font-weight:bold;color:white;height:80px;
    border-radius:15px;padding:16px;
    box-shadow:0px 4px 18px rgba(0,0,0,0.35);
    display:flex;align-items:center;justify-content:center;
  }

  #btn_add_modal {
    background-color: #D4F6CC !important;
    border-color: #A8D49A !important;
    color: black !important;
    font-size: 18px;
    font-weight: bold;
  }

  #btn_edit_modal {
    background-color: #FFE7A0 !important;
    border-color: #E6C875 !important;
    color: black !important;
    font-size: 18px;
    font-weight: bold;
  }

  #btn_approve {
    background-color: #0074C8 !important;
    color: white !important;
    font-size: 18px;
    font-weight: bold;
  }

  #btn_reject {
    background-color: #333333 !important;
    color: #FF4F70 !important;
    font-size: 18px;
    font-weight: bold;
  }

  #btn_delete {
    background-color: #FFB3B3 !important;
    border-color: #E67A7A !important;
    color: black !important;
    font-size: 18px;
    font-weight: bold;
  }
"

###############################################################################
# DATABASE
###############################################################################
DB_HOST <- "ep-fancy-bread-a1fgho5h.ap-southeast-1.aws.neon.tech"
DB_PORT <- 5432
DB_NAME <- "neondb"
DB_USER <- "neondb_owner"
DB_PASS <- "npg_Jcguq90xmtzO"

connect_db <- function() {
  tryCatch({
    dbConnect(
      Postgres(), 
      dbname = DB_NAME, 
      host = DB_HOST, 
      port = DB_PORT,
      user = DB_USER, 
      password = DB_PASS, 
      sslmode = "require"
    )
  }, error = function(e) { NULL })
}

###############################################################################
# UI  (LOGIN + DASHBOARD + UPDATED ANALYTICS GRID)
###############################################################################
ui <- tagList(
  useShinyjs(),
  tags$head(tags$style(HTML(custom_css))),
  
  # LOGIN SCREEN
  div(
    id = "login_wrapper",
    div(
      style="max-width:400px;margin:100px auto;background:white;padding:25px;border-radius:10px;",
      h3("🔐 QUANTOX TECHNOLOGIES", align="center"),
      textInput("user_id","Username"),
      passwordInput("user_pass","Password"),
      actionButton("login_btn","Login", class="btn btn-primary w-100"),
      br(), br(),
      textOutput("login_error")
    )
  ),
  
  # MAIN DASHBOARD
  hidden(
    div(
      id = "main_dashboard",
      page_sidebar(
        title = "📊 QUANTOX TECHNOLOGIES DASHBOARD",
        
        sidebar = sidebar(
          div(class="alert alert-primary", textOutput("current_user_role")),
          
          selectInput("filter_category","Project Category",choices="All"),
          selectInput("filter_project","Project",choices="All"),
          selectInput("filter_status","Project Status",choices="All"),
          selectInput("filter_payment","Payment Status",choices="All"),
          
          actionButton("refresh_data","🔁 Sync", class="btn btn-info w-100"),
          downloadButton("download_csv","⬇ Export CSV", class="btn btn-success w-100"),
          actionButton("logout_btn","Logout", class="btn btn-danger w-100")
        ),
        
        navset_card_underline(
          
          ############ ANALYTICS TAB ############
          nav_panel(
            "Analytics",
            
            # KPIs
            layout_columns(
              div(class="kpi-box", style="background:#512DA8;", "📌 Total: ", textOutput("kpi_total")),
              div(class="kpi-box", style="background:#28B463;", "✔ Completed: ", textOutput("kpi_completed")),
              div(class="kpi-box", style="background:#E67E22;", "⏳ Pending: ", textOutput("kpi_pending"))
            ),
            
            # ROW 1: PROJECT CATEGORY + PROJECT STATUS  (swap केलेले)
            layout_columns(
              card(class="chart-card", card_header("📦 Project Category"), withSpinner(plotlyOutput("plot_category"), type=6)),
              card(class="chart-card", card_header("📊 Project Status"),   withSpinner(plotlyOutput("plot_progress"), type=6))
            ),
            
            # ROW 2: FULL-WIDTH DAYS TAKEN
            layout_columns(
              card(class="chart-card", width = 12,
                   card_header("⏱ Days Taken (End - Start)"),
                   withSpinner(plotlyOutput("plot_days_taken"), type=6))
            ),
            
            # ROW 3: PAYMENT + START MONTH
            layout_columns(
              card(class="chart-card", card_header("💰 Payment Status"),     withSpinner(plotlyOutput("plot_payment"), type=6)),
              card(class="chart-card", card_header("📈 Start Month Trend"),  withSpinner(plotlyOutput("plot_start"), type=6))
            ),
            
            # ROW 4: END MONTH + SUBMISSION DATE
            layout_columns(
              card(class="chart-card", card_header("📅 End Month"),          withSpinner(plotlyOutput("plot_end"), type=6)),
              card(class="chart-card", card_header("📤 Submission Date"),    withSpinner(plotlyOutput("plot_submission"), type=6))
            )
          ),
          
          ############ MAP TAB ############
          nav_panel(
            "Live Map",
            card(
              class="chart-card",
              leafletOutput("map_view", height="550px")
            )
          ),
          
          ############ DATABASE TAB ############
          nav_panel(
            "Database",
            
            actionButton("btn_add_modal","➕ Add",class="btn btn-success w-100"),
            actionButton("btn_edit_modal","✏ Edit",class="btn btn-warning w-100"),
            actionButton("btn_approve","✅ Approve",class="btn btn-primary w-100"),
            actionButton("btn_reject","❌ Reject",class="btn btn-secondary w-100"),
            actionButton("btn_delete","🗑 Delete",class="btn btn-danger w-100"),
            
            br(), br(),
            DTOutput("project_tracker")
          )
        )
      )
    )
  )
)
###############################################################################
# SERVER
###############################################################################
server <- function(input, output, session) {
  
  user_auth <- reactiveValues(logged=FALSE, role=NULL, username=NULL)
  rv <- reactiveValues(data=data.frame(), approvals=data.frame())
  login_error <- reactiveVal("")
  
  ###############################################################
  # LOAD APPROVAL CSV (NO DB CHANGE)
  ###############################################################
  load_approvals <- function() {
    if (file.exists("approvals.csv")) {
      df <- tryCatch(read.csv("approvals.csv", stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) {
        rv$approvals <- df
      } else {
        rv$approvals <- data.frame(
          sr_no = integer(),
          approval_status = character(),
          approved_by = character(),
          approved_date = character(),
          stringsAsFactors = FALSE
        )
      }
    } else {
      rv$approvals <- data.frame(
        sr_no = integer(),
        approval_status = character(),
        approved_by = character(),
        approved_date = character(),
        stringsAsFactors = FALSE
      )
    }
  }
  
  ###############################################################
  # USER LOGIN
  ###############################################################
  observeEvent(input$login_btn, {
    
    con <- connect_db()
    if (is.null(con)) {
      login_error("❌ DB connection failed.")
      return()
    }
    
    user <- dbGetQuery(con,
                       "SELECT * FROM app_users WHERE username=$1 AND password=$2",
                       params = list(input$user_id, input$user_pass)
    )
    dbDisconnect(con)
    
    if (nrow(user) != 1) {
      login_error("❌ Wrong Username or Password")
      return()
    }
    
    # ASSIGN ROLE
    r <- tolower(user$role[1])
    if (r == "admin") user_auth$role <- "Admin"
    else if (r == "manager") user_auth$role <- "Manager"
    else user_auth$role <- "Viewer"
    
    user_auth$username <- input$user_id
    user_auth$logged <- TRUE
    
    shinyjs::hide("login_wrapper")
    shinyjs::show("main_dashboard")
    
    # ROLE BASED PERMISSION
    if (user_auth$role == "Viewer") {
      shinyjs::hide("btn_add_modal")
      shinyjs::hide("btn_edit_modal")
      shinyjs::hide("btn_delete")
      shinyjs::hide("btn_approve")
      shinyjs::hide("btn_reject")
      shinyjs::hide("refresh_data")
      shinyjs::hide("download_csv")
    }
    if (user_auth$role == "Manager") {
      shinyjs::show("btn_add_modal")
      shinyjs::show("btn_edit_modal")
      shinyjs::show("btn_approve")
      shinyjs::show("btn_reject")
      shinyjs::hide("btn_delete")
    }
    if (user_auth$role == "Admin") {
      shinyjs::show("btn_add_modal")
      shinyjs::show("btn_edit_modal")
      shinyjs::show("btn_delete")
      shinyjs::hide("btn_approve")   # ✅ ONLY MANAGER CAN SEE
      shinyjs::hide("btn_reject")    # ✅ ONLY MANAGER CAN SEE
    }
    
    load_data()
    load_approvals()
  })
  
  output$current_user_role <- renderText({
    paste("Logged in as:", user_auth$role)
  })
  
  ###############################################################
  # LOAD DATA FROM NEON DATABASE
  ###############################################################
  load_data <- function() {
    
    con <- connect_db()
    if (is.null(con)) return()
    
    rv$data <- dbGetQuery(con, "SELECT * FROM project_tracker ORDER BY sr_no")
    dbDisconnect(con)
    
    if (nrow(rv$data) == 0) return()
    
    updateSelectInput(session, "filter_category",
                      choices = c("All", sort(unique(rv$data$category))),
                      selected = "All"
    )
    
    updateSelectInput(session, "filter_project",
                      choices = c("All", unique(rv$data$project)),
                      selected = "All"
    )
    
    updateSelectInput(session, "filter_status",
                      choices = c("All", unique(rv$data$project_status)),
                      selected = "All"
    )
    
    updateSelectInput(session, "filter_payment",
                      choices = c("All", unique(rv$data$payment_status)),
                      selected = "All"
    )
  }
  
  observeEvent(input$refresh_data, load_data)
  
  ###############################################################
  # FILTERED DATA (MERGED WITH APPROVAL CSV) + DAYS_TAKEN
  ###############################################################
  filtered_data <- reactive({
    df <- rv$data
    if (nrow(df) == 0) return(df)
    
    # MERGE APPROVAL INFO
    if (!is.null(rv$approvals) && nrow(rv$approvals) > 0) {
      df <- dplyr::left_join(df, rv$approvals, by = "sr_no")
    }
    
    if (!is.null(input$filter_category) && input$filter_category != "All") {
      df <- df[df$category == input$filter_category, ]
    }
    
    if (!is.null(input$filter_project) && input$filter_project != "All") {
      df <- df[df$project == input$filter_project, ]
    }
    
    if (!is.null(input$filter_status) && input$filter_status != "All") {
      df <- df[df$project_status == input$filter_status, ]
    }
    
    if (!is.null(input$filter_payment) && input$filter_payment != "All") {
      df <- df[df$payment_status == input$filter_payment, ]
    }
    
    # ⭐ NEW COLUMN: किती दिवस लागले (end_month - start_month)
    df <- df %>%
      mutate(
        start_month = as.Date(start_month),
        end_month   = as.Date(end_month),
        days_taken  = ifelse(
          !is.na(start_month) & !is.na(end_month),
          as.integer(end_month - start_month),
          NA_integer_
        )
      )
    
    df
  })
  
  ###############################################################
  # KPIs
  ###############################################################
  output$kpi_total <- renderText(nrow(filtered_data()))
  
  output$kpi_completed <- renderText(
    sum(filtered_data()$project_status == "Done", na.rm = TRUE)
  )
  
  output$kpi_pending <- renderText(
    sum(filtered_data()$project_status != "Done", na.rm = TRUE)
  )
  ###############################################################
  # CHARTS  (Dynamic font size + swapped + full-width days_taken)
  ###############################################################
  
  # Helper for dynamic text size & position
  dynamic_font <- function(df) {
    if (nrow(df) == 1) {
      list(size = 26, pos = "inside")
    } else {
      list(size = 14, pos = "outside")
    }
  }
  
  # ==== PROJECT STATUS BAR (Row 1, left) ====
  output$plot_progress <- renderPlotly({
    df <- filtered_data() %>% count(project_status)
    if (nrow(df) == 0) return(NULL)
    
    f <- dynamic_font(df)
    bar_colors <- c("red", "green", "blue", "yellow")
    
    plot_ly(
      df,
      x = ~project_status,
      y = ~n,
      type = "bar",
      marker = list(color = bar_colors[1:nrow(df)]),
      text = ~project_status,
      textposition = f$pos,
      textfont = list(size = f$size)
    ) %>% 
      layout(
        title = list(text = "Project Status",
                     font = list(size = f$size + 4)),
        xaxis = list(title = "Status",
                     tickfont = list(size = f$size)),
        yaxis = list(title = "Projects",
                     tickfont = list(size = f$size)),
        margin = list(t = 60, b = 80)
      )
  })
  
  # ==== PROJECT CATEGORY BAR (Row 1, right) ====
  output$plot_category <- renderPlotly({
    df <- filtered_data() %>% count(category)
    if (nrow(df) == 0) return(NULL)
    
    f <- dynamic_font(df)
    bar_colors <- c("#F1C40F", "#2ECC71", "#E74C3C",
                    "#8E44AD", "pink", "blue", "maroon")
    
    plot_ly(
      df,
      x = ~category,
      y = ~n,
      type = "bar",
      marker = list(color = bar_colors[1:nrow(df)]),
      text = ~category,
      textposition = f$pos,
      textfont = list(size = f$size)
    ) %>% 
      layout(
        title  = list(text = "Project Category",
                      font = list(size = f$size + 4)),
        xaxis  = list(title = "Category",
                      tickfont = list(size = f$size)),
        yaxis  = list(title = "Projects",
                      tickfont = list(size = f$size)),
        margin = list(t = 60, b = 80)
      )
  })
  
  
  output$plot_days_taken <- renderPlotly({
    df <- filtered_data()
    if (!"days_taken" %in% names(df)) return(NULL)
    
    df <- df %>%
      filter(!is.na(days_taken)) %>%
      arrange(desc(days_taken))
    
    if (nrow(df) == 0) return(NULL)
    
    df$project <- factor(df$project, levels = df$project)
    
    # --- AUTO SIZE SETTINGS WHEN ONLY ONE PROJECT ---
    is_single <- nrow(df) == 1
    text_size  <- if (is_single) 28 else 14          # bar text
    label_size <- if (is_single) 22 else 12          # x-axis label size
    text_pos   <- if (is_single) "inside" else "outside"
    text_color <- if (is_single) "white" else "black"
    
    plot_ly(
      df,
      x = ~project,
      y = ~days_taken,
      type = "bar",
      marker = list(color = "rgba(0,153,0,1)"),
      
      # TEXT ON BAR
      text = ~paste("Days:", days_taken),
      textposition = text_pos,
      textfont = list(size = text_size, color = text_color),
      
      hoverinfo = "text",
      hovertext = ~paste(
        "<b>Project:</b> ", project, "<br>",
        "<b>Days:</b> ", days_taken
      )
    ) %>%
      layout(
        xaxis = list(
          title = "Project",
          tickangle = if (is_single) 0 else -60,   # ⭐ FIX: single → straight name
          tickfont = list(size = label_size)
        ),
        yaxis = list(title = "Days Taken"),
        margin = list(t = 70, b = 160)
      )
  })
  
  
  
  
  # ==== PAYMENT PIE (Row 3, left) ====
  output$plot_payment <- renderPlotly({
    df <- filtered_data() %>% count(payment_status)
    if (nrow(df) == 0) return(NULL)
    
    f <- dynamic_font(df)
    pie_colors <- c("#F1C40F", "#2ECC71", "#E74C3C", "#8E44AD")
    
    plot_ly(
      df,
      labels = ~payment_status,
      values = ~n,
      type   = "pie",
      marker = list(colors = pie_colors[1:nrow(df)]),
      textfont = list(size = f$size)
    ) %>%
      layout(
        title  = list(text = "Payment Status",
                      font = list(size = f$size + 4)),
        margin = list(t = 60)
      )
  })
  
  # ==== START DATE TREND (Row 3, right) ====
  output$plot_start <- renderPlotly({
    df <- filtered_data()
    
    df <- df %>%
      mutate(start_month = as.Date(start_month)) %>%
      filter(!is.na(start_month)) %>%
      mutate(date_label = format(start_month, "%d-%m-%Y")) %>%
      count(date_label)
    
    if (nrow(df) == 0) return(NULL)
    
    f <- dynamic_font(df)
    
    plot_ly(
      df,
      x    = ~date_label,
      y    = ~n,
      type = "scatter",
      mode = "lines+markers",
      line   = list(color = "#2ECC71", width = ifelse(nrow(df)==1, 6, 3)),
      marker = list(size  = ifelse(nrow(df) == 1, 14, 8),
                    color = "#E74C3C"),
      text  = ~paste("Date:", date_label, "<br>Projects:", n),
      hoverinfo = "text"
    ) %>%
      layout(
        title  = list(text = "Start Date Trend",
                      font = list(size = f$size + 6)),
        margin = list(t = 80, b = 80),
        xaxis  = list(title = "Start Date",
                      tickfont = list(size = f$size)),
        yaxis  = list(title = "Projects",
                      tickfont = list(size = f$size))
      )
  })
  
  # ==== END DATE TREND (Row 4, left) ====
  output$plot_end <- renderPlotly({
    df <- filtered_data()
    
    df <- df %>%
      mutate(end_month = as.Date(end_month)) %>%
      filter(!is.na(end_month)) %>%
      mutate(date_label = format(end_month, "%d-%m-%Y")) %>%
      count(date_label)
    
    if (nrow(df) == 0) return(NULL)
    
    f <- dynamic_font(df)
    
    plot_ly(
      df,
      x    = ~date_label,
      y    = ~n,
      type = "scatter",
      mode = "lines+markers",
      line   = list(color = "#2ECC71", width = ifelse(nrow(df)==1, 6, 3)),
      marker = list(size  = ifelse(nrow(df) == 1, 14, 8),
                    color = "#C2185B"),
      text  = ~paste("Date:", date_label, "<br>Projects:", n),
      hoverinfo = "text"
    ) %>%
      layout(
        title  = list(text = "End Date Trend",
                      font = list(size = f$size + 6)),
        margin = list(t = 80, b = 80),
        xaxis  = list(title = "End Date",
                      tickfont = list(size = f$size)),
        yaxis  = list(title = "Projects",
                      tickfont = list(size = f$size))
      )
  })
  
  # ==== SUBMISSION DATE TREND (Row 4, right) ====
  output$plot_submission <- renderPlotly({
    df <- filtered_data()
    
    df <- df %>%
      mutate(submission_date = as.Date(submission_date)) %>%
      filter(!is.na(submission_date)) %>%
      mutate(date_label = format(submission_date, "%d-%m-%Y")) %>%
      count(date_label)
    
    if (nrow(df) == 0) return(NULL)
    
    f <- dynamic_font(df)
    
    plot_ly(
      df,
      x = ~date_label,
      y = ~n,
      type = "scatter",
      mode = "lines+markers",
      line   = list(color = "#2ECC71",
                    width = ifelse(nrow(df) == 1, 6, 3)),
      marker = list(size = ifelse(nrow(df) == 1, 14, 8),
                    color = "#F1C40F"),
      text = ~paste("Date:", date_label, "<br>Projects:", n),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(text = "Submission Date Trend",
                     font = list(size = f$size + 6)),
        margin = list(t = 60, b = 80),
        xaxis = list(title = "Submission Date",
                     tickfont = list(size = f$size)),
        yaxis = list(title = "Projects",
                     tickfont = list(size = f$size))
      )
  })
  # MAP
  ###############################################################
  output$map_view <- renderLeaflet({
    df <- filtered_data()
    
    df <- df %>%
      mutate(latitude  = as.numeric(latitude),
             longitude = as.numeric(longitude)) %>%
      filter(!is.na(latitude), !is.na(longitude))
    
    leaflet(df) %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery",   group = "Satellite") %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Street View") %>%
      addProviderTiles("Esri.WorldTopoMap",   group = "Terrain") %>%
      addProviderTiles("CartoDB.Positron",    group = "Positron Light") %>%
      addProviderTiles("CartoDB.DarkMatter",  group = "Dark Mode") %>%
      addProviderTiles("Stadia.StamenWatercolor", group = "Watercolor") %>%
      addCircleMarkers(
        lng  = ~longitude,
        lat  = ~latitude,
        popup = ~project,
        radius = 7,
        fillColor = "red",
        color     = "red",
        stroke = TRUE,
        fillOpacity = 0.9,
        group = "Projects"
      ) %>%
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap", "Satellite", "Street View",
          "Terrain", "Positron Light", "Dark Mode", "Watercolor"
        ),
        overlayGroups = c("Projects"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = mean(df$longitude), lat = mean(df$latitude), zoom = 5)
  })
  
  
  ###############################################################
  # TABLE + EXPORT
  ###############################################################
  output$project_tracker <- renderDT({
    df <- filtered_data()
    
    # ⭐ Convert column names → First letter capital
    new_names <- tools::toTitleCase(colnames(df))
    
    datatable(
      df,
      colnames = new_names,
      selection = "single",
      options = list(pageLength = 10)
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("project_data_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
  
  ###############################################################
  # ADD (CREATE)
  ###############################################################
  observeEvent(input$btn_add_modal, {
    req(user_auth$role %in% c("Admin", "Manager"))
    
    next_sr <- if (!is.null(rv$data) &&
                   nrow(rv$data) > 0 &&
                   !is.null(rv$data$sr_no)) {
      suppressWarnings(max(as.numeric(rv$data$sr_no), na.rm = TRUE) + 1)
    } else {
      1
    }
    
    showModal(modalDialog(
      title = "New Project Entry",
      p("Note: SR No must be unique for each project."),
      numericInput("new_sr_no", "SR No (ID)", value = next_sr, min = 1, step = 1),
      textInput("new_project", "Project Name"),
      selectInput("new_category", "Category",
                  choices = c("Web Development", "GIS",
                              "Digital Marketing", "App Development"),
                  multiple = TRUE),
      numericInput("new_progress", "Progress", value = 0, min = 0, max = 100),
      selectInput("new_status", "Status",
                  choices = c("Ongoing", "Done", "On Hold")),
      selectInput("new_payment", "Payment",
                  choices = c("Pending", "Partial",
                              "Done", "Not Applicable")),
      textInput("new_location", "City Name"),
      numericInput("new_lat",  "Lat",  value = 0),
      numericInput("new_long", "Long", value = 0),
      dateInput("new_start", "Start"),
      dateInput("new_end",   "End"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_save_new", "Save Entry", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$btn_save_new, {
    req(
      input$new_project,
      input$new_sr_no,
      user_auth$role %in% c("Admin", "Manager")
    )
    
    con <- connect_db()
    if (!is.null(con)) {
      cats <- paste(input$new_category, collapse = ", ")
      
      query <- "INSERT INTO project_tracker(
        sr_no, project, categories, progress, project_status, payment_status,
        client_location, latitude, longitude, start_month, end_month
      ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)"
      
      tryCatch({
        dbExecute(con, query, list(
          as.integer(input$new_sr_no),
          input$new_project,
          cats,
          input$new_progress,
          input$new_status,
          input$new_payment,
          input$new_location,
          input$new_lat,
          input$new_long,
          as.character(input$new_start),
          as.character(input$new_end)
        ))
        showNotification("New project added.", type = "message")
        removeModal()
        load_data()
      }, error = function(e) {
        showNotification(paste("Error inserting project:", e$message),
                         type = "error")
      })
      
      dbDisconnect(con)
    }
  })
  
  ###############################################################
  # EDIT (UPDATE)
  ###############################################################
  observeEvent(input$btn_edit_modal, {
    req(user_auth$role %in% c("Admin"))
    req(nrow(rv$data) > 0)
    
    showModal(modalDialog(
      title = "✏ Edit Project",
      selectInput(
        "edit_sr_no", "Select SR No / Project",
        choices = setNames(rv$data$sr_no,
                           paste(rv$data$sr_no, "-", rv$data$project))
      ),
      uiOutput("edit_form"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit", "Update", class = "btn btn-success")
      )
    ))
  })
  
  output$edit_form <- renderUI({
    req(input$edit_sr_no)
    row <- rv$data[rv$data$sr_no == input$edit_sr_no, ]
    if (nrow(row) == 0) return(NULL)
    
    tagList(
      textInput("edit_project",  "Project Name",  value = row$project),
      textInput("edit_category", "Category",      value = row$category),
      numericInput("edit_progress", "Progress (%)", value = row$progress),
      textInput("edit_status",   "Project Status",   value = row$project_status),
      textInput("edit_payment",  "Payment Status",   value = row$payment_status),
      textInput("edit_location", "Location",         value = row$location),
      numericInput("edit_latitude",  "Latitude",  value = row$latitude),
      numericInput("edit_longitude", "Longitude", value = row$longitude),
      dateInput("edit_start_month", "Start Month",
                value = as.Date(row$start_month)),
      dateInput("edit_end_month",   "End Month",
                value = as.Date(row$end_month)),
      dateInput("edit_submission_date", "Submission Date",
                value = as.Date(row$submission_date))
    )
  })
  
  observeEvent(input$confirm_edit, {
    req(input$edit_sr_no)
    
    con <- connect_db()
    if (is.null(con)) {
      showNotification("DB connection failed while updating.", type = "error")
      return()
    }
    
    dbExecute(
      con,
      "UPDATE project_tracker SET
        project = $1,
        category = $2,
        progress = $3,
        project_status = $4,
        payment_status = $5,
        location = $6,
        latitude = $7,
        longitude = $8,
        start_month = $9,
        end_month   = $10,
        submission_date = $11
       WHERE sr_no = $12",
      params = list(
        input$edit_project,
        input$edit_category,
        input$edit_progress,
        input$edit_status,
        input$edit_payment,
        input$edit_location,
        input$edit_latitude,
        input$edit_longitude,
        as.character(input$edit_start_month),
        as.character(input$edit_end_month),
        as.character(input$edit_submission_date),
        input$edit_sr_no
      )
    )
    dbDisconnect(con)
    removeModal()
    load_data()
    showNotification("✔ Project updated.", type = "message")
  })
  
  ###############################################################
  # APPROVE / REJECT  (CSV आधारित, DB बदल नाही)
  ###############################################################
  observeEvent(input$btn_approve, {
    req(user_auth$role == "Manager")   # ✅ फक्त Manager साठी
    
    s <- input$project_tracker_rows_selected
    if (length(s) != 1) {
      showNotification("Please select one row from table.", type = "warning")
      return()
    }
    
    df <- filtered_data()
    sr <- df$sr_no[s]
    
    new_row <- data.frame(
      sr_no          = as.integer(sr),
      approval_status = "Approved",
      approved_by     = user_auth$username,
      approved_date   = as.character(Sys.Date()),
      stringsAsFactors = FALSE
    )
    
    idx <- which(rv$approvals$sr_no == sr)
    if (length(idx) > 0) {
      rv$approvals[idx, ] <- new_row
    } else {
      rv$approvals <- rbind(rv$approvals, new_row)
    }
    
    write.csv(rv$approvals, "approvals.csv", row.names = FALSE)
    showNotification("Project approved.", type = "message")
  })
  
  observeEvent(input$btn_reject, {
    req(user_auth$role == "Manager")   # ✅ फक्त Manager साठी
    
    s <- input$project_tracker_rows_selected
    if (length(s) != 1) {
      showNotification("Please select one row from table.", type = "warning")
      return()
    }
    
    df <- filtered_data()
    sr <- df$sr_no[s]
    
    new_row <- data.frame(
      sr_no          = as.integer(sr),
      approval_status = "Rejected",
      approved_by     = user_auth$username,
      approved_date   = as.character(Sys.Date()),
      stringsAsFactors = FALSE
    )
    
    idx <- which(rv$approvals$sr_no == sr)
    if (length(idx) > 0) {
      rv$approvals[idx, ] <- new_row
    } else {
      rv$approvals <- rbind(rv$approvals, new_row)
    }
    
    write.csv(rv$approvals, "approvals.csv", row.names = FALSE)
    showNotification("Project rejected.", type = "warning")
  })
  
  ###############################################################
  # DELETE
  ###############################################################
  observeEvent(input$btn_delete, {
    req(user_auth$role == "Admin")
    req(rv$data)
    
    if (nrow(rv$data) == 0) {
      showNotification("No data.", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "Delete Project",
      div(class = "text-danger", icon("triangle-exclamation"),
          "Warning: Deletion is permanent."), br(),
      radioButtons("delete_method", "Delete Method:",
                   choices = c("Specific Entry" = "id")),
      uiOutput("delete_input_ui"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_confirm_delete_search", "Confirm Delete",
                     class = "btn-danger")
      )
    ))
  })
  
  output$delete_input_ui <- renderUI({
    req(input$delete_method)
    if (input$delete_method == "id") {
      proj_choices <- setNames(rv$data$sr_no,
                               paste0(rv$data$project,
                                      " [ID: ", rv$data$sr_no, "]"))
      pickerInput("delete_val_id", "Select Project to Delete:",
                  choices = proj_choices,
                  options = list(`live-search` = TRUE,
                                 title = "Type name to search..."),
                  width = "100%")
    } else {
      unique_names <- sort(unique(rv$data$project))
      pickerInput("delete_val_name",
                  "Select Project Name (Deletes All Matches):",
                  choices = unique_names,
                  options = list(`live-search` = TRUE,
                                 title = "Select Name..."),
                  width = "100%")
    }
  })
  
  observeEvent(input$btn_confirm_delete_search, {
    req(user_auth$role == "Admin")
    
    con <- connect_db()
    if (!is.null(con)) {
      tryCatch({
        if (input$delete_method == "id") {
          req(input$delete_val_id)
          id_del <- suppressWarnings(as.integer(input$delete_val_id))
          dbExecute(con,
                    "DELETE FROM company_projects WHERE sr_no = $1",
                    list(id_del))
          showNotification("Entry deleted.", type = "message")
        } else {
          req(input$delete_val_name)
          dbExecute(con,
                    "DELETE FROM company_projects WHERE project = $1",
                    list(input$delete_val_name))
          showNotification("Entries matching name deleted.", type = "message")
        }
        removeModal()
        load_data()
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
      dbDisconnect(con)
    }
  })
}
###############################################################################
# RUN APP
###############################################################################
shinyApp(ui, server)







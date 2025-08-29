library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(sf)
library(RSQLite)
library(data.table)
library(dplyr)
library(RColorBrewer)
library(htmltools)

options(scipen = 999)

con <- dbConnect(RSQLite::SQLite(), "input_data.sqlite")
sim_data_raw <- dbReadTable(con, "data")
dbDisconnect(con)

sim_data <- as.data.table(sim_data_raw)

sim_data[, scenario_short := case_when(
  grepl("BAU|Business.*as.*usual", scenario_name, ignore.case = TRUE) ~ "BAU",
  grepl("NSP|National.*Strategic", scenario_name, ignore.case = TRUE) ~ "NSP",
  grepl("Intervention", scenario_name, ignore.case = TRUE) ~ "Intervention",
  TRUE ~ gsub("_", " ", substr(scenario_name, 1, 15))
)]

sim_data[, join_id := paste(admin_1, year, scenario_short, sep = "_")]

tryCatch({
  ch_shapes_raw <- st_read("ch_shapefiles/")
  
  ch_shapes_fixed <- st_zm(ch_shapes_raw, drop = TRUE, what = "ZM")
  
  if(any(!st_is_valid(ch_shapes_fixed))) {
    ch_shapes_fixed <- st_make_valid(ch_shapes_fixed)
  }
  
  ch_shapes <- st_transform(ch_shapes_fixed, 4326)
  
  message(paste("Loaded and fixed", nrow(ch_shapes), "canton shapes"))
}, error = function(e) {
  message("Warning: Could not load Shapefiles - mapping will be disabled")
  ch_shapes <- NULL
})

message(paste("Loaded", nrow(sim_data), "simulation records"))

# age groups
get_age_groups <- function(data) {
  if("age_group" %in% names(data)) {
    age_groups <- sort(unique(data$age_group))
    return(c("All Ages", age_groups))
  }
  return(c("All Ages"))
}

filter_by_age_group <- function(data, selected_age_group) {
  if(selected_age_group == "All Ages") {
    return(data)
  }
  
  if("age_group" %in% names(data)) {
    filtered_data <- data[age_group == selected_age_group]
    return(filtered_data)
  }
  
  return(data)
}

# reduction
calculate_reduction <- function(data, BAU_scenario = "BAU") {
  data <- data[!is.na(prevalenceRate) & !is.na(incidenceRate)]
  
  bau_data <- data[scenario_short == BAU_scenario]
  nsp_data <- data[scenario_short == "NSP"]
  
  if(nrow(bau_data) == 0 || nrow(nsp_data) == 0) {
    message("Warning: No data available for comparison")
    return(data.table())
  }
  
  bau_agg <- bau_data[, .(
    prevalenceRate_bau = mean(prevalenceRate, na.rm = TRUE),
    incidenceRate_bau = mean(incidenceRate, na.rm = TRUE)
  ), by = .(admin_1, year)]
  
  nsp_agg <- nsp_data[, .(
    prevalenceRate_nsp = mean(prevalenceRate, na.rm = TRUE),
    incidenceRate_nsp = mean(incidenceRate, na.rm = TRUE)
  ), by = .(admin_1, year)]
  
  setkey(bau_agg, admin_1, year)
  setkey(nsp_agg, admin_1, year)
  
  tryCatch({
    merged_data <- merge(nsp_agg, bau_agg, 
                         by = c("admin_1", "year"), 
                         all = FALSE)
    
    merged_data[, prevalence_reduction := fifelse(
      prevalenceRate_bau > 0,
      pmax(0, (prevalenceRate_bau - prevalenceRate_nsp) / prevalenceRate_bau * 100),
      0
    )]
    merged_data[, incidence_reduction := fifelse(
      incidenceRate_bau > 0,
      pmax(0, (incidenceRate_bau - incidenceRate_nsp) / incidenceRate_bau * 100),
      0
    )]
    
    return(merged_data)
  }, error = function(e) {
    message("Error in calculate_reduction: ", e$message)
    return(data.table())
  })
}

# intervention
get_intervention_columns <- function(data) {
  int_cols <- names(data)[grepl("^deployed_int_", names(data))]
  return(int_cols)
}

clean_intervention_name <- function(name) {
  cleaned <- gsub("deployed_int_", "", name)
  cleaned <- gsub("_", " ", cleaned)
  cleaned <- tools::toTitleCase(cleaned)
  if(nchar(cleaned) > 20) {
    cleaned <- paste0(substr(cleaned, 1, 17), "...")
  }
  return(cleaned)
}

get_safe_colors <- function(n, palette_name = "Set1") {
  if(n <= 1) return("#1f77b4")
  if(n <= 3) {
    return(c("#1f77b4", "#ff7f0e", "#2ca02c")[1:n])
  }
  
  tryCatch({
    if(n <= 9) {
      RColorBrewer::brewer.pal(max(3, n), palette_name)
    } else {
      colorRampPalette(RColorBrewer::brewer.pal(9, palette_name))(n)
    }
  }, error = function(e) {
    rainbow(n)
  })
}



####### dashboard

ui <- dashboardPage(
  dashboardHeader(
    title = "Malaria Simulation Dashboard",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("line-chart")),
      menuItem("Intervention Map", tabName = "mapping", icon = icon("map")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    
    hr(),
    h4("Controls", style = "margin-left: 15px; color: white;"),
    
    selectInput("age_group", 
                "Age Group:",
                choices = get_age_groups(sim_data),
                selected = "All Ages"),
    
    selectInput("scenario_filter",
                "Scenario:",
                choices = c("All" = "all", 
                            "BAU" = "BAU", 
                            "NSP" = "nsp"),
                selected = "all"),
    
    sliderInput("year_range",
                "Year Range:",
                min = min(sim_data$year, na.rm = TRUE),
                max = max(sim_data$year, na.rm = TRUE),
                value = c(min(sim_data$year, na.rm = TRUE), 
                          max(sim_data$year, na.rm = TRUE)),
                step = 1,
                sep = "")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          margin-bottom: 20px;
        }
        .small-legend .legend-item {
          font-size: 10px !important;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Dashboard Overview", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  p("Malaria transmission simulation results for Switzerland, comparing BAU and NSP scenarios."),
                  tags$ul(
                    tags$li("Time Series: Compare incidence and prevalence trends"),
                    tags$li("Intervention Map: View geographical deployment"),
                    tags$li("Data Explorer: Browse simulation data")
                  )
                )
              ),
              
              fluidRow(
                valueBoxOutput("total_population"),
                valueBoxOutput("avg_incidence"),
                valueBoxOutput("avg_prevalence")
              ),
              
              fluidRow(
                box(
                  title = "Incidence Trends",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("overview_incidence", height = "350px")
                ),
                box(
                  title = "Prevalence Trends",
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("overview_prevalence", height = "350px")
                )
              )
      ),
      
      tabItem(tabName = "timeseries",
              fluidRow(
                box(
                  title = "Incidence Rate Over Time",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("incidence_plot", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Prevalence Rate Over Time", 
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("prevalence_plot", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Effectiveness (% Reduction vs BAU)",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("reduction_plot", height = "400px")
                )
              )
      ),
      
      tabItem(tabName = "mapping",
              fluidRow(
                box(
                  title = "Intervention Deployment Map",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  height = "600px",
                  if(!is.null(ch_shapes)) {
                    leafletOutput("intervention_map", height = "550px")
                  } else {
                    p("Map unavailable - shapefile not found")
                  }
                )
              )
      ),
      
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Simulation Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("data_table")
                )
              )
      )
    )
  )
)



######### server

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$scenario_filter, input$year_range, input$age_group)
    
    data <- sim_data
    
    data <- filter_by_age_group(data, input$age_group)
    
    if (input$scenario_filter != "all") {
      if (input$scenario_filter == "BAU") {
        data <- data[scenario_short == "BAU"]
      } else if (input$scenario_filter == "nsp") {
        data <- data[scenario_short == "NSP"]
      }
    }
    
    data <- data[year >= input$year_range[1] & year <= input$year_range[2]]
    
    data <- data[!is.na(prevalenceRate) & !is.na(incidenceRate)]
    
    return(data)
  })
  
  reduction_data <- reactive({
    req(nrow(filtered_data()) > 0)
    year_data <- filter_by_age_group(sim_data, input$age_group)
    year_data <- year_data[year >= input$year_range[1] & year <= input$year_range[2]]
    calculate_reduction(year_data)
  })
  
  output$total_population <- renderValueBox({
    total_pop <- sum(filtered_data()$nHost, na.rm = TRUE)
    valueBox(
      value = format(total_pop, big.mark = ","),
      subtitle = paste("Total Population -", input$age_group),
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_incidence <- renderValueBox({
    avg_inc <- mean(filtered_data()$incidenceRate, na.rm = TRUE)
    valueBox(
      value = round(avg_inc, 4),
      subtitle = "Avg Incidence",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  output$avg_prevalence <- renderValueBox({
    avg_prev <- mean(filtered_data()$prevalenceRate, na.rm = TRUE)
    valueBox(
      value = round(avg_prev, 4),
      subtitle = "Avg Prevalence", 
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  output$overview_incidence <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    seed_avg <- data[, .(mean_inc = mean(incidenceRate, na.rm = TRUE)), 
                     by = .(admin_1, year, scenario_short)]
    
    plot_data <- seed_avg[, .(avg_incidence = mean(mean_inc, na.rm = TRUE)), 
                          by = .(year, scenario_short)]
    
    colors <- get_safe_colors(length(unique(plot_data$scenario_short)))
    
    plot_ly(plot_data,
            x = ~year,
            y = ~avg_incidence,
            color = ~scenario_short,
            colors = colors,
            type = 'scatter',
            mode = 'markers',
            hovertemplate = paste(
              "Year: %{x}<br>",
              "Incidence: %{y:.4f}<br>",
              "<extra>%{fullData.name}</extra>"
            )) %>%
      layout(
        title = list(text = "Incidence by Scenario", font = list(size = 14)),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Incidence Rate"),
        legend = list(orientation = "h", x = 0.1, y = -0.3),
        margin = list(t = 60, b = 80)
      )
  })
  
  output$overview_prevalence <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    by_canton <- data[, .(canton_avg = mean(prevalenceRate, na.rm = TRUE)), 
                      by = .(admin_1, year, scenario_short)]
    
    plot_data <- by_canton[, .(avg_prevalence = mean(canton_avg, na.rm = TRUE)), 
                           by = .(year, scenario_short)]
    
    colors <- get_safe_colors(length(unique(plot_data$scenario_short)))
    
    plot_ly(plot_data,
            x = ~year,
            y = ~avg_prevalence,
            color = ~scenario_short,
            colors = colors,
            type = 'scatter',
            mode = 'markers',
            hovertemplate = paste(
              "Year: %{x}<br>",
              "Prevalence: %{y:.4f}<br>",
              "<extra>%{fullData.name}</extra>"
            )) %>%
      layout(
        title = list(text = "Prevalence by Scenario", font = list(size = 14)),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Prevalence Rate"),
        legend = list(orientation = "h", x = 0.1, y = -0.3),
        margin = list(t = 60, b = 80)
      )
  })
  
  output$incidence_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", 
                                           showarrow = FALSE))
    }
    
    plot_data <- data[, .(incidence = mean(incidenceRate, na.rm = TRUE)),
                      by = .(admin_1, year, scenario_short)]
    
    plot_data <- plot_data[order(admin_1, scenario_short, year)]
    p <- plot_ly()
    
    unique_combos <- unique(plot_data[, .(admin_1, scenario_short)])
    colors <- get_safe_colors(length(unique(plot_data$admin_1)))
    
    for(i in 1:nrow(unique_combos)) {
      combo <- unique_combos[i]
      subset_data <- plot_data[admin_1 == combo$admin_1 & scenario_short == combo$scenario_short]
      
      line_dash <- ifelse(combo$scenario_short == "BAU", "solid", "dash")
      
      p <- p %>% add_trace(
        data = subset_data,
        x = ~year, 
        y = ~incidence,
        type = 'scatter',
        mode = 'lines+markers',
        name = paste(combo$admin_1, combo$scenario_short),
        line = list(
          color = colors[which(unique(plot_data$admin_1) == combo$admin_1)],
          dash = line_dash,
          width = 2
        ),
        marker = list(
          color = colors[which(unique(plot_data$admin_1) == combo$admin_1)],
          size = 6
        ),
        hovertemplate = paste(
          "<b>%{fullData.name}</b><br>",
          "Year: %{x}<br>",
          "Incidence: %{y:.4f}<br>",
          "<extra></extra>"
        )
      )
    }
    
    p %>% layout(
      title = "Incidence Trends by Canton",
      xaxis = list(title = "Year"),
      yaxis = list(title = "Incidence Rate"),
      legend = list(font = list(size = 9)),
      hovermode = 'closest'
    )
  })
  
  output$prevalence_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", 
                                           showarrow = FALSE))
    }
    
    plot_data <- data[, .(prevalence = mean(prevalenceRate, na.rm = TRUE)),
                      by = .(admin_1, year, scenario_short)]
    
    plot_data <- plot_data[!is.na(prevalence) & !is.na(year)]
    plot_data <- plot_data[order(admin_1, scenario_short, year)]
    p <- plot_ly()
    
    unique_combos <- unique(plot_data[, .(admin_1, scenario_short)])
    canton_colors <- get_safe_colors(length(unique(plot_data$admin_1)))
    names(canton_colors) <- unique(plot_data$admin_1)
    
    for(i in 1:nrow(unique_combos)) {
      combo <- unique_combos[i]
      subset_data <- plot_data[admin_1 == combo$admin_1 & scenario_short == combo$scenario_short]
      
      if(nrow(subset_data) == 0) next
      
      line_dash <- ifelse(combo$scenario_short == "BAU", "solid", "dash")
      line_opacity <- ifelse(combo$scenario_short == "BAU", 1.0, 0.8)
      
      p <- p %>% add_trace(
        data = subset_data,
        x = ~year, 
        y = ~prevalence,
        type = 'scatter',
        mode = 'lines+markers',
        name = paste(combo$admin_1, "-", combo$scenario_short),
        line = list(
          color = canton_colors[combo$admin_1],
          dash = line_dash,
          width = 2.5,
          opacity = line_opacity
        ),
        marker = list(
          color = canton_colors[combo$admin_1],
          size = 5,
          opacity = line_opacity,
          line = list(color = "white", width = 1)
        ),
        connectgaps = FALSE, 
        hovertemplate = paste(
          "<b>Canton:</b> %{fullData.name}<br>",
          "<b>Year:</b> %{x}<br>", 
          "<b>Prevalence Rate:</b> %{y:.4f}<br>",
          "<extra></extra>"
        )
      )
    }
    
    p %>% layout(
      title = list(
        text = "Prevalence Rate Over Time by Canton",
        font = list(size = 16, color = "#2c3e50")
      ),
      xaxis = list(
        title = list(text = "Year", font = list(size = 14)),
        tickfont = list(size = 12),
        gridcolor = "lightgray",
        gridwidth = 1
      ),
      yaxis = list(
        title = list(text = "Prevalence Rate", font = list(size = 14)),
        tickfont = list(size = 12),
        gridcolor = "lightgray",
        gridwidth = 1,
        tickformat = ".4f"
      ),
      legend = list(
        font = list(size = 10),
        orientation = "v",
        x = 1.02,
        y = 1,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "lightgray",
        borderwidth = 1
      ),
      hovermode = 'closest',
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(r = 120, t = 60, b = 60, l = 60)
    )
  })
  
  
  output$reduction_plot <- renderPlotly({
    red_data <- reduction_data()
    
    if (nrow(red_data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No reduction data available", 
                                           showarrow = FALSE))
    }
    
    colors <- get_safe_colors(length(unique(red_data$admin_1)))
    
    p <- plot_ly(red_data, x = ~year, y = ~prevalence_reduction,
                 color = ~admin_1, colors = colors,
                 type = 'scatter', mode = 'lines+markers',
                 hovertemplate = paste(
                   "<b>%{fullData.name}</b><br>",
                   "Year: %{x}<br>",
                   "Reduction: %{y:.1f}%<br>",
                   "<extra></extra>"
                 )) %>%
      layout(title = "Prevalence Reduction vs BAU (%)",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Reduction (%)"),
             legend = list(font = list(size = 9)))
    
    return(p)
  })
  
  output$intervention_map <- renderLeaflet({
    if(is.null(ch_shapes)) {
      return(leaflet() %>% addTiles() %>% 
               addMarkers(lng = 8.2, lat = 46.8, popup = "Shapefiles not available"))
    }
    
    nsp_data <- sim_data[scenario_short == "NSP"]
    nsp_data <- filter_by_age_group(nsp_data, input$age_group)
    nsp_data <- nsp_data[year >= input$year_range[1] & year <= input$year_range[2]]
    
    if (nrow(nsp_data) == 0) {
      return(leaflet() %>% 
               addTiles() %>%
               setView(lng = 8.2275, lat = 46.8182, zoom = 8))
    }
    
    int_cols <- get_intervention_columns(nsp_data)
    
    if (length(int_cols) > 0) {
      tryCatch({
        nsp_summary <- nsp_data[, {
          result <- list()
          for(col in int_cols) {
            result[[col]] <- any(get(col) > 0, na.rm = TRUE)
          }
          result
        }, by = admin_1]
        
        nsp_summary$intervention_combo <- apply(nsp_summary[, ..int_cols], 1, function(row) {
          active <- names(row)[row == TRUE]
          if(length(active) == 0) {
            return("None")
          } else {
            clean_names <- sapply(active, clean_intervention_name)
            combo <- paste(clean_names, collapse = ", ")
            if(nchar(combo) > 50) {
              combo <- paste0(substr(combo, 1, 47), "...")
            }
            return(combo)
          }
        })
        
        ch_merged <- merge(ch_shapes, nsp_summary, 
                           by.x = "NAME", by.y = "admin_1", all.x = TRUE)
        
        ch_merged$intervention_combo[is.na(ch_merged$intervention_combo)] <- "No data"
        
        unique_combos <- unique(ch_merged$intervention_combo)
        colors <- get_safe_colors(length(unique_combos), "Set3")
        pal <- colorFactor(palette = colors, domain = unique_combos)
        
        leaflet(ch_merged) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(intervention_combo),  # FIXED: Use colorFactor
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE
            ),
            popup = ~paste0(
              "<strong>Canton: </strong>", NAME, "<br>",
              "<strong>Interventions: </strong>", intervention_combo
            )
          ) %>%
          addLegend(
            pal = pal,                    
            values = ~intervention_combo,
            title = "Interventions",
            position = "bottomright"
          ) %>%
          setView(lng = 8.2275, lat = 46.8182, zoom = 8)
        
      }, error = function(e) {
        message("Map error: ", e$message)
        leaflet(ch_shapes) %>%
          addTiles() %>%
          addPolygons(popup = ~paste("Canton:", NAME)) %>%
          setView(lng = 8.2275, lat = 46.8182, zoom = 8)
      })
      
    } else {
      leaflet(ch_shapes) %>%
        addTiles() %>%
        addPolygons(popup = ~paste("Canton:", NAME)) %>%
        setView(lng = 8.2275, lat = 46.8182, zoom = 8)
    }
  })
  
  output$data_table <- DT::renderDataTable({
    data <- filtered_data()
    
    # Include age_group in the display if it exists
    display_cols <- c("Canton" = "admin_1", "Year" = "year", "Scenario" = "scenario_short")
    
    if("age_group" %in% names(data)) {
      display_cols <- c(display_cols, "Age Group" = "age_group")
    }
    
    display_cols <- c(display_cols, 
                      "Population" = "nHost",
                      "Prevalence Rate" = "prevalenceRate", 
                      "Incidence Rate" = "incidenceRate")
    
    display_data <- data[, .(
      Canton = admin_1,
      Year = year,
      Scenario = scenario_short,
      `Age Group` = if("age_group" %in% names(data)) age_group else "All",
      Population = paste0(round(nHost/1000, 1), "K"),
      `Prevalence Rate` = round(prevalenceRate, 4),
      `Incidence Rate` = round(incidenceRate, 4)
    )]
    
    if (nrow(display_data) > 5000) {
      display_data <- display_data[1:5000]
    }
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      filter = 'top'
    )
  })
}

shinyApp(ui = ui, server = server)
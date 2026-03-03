library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(forcats)
library(tidyr)


# ---- UI ----
ui = fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Poppins:wght@500;600&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body {
        background-color: #f4f6f8;
        font-family: 'Inter', sans-serif;
        color: #222;
      }
      h2, h3 {
        font-family: 'Poppins', sans-serif;
        font-weight: 600;
      }
      .navbar {
        background: linear-gradient(90deg, #0047AB 0%, #6BAF92 100%);
        border: none;
        box-shadow: 0 3px 8px rgba(0,0,0,0.15);
      }
      .navbar-brand {
        color: white !important;
        font-weight: 600;
        font-size: 20px;
      }
      .navbar-nav > li > a {
        color: white !important;
        font-weight: 500;
        padding: 12px 18px;
        transition: background 0.3s;
        border-radius: 8px;
      }
      .navbar-nav > li > a:hover {
        background-color: rgba(255, 255, 255, 0.15);
      }
      .navbar-nav > .active > a {
        background-color: white !important;
        color: #0047AB !important;
        font-weight: 600;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      }
      .top-filters {
        display: flex;
        justify-content: center;
        gap: 20px;
        background: linear-gradient(180deg, #ffffff 0%, #f0f3f6 100%);
        padding: 15px;
        border-bottom: 1px solid #d1d9e0;
      }
      .main-container {
        max-width: 1800px;
        margin: 0 auto;
        padding: 15px;
      }
      .dataTables_wrapper {
        background-color: #fff;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        padding: 10px;
      }
      .percentile-plots {
        display: flex;
        justify-content: space-between;
        gap: 15px;
        width: 100%;
      }
      .percentile-plots > div {
        flex: 1;
        background-color: #ffffff;
        border-radius: 12px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.08);
        padding: 10px;
      }
      h3 {
        text-align: center;
      }
      .split-chart-container {
        background-color: #ffffff;
        border-radius: 12px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.08);
        padding: 20px;
        margin-top: 20px;
      }
      .radar-chart-container {
        background-color: #ffffff;
        border-radius: 12px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.08);
        padding: 20px;
        margin-top: 20px;
      }
      .btn-default {
        background-color: #0047AB;
        color: white;
        border: none;
        padding: 8px 16px;
        border-radius: 6px;
        font-weight: 500;
        margin: 5px;
      }
      .btn-default:hover {
        background-color: #003380;
      }
      .download-section {
        text-align: center;
        margin: 15px 0;
      }
    "))
  ),
  
  navbarPage(
    "Player Cards",
    
    # ---- HOME TAB ----
    tabPanel("Home",
             div(class = "top-filters",
                 selectInput("player_select", "Select Player:",
                             choices = sort(unique(fg_playercard2025$batter_name)),
                             selected = "Judge, Aaron")
             ),
             div(class = "main-container",
                 uiOutput("player_title"),
                 br(),
                 DTOutput("combined_stats"),
                 br(),
                 h3("Percentile Charts"),
                 div(class = "download-section",
                     downloadButton("download_batted", "Download Batted Ball"),
                     downloadButton("download_contact", "Download Contact"),
                     downloadButton("download_plate", "Download Plate Discipline")
                 ),
                 div(class = "percentile-plots",
                     div(plotOutput("batted_ball_plot", height = "480px")),
                     div(plotOutput("contact_plot", height = "480px")),
                     div(plotOutput("plate_plot", height = "480px"))
                 ),
                 br(),
                 h3("Pitch Class Performance Radar"),
                 div(class = "download-section",
                     downloadButton("download_radar", "Download Radar Chart")
                 ),
                 div(class = "radar-chart-container",
                     plotOutput("batter_radar_plot", height = "500px")
                 )
             )
    ),
    
    # ---- PITCH TYPE TAB ----
    tabPanel("Pitch Type",
             div(class = "top-filters",
                 selectInput("pitch_player", "Select Player:",
                             choices = sort(unique(mutated_sc_2025$batter_name)),
                             selected = "Judge, Aaron")
             ),
             div(class = "main-container",
                 h2("Pitch Type Splits", align = "center"),
                 br(),
                 DTOutput("pitchtype_battedball"),
                 br(),
                 DTOutput("pitchtype_contact"),
                 br(),
                 DTOutput("pitchtype_disc")
             )
    ),
    
    # ---- L/R SPLITS TAB ----
    tabPanel("L/R Splits",
             div(class = "top-filters",
                 selectInput("LR_player", "Select Player:",
                             choices = sort(unique(mutated_sc_2025$batter_name)),
                             selected = "Judge, Aaron")
             ),
             div(class = "main-container",
                 div(class = "download-section",
                     downloadButton("download_LRcontact", "Download L/R Contact"),
                     downloadButton("download_LRbatted", "Download L/R Batted Ball"),
                     downloadButton("download_LRdiscipline", "Download L/R Discipline")
                 ),
                 div(class = "split-chart-container",
                     plotOutput("LRcontact_plot", height = "700px")
                 )
             ),
             div(class = "main-container",
                 div(class = "split-chart-container",
                     plotOutput("LRbatted_plot", height = "700px")
                 )
             ),
             div(class = "main-container",
                 div(class = "split-chart-container",
                     plotOutput("LRdiscipline_plot", height = "700px")
                 )
             )
    ),
    
    # ---- RISP SPLITS TAB ----
    tabPanel("RISP Splits",
             div(class = "top-filters",
                 selectInput("risp_player", "Select Player:",
                             choices = sort(unique(mutated_sc_2025$batter_name)),
                             selected = "Judge, Aaron")
             ),
             div(class = "main-container",
                 div(class = "download-section",
                     downloadButton("download_risp", "Download RISP Chart")
                 ),
                 div(class = "split-chart-container",
                     plotOutput("risp_plot", height = "700px")
                 )
             )
    ),
    
    # ---- HOME/ROAD SPLITS TAB ----
    tabPanel("Home/Road Splits",
             div(class = "top-filters",
                 selectInput("homeroad_player", "Select Player:",
                             choices = sort(unique(mutated_sc_2025$batter_name)),
                             selected = "Judge, Aaron")
             ),
             div(class = "main-container",
                 div(class = "download-section",
                     downloadButton("download_homeroad", "Download Home/Road Chart")
                 ),
                 div(class = "split-chart-container",
                     plotOutput("homeroad_plot", height = "700px")
                 )
             )
    ),
    
    # ---- PITCH COUNT SPLITS TAB ----
    tabPanel("Pitch Count Splits",
             div(class = "top-filters",
                 selectInput("pitchcount_player", "Select Player:",
                             choices = sort(unique(mutated_sc_2025$batter_name)),
                             selected = "Judge, Aaron")
             ),
             div(class = "main-container",
                 div(class = "download-section",
                     downloadButton("download_pitchcount", "Download Pitch Count Chart")
                 ),
                 div(class = "split-chart-container",
                     plotOutput("pitchcount_plot", height = "700px")
                 )
             )
    ),
    
    # ---- FIRST/SECOND HALF SPLITS TAB ----
    tabPanel("First/Second Half Splits",
             div(class = "top-filters",
                 selectInput("firstsecondhalf_player", "Select Player:",
                             choices = sort(unique(mutated_sc_2025$batter_name)),
                             selected = "Judge, Aaron")
             ),
             div(class = "main-container",
                 div(class = "download-section",
                     downloadButton("download_firstsecondhalf", "Download First/Second Half Chart")
                 ),
                 div(class = "split-chart-container",
                     plotOutput("firstsecondhalf_plot", height = "700px")
                 )
             )
    )
  )
)

# ---- SERVER ----
server = function(input, output, session) {
  
  # ----------------- Player Stats Section -----------------
  player_info = reactive({
    fg_playercard2025 %>% filter(batter_name == input$player_select)
  })
  
  output$player_title = renderUI({
    df = player_info()
    if (nrow(df) == 0) return(NULL)
    h2(paste0(df$batter_name, " | ", df$Bats, " | Age ", df$Age, " | ", df$position),
       align = "center")
  })
  
  output$combined_stats = renderDT({
    df = player_info() %>%
      select(AVG, OBP, SLG, OPS, wOBA, wRC_plus, BB_pct, K_pct, BABIP,
             WAR, WPA, RE24, Clutch) %>%
      mutate(across(c(AVG, OBP, SLG, OPS, wOBA, BABIP), ~round(., 3)),
             `BB%` = round(BB_pct * 100, 1),
             `K%` = round(K_pct * 100, 1),
             WAR = round(WAR, 1),
             WPA = round(WPA, 1),
             RE24 = round(RE24, 1),
             Clutch = round(Clutch, 1),
             `wRC+` = round(wRC_plus, 0)) %>%
      select(AVG, OBP, SLG, OPS, wOBA, `wRC+`, `BB%`, `K%`, BABIP,
             WAR, WPA, RE24, Clutch)
    
    datatable(df,
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE,
              class = 'cell-border hover') %>%
      formatStyle(columns = names(df), border = '1px solid black')
  })
  
  #  - Percentile Plots  -
  output$batted_ball_plot = renderPlot({
    source("plot_batted_ball.R", local = TRUE)
    make_batted_ball_plot(input$player_select)
  })
  
  output$contact_plot = renderPlot({
    source("plot_contact.R", local = TRUE)
    make_contact_plot(input$player_select)
  })
  
  output$plate_plot = renderPlot({
    source("plot_plate.R", local = TRUE)
    make_plate_plot(input$player_select)
  })
  
  # Batter Radar Chart
  output$batter_radar_plot = renderPlot({
    create_batter_radar(input$player_select)
  })
  
  # ----------------- DOWNLOAD HANDLERS -----------------
  
  # Home tab downloads
  output$download_batted = downloadHandler(
    filename = function() {
      paste0(input$player_select, "_BattedBall_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 150)
      source("plot_batted_ball.R", local = TRUE)
      print(make_batted_ball_plot(input$player_select))
      dev.off()
    }
  )
  
  output$download_contact = downloadHandler(
    filename = function() {
      paste0(input$player_select, "_Contact_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 150)
      source("plot_contact.R", local = TRUE)
      print(make_contact_plot(input$player_select))
      dev.off()
    }
  )
  
  output$download_plate = downloadHandler(
    filename = function() {
      paste0(input$player_select, "_PlateDiscipline_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 150)
      source("plot_plate.R", local = TRUE)
      print(make_plate_plot(input$player_select))
      dev.off()
    }
  )
  
  output$download_radar = downloadHandler(
    filename = function() {
      paste0(input$player_select, "_RadarChart_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1600, height = 600, res = 150)
      print(create_batter_radar(input$player_select))
      dev.off()
    }
  )
  
  # L/R Splits downloads
  output$download_LRcontact = downloadHandler(
    filename = function() {
      paste0(input$LR_player, "_LR_Contact_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 900, res = 150)
      print(contact.handedness.percentile(input$LR_player))
      dev.off()
    }
  )
  
  output$download_LRbatted = downloadHandler(
    filename = function() {
      paste0(input$LR_player, "_LR_BattedBall_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 900, res = 150)
      print(battedball.handedness.percentile(input$LR_player))
      dev.off()
    }
  )
  
  output$download_LRdiscipline = downloadHandler(
    filename = function() {
      paste0(input$LR_player, "_LR_Discipline_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 900, res = 150)
      print(platediscipline.handedness.percentile(input$LR_player))
      dev.off()
    }
  )
  
  # Other splits downloads
  output$download_risp = downloadHandler(
    filename = function() {
      paste0(input$risp_player, "_RISP_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 900, res = 150)
      print(risp.percentile(input$risp_player))
      dev.off()
    }
  )
  
  output$download_homeroad = downloadHandler(
    filename = function() {
      paste0(input$homeroad_player, "_HomeRoad_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 900, res = 150)
      print(homeroad.percentile(input$homeroad_player))
      dev.off()
    }
  )
  
  output$download_pitchcount = downloadHandler(
    filename = function() {
      paste0(input$pitchcount_player, "_PitchCount_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 900, res = 150)
      print(pitchcount.percentile(input$pitchcount_player))
      dev.off()
    }
  )
  
  output$download_firstsecondhalf = downloadHandler(
    filename = function() {
      paste0(input$firstsecondhalf_player, "_FirstSecondHalf_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 900, res = 150)
      print(firstsecondhalf.percentile(input$firstsecondhalf_player))
      dev.off()
    }
  )
  
  # ----------------- Pitch Type Tab Tables -----------------
  pitch_colors = c("CH"="#1DBE3A","CU"="#00D1ED","FC"="#933F2C","FF"="#D22D49","FS"="#3BACAC","SI"="#FE9D00","SL"="#FFD166")
  
  # Batted Ball
  output$pitchtype_battedball = renderDT({
    player = input$pitch_player
    df = mutated_sc_2025 %>%
      mutate(pitch_type = case_when(
        pitch_type %in% c("CS", "KC") ~ "CU",
        pitch_type %in% c("ST", "SV") ~ "SL",
        pitch_type == "FO" ~ "FS",
        pitch_type == "SC" ~ "CH",
        TRUE ~ pitch_type
      )) %>%
      group_by(batter_name, pitch_type) %>%
      summarise(
        pitches_seen = n(),
        xwOBAcon = round(mean(estimated_woba_using_speedangle[type == "X"], na.rm = T), 3),
        sweetspotpct = round(sum(launch_angle >= 8 & launch_angle <= 32 & type == "X", na.rm = T) / sum(type == "X", na.rm = T), 3),
        hardhitpct = round(sum(launch_speed >= 95 & type == "X", na.rm = T) / sum(type == "X", na.rm = T), 3),
        xwOBA = round(sum(estimated_woba_using_speedangle, na.rm = T) / sum(woba_denom, na.rm = T), 3),
        AIRpct = round(sum(bb_type %in% c("popup", "fly_ball", "line_drive"), na.rm = T) / sum(type == "X", na.rm = T), 3)
      ) %>%
      filter(batter_name == player & pitch_type != "EP" & !is.na(pitch_type)) %>%
      na.omit() %>%
      ungroup() %>%
      select(-batter_name) %>%
      rename(`Pitch Type` = pitch_type, Pitches = pitches_seen, `xwOBAcon` = xwOBAcon,
             `SwtSpt%` = sweetspotpct, `Hardhit%` = hardhitpct, `xwOBA` = xwOBA, `Air%` = AIRpct)
    
    datatable(df, caption = tags$caption(style = 'caption-side: top; text-align: center; font-weight: 700; font-size: 20px;',
                                         "Pitch Type Splits: Batted Ball Quality"),
              options = list(dom = 't', ordering = F, pageLength = nrow(df),
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))),
              rownames = F) %>%
      formatStyle(columns = 'Pitch Type', backgroundColor = styleEqual(names(pitch_colors), pitch_colors),
                  color = 'black', textAlign = "center")
  })
  
  # Contact
  output$pitchtype_contact = renderDT({
    player = input$pitch_player
    df = mutated_sc_2025 %>%
      mutate(pitch_type = case_when(
        pitch_type %in% c("CS", "KC") ~ "CU",
        pitch_type %in% c("ST", "SV") ~ "SL",
        pitch_type == "FO" ~ "FS",
        pitch_type == "SC" ~ "CH",
        TRUE ~ pitch_type
      )) %>%
      group_by(batter_name, pitch_type) %>%
      summarise(
        pitches_seen = n(),
        contactpct = round(sum(whiff_or_contact == "contact", na.rm = T) /
                             sum(swing_or_noswing == "swing", na.rm = T), 3),
        z_contactpct = round(sum(zone %in% 1:9 & whiff_or_contact == "contact", na.rm = T) /
                               sum(zone %in% 1:9 & swing_or_noswing == "swing", na.rm = T), 3),
        ozone_contactpct = round(sum(!zone %in% 1:9 & whiff_or_contact == "contact", na.rm = T) /
                                   sum(!zone %in% 1:9 & swing_or_noswing == "swing", na.rm = T), 3)
      ) %>%
      filter(batter_name == player & pitch_type != "EP" & !is.na(pitch_type)) %>%
      na.omit() %>%
      ungroup() %>%
      select(-batter_name) %>%
      rename(`Pitch Type` = pitch_type, Pitches = pitches_seen,
             `Contact%` = contactpct, `z-Contact%` = z_contactpct, `o-Contact%` = ozone_contactpct)
    
    datatable(df, caption = tags$caption(style = 'caption-side: top; text-align: center; font-weight: 700; font-size: 20px;',
                                         "Pitch Type Splits: Contact"),
              options = list(dom = 't', ordering = F, pageLength = nrow(df),
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))),
              rownames = F) %>%
      formatStyle(columns = 'Pitch Type', backgroundColor = styleEqual(names(pitch_colors), pitch_colors),
                  color = 'black', textAlign = "center")
  })
  
  # Plate Discipline
  output$pitchtype_disc = renderDT({
    player = input$pitch_player
    df = mutated_sc_2025 %>%
      mutate(pitch_type = case_when(
        pitch_type %in% c("CS", "KC") ~ "CU",
        pitch_type %in% c("ST", "SV") ~ "SL",
        pitch_type == "FO" ~ "FS",
        pitch_type == "SC" ~ "CH",
        TRUE ~ pitch_type
      )) %>%
      group_by(batter_name, pitch_type) %>%
      summarise(
        pitches_seen = n(),
        chasepct = round(sum(zone %in% c("11","12","13","14") & swing_or_noswing == "swing", na.rm =T) /
                           sum(zone %in% c("11","12","13","14"), na.rm = T), 3),
        z_swingpct = round(sum(zone %in% 1:9 & swing_or_noswing == "swing", na.rm = T) /
                             sum(zone %in% 1:9, na.rm = T), 3)
      ) %>%
      filter(batter_name == player & pitch_type != "EP" & !is.na(pitch_type)) %>%
      na.omit() %>%
      ungroup() %>%
      select(-batter_name) %>%
      rename(`Pitch Type` = pitch_type, Pitches = pitches_seen,
             `Chase%` = chasepct, `z-Swing%` = z_swingpct)
    
    datatable(df, caption = tags$caption(style = 'caption-side: top; text-align: center; font-weight: 700; font-size: 20px;',
                                         "Pitch Type Splits: Plate Discipline"),
              options = list(dom = 't', ordering = F, pageLength = nrow(df),
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))),
              rownames = F) %>%
      formatStyle(columns = 'Pitch Type', backgroundColor = styleEqual(names(pitch_colors), pitch_colors),
                  color = 'black', textAlign = "center")
  })
  
  
  
  # ----------------- NEW SPLIT CHART OUTPUTS -----------------
  
  ########################################## L/R SPLITS ###############################
  # L/R Contact Splits Chart
  output$LRcontact_plot = renderPlot({
    contact.handedness.percentile(input$LR_player)
  })
  
  # L/R Batted Ball Splits Chart
  output$LRdiscipline_plot = renderPlot({
    platediscipline.handedness.percentile(input$LR_player)
  })
  
  # L/R Plate Discipline Splits Chart
  output$LRbatted_plot = renderPlot({
    battedball.handedness.percentile(input$LR_player)
  })
  
  ###################### OTHER SPLITS #########################
  
  # RISP Splits Chart
  output$risp_plot = renderPlot({
    risp.percentile(input$risp_player)
  })
  
  # Home/Road Splits Chart
  output$homeroad_plot = renderPlot({
    homeroad.percentile(input$homeroad_player)
  })
  
  # Pitch Count Splits Chart
  output$pitchcount_plot = renderPlot({
    pitchcount.percentile(input$pitchcount_player)
  })
  
  # First/Second Half Splits Chart
  output$firstsecondhalf_plot = renderPlot({
    firstsecondhalf.percentile(input$firstsecondhalf_player)
  })
  
}

# ---- BATTER RADAR FUNCTION ----
create_batter_radar <- function(batter) {
  mutated_sc_2025 %>% 
    mutate(
      pitch_class = case_when(
        pitch_type %in% c("FF", "FC", "SI") ~ "Fastball",
        pitch_type %in% c("CH", "FS", "FO", "SC") ~ "Offspeed",
        pitch_type %in% c("CS", "KC", "CU", "ST", "SV", "SL") ~ "Breaking",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(pitch_class)) %>%
    group_by(batter_name, pitch_class) %>% 
    summarise(
      n_pitches = n(),
      `Whiff%` = round(sum(whiff_or_contact == 'whiff', na.rm = T)/
                         sum(swing_or_noswing == 'swing', na.rm = T),3),
      `z-Whiff%` = round(sum(whiff_or_contact == 'whiff' & zone %in% as.character(1:9), na.rm = T)/
                           sum(swing_or_noswing == 'swing' & zone %in% as.character(1:9), na.rm = T),3),
      `PutAway%` = round(sum(events %in% c("strikeout", "strikeout_double_play") & strikes == 2, na.rm = T)/
                           sum(strikes == 2, na.rm = T),3),
      `CS%` = round(sum(description == "called_strike", na.rm = T)/
                      sum(!is.na(description), na.rm = T),3),
      `CSW%` = round(sum(description %in% c("called_strike","swinging_strike", 
                                            "swinging_strike_blocked","foul_tip"), na.rm = T)/
                       sum(!is.na(description), na.rm = T),3),
      `Chase%` = round(sum(swing_or_noswing == 'swing' & zone %in% c("11", "12", "13","14"), na.rm = T)/
                         sum(zone %in% c("11", "12", "13", "14"), na.rm = T),3),
      xwOBAcon = round(mean(estimated_woba_using_speedangle[type == "X"], na.rm = T),3),
      `Hardhit%` = round(sum(launch_speed >= 95 & type == "X", na.rm = T)/
                           sum(type == "X", na.rm = T),3),
      `GB%` = round(sum(bb_type %in% c("ground_ball") & type == "X", na.rm = T)/
                      sum(type == "X", na.rm = T),3),
      `AirEV` = round(median(launch_speed[bb_type %in% c("fly_ball", "line_drive", "popup")], na.rm = T),3),
      .groups = "drop"
    ) %>% 
    group_by(pitch_class) %>% 
    mutate(
      #ecdf functions based only on batters with 100+ pitches minimum
      # For batters: LOWER is better for Whiff%, z-Whiff%, PutAway%, CS%, CSW%, Chase%, GB%
      Whiff_percentile   = round(1 - ecdf(`Whiff%`[n_pitches >= 100])(`Whiff%`),3),
      zWhiff_percentile  = round(1 - ecdf(`z-Whiff%`[n_pitches >= 100])(`z-Whiff%`),3),
      PutAway_percentile = round(1 - ecdf(`PutAway%`[n_pitches >= 100])(`PutAway%`),3),
      CS_percentile      = round(1 - ecdf(`CS%`[n_pitches >= 100])(`CS%`),3),
      CSW_percentile     = round(1 - ecdf(`CSW%`[n_pitches >= 100])(`CSW%`),3),
      Chase_percentile   = round(1 - ecdf(`Chase%`[n_pitches >= 100])(`Chase%`),3),
      GB_percentile      = round(1 - ecdf(`GB%`[n_pitches >= 100])(`GB%`),3),
      # For batters: HIGHER is better for xwOBAcon, Hardhit%, AirEV
      xwOBAcon_percentile = round(ecdf(xwOBAcon[n_pitches >= 100])(xwOBAcon),3),
      HardHit_percentile  = round(ecdf(`Hardhit%`[n_pitches >= 100])(`Hardhit%`),3),
      AirEV_percentile    = round(ecdf(AirEV[n_pitches >= 100])(AirEV),3)
    ) %>% 
    filter(n_pitches >= 50) %>% 
    ungroup() %>% 
    filter(batter_name == batter) %>% 
    select(pitch_class, ends_with("_percentile")) %>%
    # Pivot to long format
    pivot_longer(
      cols = ends_with("_percentile"),
      names_to = "metric",
      values_to = "percentile"
    ) %>%
    mutate(
      metric = gsub("_percentile", "", metric)
    ) %>%
    # Add coordinates for radar chart
    group_by(pitch_class) %>%
    mutate(
      n_metrics = n(),
      angle = 2 * pi * (row_number() - 1) / n_metrics,
      x = percentile * cos(angle),
      y = percentile * sin(angle),
      x_label = 1.2 * cos(angle),
      y_label = 1.2 * sin(angle)
    ) %>%
    ungroup() %>%
    ggplot(aes(x = x, y = y, group = pitch_class, color = pitch_class, fill = pitch_class)) +
    #Radar background circles
    geom_polygon(
      data = data.frame(
        percentile = rep(c(0.25, 0.5, 0.75, 1), each = 100),
        angle = rep(seq(0, 2*pi, length.out = 100), 4)
      ) %>%
        mutate(x = percentile * cos(angle), y = percentile * sin(angle)),
      aes(x = x, y = y, group = percentile),
      fill = NA, color = "grey80", inherit.aes = FALSE
    ) +
    #labels for the background axis
    geom_text(
      data = data.frame(
        percentile = c(0.25, 0.5, 0.75, 1),
        label = c("25th", "50th", "75th", "100th")
      ) %>%
        mutate(
          angle_offset = pi / 20,
          x = percentile * cos(angle_offset), 
          y = percentile * sin(angle_offset)
        ),
      aes(x = x, y = y, label = label),
      size = 2.5,
      color = "grey50",
      inherit.aes = FALSE
    ) +
    #radial, straight lines
    geom_segment(
      aes(x = 0, y = 0, xend = x_label, yend = y_label),
      color = "grey80",
      inherit.aes = FALSE
    ) +
    #The actual radar plots/points
    geom_polygon(alpha = 0.3) +
    geom_point(size = 2) +
    #Metric labels 
    geom_text(
      aes(x = x_label, y = y_label, label = metric),
      size = 3,
      inherit.aes = FALSE,
      check_overlap = FALSE
    ) +
    facet_wrap(~ pitch_class, nrow = 1) +
    coord_fixed(clip = "off") +
    scale_color_manual(values = c(
      "Fastball" = "#D22D49",
      "Offspeed" = "#1DBE3A",
      "Breaking" = "#EEE716"
    )) +
    scale_fill_manual(values = c(
      "Fastball" = "#D22D49",
      "Offspeed" = "#1DBE3A",
      "Breaking" = "#EEE716"
    )) +
    labs(
      title = paste("Pitch Performance Radar Charts -", batter),
      subtitle = "Percentile Rankings by Pitch Class (Based on 100+ Pitch Sample)"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(hjust = 0.5,
                                size = 18,
                                color = 'black',
                                face = 'bold'),
      plot.subtitle = element_text(hjust = 0.5),
      plot.margin = margin(10, 30, 10, 30)
    )
}

shinyApp(ui, server)
make_plate_plot = function(player_name) {
  df = plate.discipline.playercard %>%
    filter(batter_name == player_name) %>%
    select(-BB_pct, -K_pct, -PAs, -pitchesPerPA) %>% 
    pivot_longer(
      cols = -batter_name,
      names_to = "MetricName",
      values_to = "Value"
    ) %>%
    mutate(
      BaseMetric = str_remove(MetricName, "_percentile$"),
      IsPercentile = str_detect(MetricName, "_percentile$"),
      player_name_clean = str_c(
        str_extract(batter_name, "(?<=, ).*"), " ", 
        str_remove(batter_name, ",.*")
      )
    ) %>%
    pivot_wider(
      names_from = IsPercentile,
      values_from = Value,
      names_prefix = "MetricPercentile_"
    ) %>%
    select(
      batter_name,
      Metric = BaseMetric,
      MetricValue = MetricPercentile_FALSE,
      MetricPercentile = MetricPercentile_TRUE,
      player_name_clean
    ) %>% 
    filter(!is.na(MetricPercentile)) %>% 
    select(player_name_clean, Metric, MetricPercentile) %>% 
    inner_join(
      (plate.discipline.playercard %>% 
         filter(batter_name == player_name) %>% 
         select(-BB_pct, -K_pct, -PAs, -pitchesPerPA) %>% 
         pivot_longer(
           cols = -batter_name,
           names_to = "MetricName",
           values_to = "Value"
         ) %>%
         mutate(
           BaseMetric = str_remove(MetricName, "_percentile$"),
           IsPercentile = str_detect(MetricName, "_percentile$"),
           player_name_clean = str_c(
             str_extract(batter_name, "(?<=, ).*"), " ", 
             str_remove(batter_name, ",.*")
           )
         ) %>%
         pivot_wider(
           names_from = IsPercentile,
           values_from = Value,
           names_prefix = "MetricPercentile_"
         ) %>%
         select(
           batter_name,
           Metric = BaseMetric,
           MetricValue = MetricPercentile_FALSE,
           MetricPercentile = MetricPercentile_TRUE,
           player_name_clean
         ) %>% 
         filter(!is.na(MetricValue)) %>% 
         select(player_name_clean, Metric, MetricValue)),
      by = c("player_name_clean", "Metric")
    ) %>% 
    mutate(
      Metric = factor(Metric, levels = rev(c("z_swingpct","chasepct","zone_ozonepct","heart_swingpct", 
                                             "chase_swingpct", "approach", "high_swingpct",
                                             "low_swingpct", "inside_swingpct",
                                             "away_swingpct"))),
      Metric = recode(Metric,
                      "z_swingpct" = "Z-Swing%",
                      "chasepct" = "O-Swing%",
                      "zone_ozonepct" = "inZone% - oZone%",
                      "heart_swingpct" = "Heart Swing%",
                      "chase_swingpct" = "Chase Swing%",
                      "approach" = "Heart% - Chase%",
                      "high_swingpct" = "High Swing%",
                      "low_swingpct" = "Low Swing%",
                      "inside_swingpct" = "Inside Swing%",
                      "away_swingpct" = "Away Swing%"
      )
    ) 
    
  plot = ggplot(df, aes(x = Metric, y = MetricPercentile)) +
    
    # background rectangle
    geom_rect(
      aes(xmin = as.numeric(Metric) - 0.45,
          xmax = as.numeric(Metric) + 0.45,
          ymin = 0, ymax = 100),
      fill = "#F7F7F7", color = NA
    ) +
    
    # percentile bars
    geom_col(aes(fill = MetricPercentile), width = 0.75, color = NA) +
    
    # circle + percentile text
    geom_point(aes(y = MetricPercentile), shape = 21, size = 6,
               fill = "white", color = "black", stroke = 1) +
    
    geom_text(aes(y = MetricPercentile, label = round(MetricPercentile)),
              size = 3.3, fontface = "bold") +
    
    # dashed horizontal separators (these become HORIZONTAL after coord_flip)
    geom_segment(
      data = df,
      aes(x = as.numeric(Metric)-0.5, xend = as.numeric(Metric) - 0.5,
          y = 100, yend = 107),
      inherit.aes = FALSE,
      color = "#6BAF92", linetype = "dashed", linewidth = 0.6
    ) +
    
    # metric value label on right
    geom_text(aes(y = 107, label = round(MetricValue, 3)),
              hjust = 0, size = 3.5, color = "black") +
    
    coord_flip(clip = "off") +
    
    scale_fill_gradientn(
      colours = c("#0047AB", "#B0B0B0", "#FF4C4C"),
      values = c(0, 0.5, 1),            
      limits = c(0, 100),                
      oob = scales::squish               
    )+
    
    labs(
      title = paste0(unique(df$player_name_clean),
                     " – 2025 Plate Discipline Percentile Rankings"),
      subtitle = "Percentile rankings for swing decisions and contact discipline (0–100 scale)",
      x = NULL,
      y = "Percentile"
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.y = element_text(face = "bold", color = "black", size = 11),
      legend.position = "none",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 60, 10, 10)
    )
  
  
  return(plot)
  
}






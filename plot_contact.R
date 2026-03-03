make_contact_plot = function(player_name) {
  df = hittool.playercard %>%
    filter(batter_name == player_name) %>%
    select(-PAs, -BBEs) %>% 
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
      (hittool.playercard %>% 
         select(-PAs, -BBEs) %>% 
         filter(batter_name == player_name) %>% 
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
      Metric = factor(Metric, levels = rev(c("contactpct","z_contactpct","ozone_contactpct", "heart_contactpct", 
                                             "shadow_contactpct", "chase_contactpct", "high_contactpct",
                                             "low_contactpct", "inside_contactpct",
                                             "away_contactpct"))),
      Metric = recode(Metric,
                      "contactpct" = "Contact%",
                      "z_contactpct" = "Z-Contact%",
                      "ozone_contactpct" = "O-Contact%",
                      "heart_contactpct" = "Heart Contact%",
                      "shadow_contactpct" = "Shadow Contact%",
                      "chase_contactpct" = "Chase Contact%",
                      "high_contactpct" = "High Contact%",
                      "low_contactpct" = "Low Contact%",
                      "inside_contactpct" = "Inside Contact%",
                      "away_contactpct" = "Away Contact%"
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
                     " – 2025 Contact Percentile Rankings"),
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









thecolors <<- c(
  "#F7A35C",    # Light orange
  "#ADD8E6",    # Light blue
  "#E9BB97",    # Soft amber
  "#E4D354",    # Light mustard
  "#90EE90",    # Light green
  "#1F78B4",    # Bluish
  "#F4A8A8",    # Pale red
  "#C29EC4",    # Pastel purple
  "#FFD966",    # Light gold
  "#B2DFEE",    # Light sky blue
  "#FFB6C2",    # Light pink
  "#B0E0C6"     # Powder blue
)

# thecolors

generate_general <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  fin <- data %>% 
    select(var1:var2) %>% 
    drop_na()
  
  media_rec <- fin %>% 
    gather() %>% 
    group_by(key) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value),
              n = n()) %>% 
    ungroup() %>% 
    mutate(varlabs = thevarlabs) %>% 
    mutate(varlabs = fct_reorder(varlabs, mean_val)) %>% 
    arrange(desc(varlabs))
  
  theorder <<- media_rec %>% distinct(varlabs) %>% pull(varlabs) 
  
  hchart(media_rec, "column", hcaes(x = varlabs, y = mean_val), name = "Mean") %>%
    hc_colors(thecolors) %>%
    hc_add_series(
      media_rec,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val),
      enableMouseTracking = TRUE,
      showInLegend = FALSE, name = ""
    ) %>% 
    highcharter::hc_plotOptions(
      column = list(
        colorByPoint = TRUE  # This ensures that each bar gets a different color
      ),
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    )  %>%
    hc_legend(enabled = FALSE)   %>%
    hc_caption(
      text = paste0("<em>N = ", nrow(fin), ". Errorbars show standard errors.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
}



by_age <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1:var2) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  # thevarlabs
  
  # mutate(varlabs = c("ChatGPT", "Midjourney")) %>% 
  fin <- data %>% 
    select(var1:var2, age_groups) %>% 
    drop_na()
  
  media_rec_ages <- fin %>% 
    gather(key, value, -age_groups) %>% 
    group_by(key, age_groups) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value),
              n = n()) %>% 
    ungroup() %>% 
    left_join(varlabdat) %>% 
    mutate(varlabs = factor(varlabs, levels = as.character(theorder))) %>%
    arrange(varlabs)%>% 
    arrange(age_groups)
  # mutate(varlabs = var_labels)
  # as.character(theorder)
  hchart(
    media_rec_ages, 
    "column",
    # hcaes(x = varlabs, y = mean_val),
    hcaes(x = varlabs, y = mean_val, group = age_groups),
    id = c("a", "b", "c", "d", "e", "f", "g")
  )  %>% 
    
    hc_add_series(
      media_rec_ages,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val,
            group = age_groups), 
      # hcaes(y = mean_val, x = key, low = mean_val - sd_val, high = mean_val + sd_val, group = Type),
      linkedTo = c("a", "b", "c", "d", "e", "f", "g"),
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) %>% 
    highcharter::hc_plotOptions(
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        # whiskerLength = 1,
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    ) %>% 
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      floating = TRUE,
      y = 6
    )   %>%
    hc_caption(
      text = paste0("<em>N = ", nrow(fin), ". Errorbars show standard errors.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
  
}


by_gender <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1:var2) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  fin <- data %>% 
    mutate(geslacht = sjmisc::to_label(geslacht)) %>% 
    select(var1:var2, geslacht) %>% 
    drop_na() %>% 
    mutate(geslacht = case_when(
      geslacht == "Vrouw" ~ "Woman",
      geslacht == "Anders" ~ "Other",
      T ~ geslacht
    ))
  
  media_rec_genders <- fin %>% 
    drop_na() %>% 
    gather(key, value, -geslacht) %>% 
    group_by(key, geslacht) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value),
              n = n()) %>%  
    ungroup() %>% 
    left_join(varlabdat) %>% 
    mutate(varlabs = factor(varlabs, levels = as.character(theorder))) %>%
    arrange(varlabs)
  
  hchart(
    media_rec_genders, 
    "column",
    # hcaes(x = varlabs, y = mean_val),
    hcaes(x = varlabs, y = mean_val, group = geslacht),
    id = c("a", "b", "c")
  ) %>%
    
    hc_add_series(
      media_rec_genders,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val,
            group = geslacht),
      # hcaes(y = mean_val, x = key, low = mean_val - sd_val, high = mean_val + sd_val, group = Type),
      linkedTo = c("a", "b", "c"),
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) %>% 
    highcharter::hc_plotOptions(
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        # whiskerLength = 1,
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    ) %>% 
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      floating = TRUE,
      y = 6
    ) %>%
    hc_caption(
      text = paste0("<em>N = ", nrow(fin), ". Errorbars show standard errors.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
  
}


by_edu <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1:var2) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  education_levels <- c("Primary (basisonderwijs)", 
                        "Pre-Vocational (vmbo)", 
                        "Secondary (havo/vwo)", 
                        "Vocational (mbo)", 
                        "Applied Sciences (hbo)", 
                        "University (wo)") %>% 
    tibble(eng = ., oplcat = c("basisonderwijs",
                               "vmbo",
                               "havo/vwo",
                               "mbo",
                               "hbo",
                               "wo"))
  
  
  fin <- data %>% 
    drop_na(oplcat) %>% 
    # count(oplcat) %>% 
    mutate(oplcat = sjmisc::to_label(oplcat)) %>% 
    left_join(education_levels) %>% 
    mutate(oplcat = eng) %>% 
    mutate(oplcat = fct_relevel(oplcat, c("Primary (basisonderwijs)", 
                                          "Pre-Vocational (vmbo)", 
                                          "Secondary (havo/vwo)", 
                                          "Vocational (mbo)", 
                                          "Applied Sciences (hbo)", 
                                          "University (wo)"))) %>% 
    select(var1:var2, oplcat) %>% 
    drop_na() 
  
  media_rec_edu <- fin %>% 
    gather(key, value, -oplcat) %>% 
    group_by(key, oplcat) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value),
              n = n()) %>%  
    ungroup() %>% 
    left_join(varlabdat) %>% 
    mutate(varlabs = factor(varlabs, levels = as.character(theorder))) %>%
    arrange(varlabs)
  # mutate(varlabs = var_labels)
  
  hchart(
    media_rec_edu, 
    "column",
    # hcaes(x = varlabs, y = mean_val),
    hcaes(x = varlabs, y = mean_val, group = oplcat),
    id = c("a", "b", "c", "d", "e", "f")
  ) %>%
    
    hc_add_series(
      media_rec_edu,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val,
            group = oplcat),
      # hcaes(y = mean_val, x = key, low = mean_val - sd_val, high = mean_val + sd_val, group = Type),
      linkedTo = c("a", "b", "c", "d", "e", "f"),
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) %>% 
    highcharter::hc_plotOptions(
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        # whiskerLength = 1,
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    ) %>% 
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      floating = TRUE,
      y = 6
    ) %>%
    hc_caption(
      text = paste0("<em>N = ", nrow(fin), ". Errorbars show standard errors.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
}


by_pol <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1:var2) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  fin <- data %>% 
    mutate(oplcat = sjmisc::to_label(pol_cat)) %>% 
    select(var1:var2, pol_cat) %>% 
    drop_na() 
  
  media_rec_pol <- fin %>% 
    gather(key, value, -pol_cat) %>% 
    group_by(key, pol_cat) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value),
              n = n()) %>% 
    ungroup() %>% 
    left_join(varlabdat) %>% 
    mutate(varlabs = factor(varlabs, levels = as.character(theorder))) %>%
    arrange(varlabs) %>% 
    mutate(pol_cat = fct_relevel(pol_cat, c("left", "center", "right")))
  # mutate(varlabs = var_labels)
  
  hchart(
    media_rec_pol, 
    "column",
    # hcaes(x = varlabs, y = mean_val),
    hcaes(x = varlabs, y = mean_val, group = pol_cat),
    id = c("a", "b", "c")
  ) %>%
    
    hc_add_series(
      media_rec_pol,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val,
            group = pol_cat),
      # hcaes(y = mean_val, x = key, low = mean_val - sd_val, high = mean_val + sd_val, group = Type),
      linkedTo = c("a", "b", "c"),
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) %>% 
    highcharter::hc_plotOptions(
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        # whiskerLength = 1,
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    ) %>% 
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      floating = TRUE,
      y = 6
    ) %>%
    hc_caption(
      text = paste0("<em>N = ", nrow(fin), ". Errorbars show standard errors. Politics category is based on 10-scale: 0-3: left; 4-6: center; 7-10: right.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
}





generate_general2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  

  media_rec <- data %>% 
    select(var1,var2,var3) %>% 
    gather() %>% 
    group_by(key) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value)) %>% 
    ungroup() %>% 
    mutate(varlabs = thevarlabs) %>% 
    mutate(varlabs = fct_reorder(varlabs, mean_val)) %>% 
    arrange(desc(varlabs))
  
  # theorder <<- media_rec %>% distinct(varlabs) %>% pull(varlabs) 
  
  hchart(
    media_rec, 
    "column",
    hcaes(x = varlabs, y = mean_val)
  ) %>%
    hc_add_series(
      media_rec,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val),
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) %>% 
    highcharter::hc_plotOptions(
      column = list(
        colorByPoint = TRUE  # This ensures that each bar gets a different color
      ),
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    ) 
  
}



by_age2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1,var2,var3) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  # thevarlabs
  
  # mutate(varlabs = c("ChatGPT", "Midjourney")) %>% 
  media_rec_ages <- data %>% 
    select(var1,var2,var3, age_groups) %>% 
    gather(key, value, -age_groups) %>% 
    group_by(key, age_groups) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value)) %>% 
    ungroup() %>% 
    left_join(varlabdat) %>% 
    mutate(varlabs = factor(varlabs, levels = as.character(theorder))) %>%
    arrange(varlabs)%>% 
    arrange(age_groups)
  # mutate(varlabs = var_labels)
  # as.character(theorder)
  hchart(
    media_rec_ages, 
    "column",
    # hcaes(x = varlabs, y = mean_val),
    hcaes(x = varlabs, y = mean_val, group = age_groups),
    id = c("a", "b", "c", "d", "e", "f", "g")
  )  %>% 
    
    hc_add_series(
      media_rec_ages,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val,
            group = age_groups), 
      # hcaes(y = mean_val, x = key, low = mean_val - sd_val, high = mean_val + sd_val, group = Type),
      linkedTo = c("a", "b", "c", "d", "e", "f", "g"),
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) %>% 
    highcharter::hc_plotOptions(
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        # whiskerLength = 1,
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    ) %>% 
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      floating = TRUE,
      y = 6
    )
  
}


by_gender2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1,var2,var3) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  media_rec_genders <- data %>% 
    # count(geslacht) %>% 
    mutate(geslacht = sjmisc::to_label(geslacht)) %>% 
    select(var1,var2,var3, geslacht) %>% 
    gather(key, value, -geslacht) %>% 
    group_by(key, geslacht) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value)) %>% 
    ungroup() %>% 
    left_join(varlabdat) %>% 
    mutate(varlabs = factor(varlabs, levels = as.character(theorder))) %>%
    arrange(varlabs)
  
  hchart(
    media_rec_genders, 
    "column",
    # hcaes(x = varlabs, y = mean_val),
    hcaes(x = varlabs, y = mean_val, group = geslacht),
    id = c("a", "b", "c")
  ) %>%
    
    hc_add_series(
      media_rec_genders,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val,
            group = geslacht),
      # hcaes(y = mean_val, x = key, low = mean_val - sd_val, high = mean_val + sd_val, group = Type),
      linkedTo = c("a", "b", "c"),
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) %>% 
    highcharter::hc_plotOptions(
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        # whiskerLength = 1,
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    ) %>% 
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      floating = TRUE,
      y = 6
    )
  
}


by_edu2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1,var2,var3) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  
  media_rec_edu <- data %>% 
    drop_na(oplcat) %>% 
    # count(oplcat) %>% 
    mutate(oplcat = sjmisc::to_label(oplcat)) %>% 
    select(var1,var2,var3, oplcat) %>% 
    gather(key, value, -oplcat) %>% 
    group_by(key, oplcat) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value)) %>% 
    ungroup() %>% 
    left_join(varlabdat) %>% 
    mutate(varlabs = factor(varlabs, levels = as.character(theorder))) %>%
    arrange(varlabs)
  # mutate(varlabs = var_labels)
  
  hchart(
    media_rec_edu, 
    "column",
    # hcaes(x = varlabs, y = mean_val),
    hcaes(x = varlabs, y = mean_val, group = oplcat),
    id = c("a", "b", "c", "d", "e", "f")
  ) %>%
    
    hc_add_series(
      media_rec_edu,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val,
            group = oplcat),
      # hcaes(y = mean_val, x = key, low = mean_val - sd_val, high = mean_val + sd_val, group = Type),
      linkedTo = c("a", "b", "c", "d", "e", "f"),
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) %>% 
    highcharter::hc_plotOptions(
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        # whiskerLength = 1,
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    ) %>% 
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      floating = TRUE,
      y = 6
    )
}


by_pol2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1,var2,var3) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  
  media_rec_edu <- data %>% 
    drop_na(pol_cat) %>% 
    # count(oplcat) %>% 
    mutate(oplcat = sjmisc::to_label(pol_cat)) %>% 
    select(var1,var2,var3, pol_cat) %>% 
    gather(key, value, -pol_cat) %>% 
    group_by(key, pol_cat) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value)) %>% 
    ungroup() %>% 
    left_join(varlabdat) %>% 
    mutate(varlabs = factor(varlabs, levels = as.character(theorder))) %>%
    arrange(varlabs) %>% 
    mutate(pol_cat = fct_relevel(pol_cat, c("left", "center", "right")))
  # mutate(varlabs = var_labels)
  
  hchart(
    media_rec_edu, 
    "column",
    # hcaes(x = varlabs, y = mean_val),
    hcaes(x = varlabs, y = mean_val, group = pol_cat),
    id = c("a", "b", "c")
  ) %>%
    
    hc_add_series(
      media_rec_edu,
      "errorbar", 
      hcaes(y = mean_val, 
            x = varlabs, low = mean_val - sd_val, 
            high = mean_val + sd_val,
            group = pol_cat),
      # hcaes(y = mean_val, x = key, low = mean_val - sd_val, high = mean_val + sd_val, group = Type),
      linkedTo = c("a", "b", "c"),
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) %>% 
    highcharter::hc_plotOptions(
      series = list(
        tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>')
      ),
      errorbar = list(
        color = "black", 
        # whiskerLength = 1,
        stemWidth = 1
      ) 
    ) %>% 
    highcharter::hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(
      title = list(text = thelabel), 
      max = 7, min = 1
    ) %>% 
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      floating = TRUE,
      y = 6
    )
}


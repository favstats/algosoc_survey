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
    # select(var1:var2, weight) %>% 
    drop_na()
  
  media_rec <- fin %>% 
    # gather(key, value, -weight) %>% 
    gather(key, value) %>% 
    group_by(key) %>% 
    summarize(mean_val = mean(value, na.rm =T),
    # summarize(mean_val = weighted.mean(value, w = weight, na.rm =T),
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten.</em>"),
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
    # select(var1:var2, age_groups, weight) %>% 
    drop_na()
  
  media_rec_ages <- fin %>% 
    gather(key, value, -age_groups) %>% 
    # gather(key, value, -age_groups, -weight) %>% 
    group_by(key, age_groups) %>% 
    summarize(mean_val = mean(value, na.rm =T),
    # summarize(mean_val = weighted.mean(value, w = weight,na.rm =T),
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten.</em>"),
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
    drop_na() #%>% 
    # mutate(geslacht = case_when(
    #   geslacht == "Vrouw" ~ "Woman",
    #   geslacht == "Anders" ~ "Other",
    #   T ~ geslacht
    # )) %>% 
    # mutate(geslacht = fct_relevel(geslacht, c("Man", "Woman", "Other"))) %>% 
    # arrange(geslacht)
  
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
  
}


by_edu <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1:var2) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  # 
  # education_levels <- c("Primary (basisonderwijs)", 
  #                       "Pre-Vocational (vmbo)", 
  #                       "Secondary (havo/vwo)", 
  #                       "Vocational (mbo)", 
  #                       "Applied Sciences (hbo)", 
  #                       "University (wo)") %>% 
  #   tibble(eng = ., oplcat = c("basisonderwijs",
  #                              "vmbo",
  #                              "havo/vwo",
  #                              "mbo",
  #                              "hbo",
  #                              "wo"))
  # 
  
  fin <- data %>% 
    drop_na(oplcat) %>% 
    # count(oplcat) %>% 
    mutate(oplcat = sjmisc::to_label(oplcat)) %>% 
    # left_join(education_levels) %>% 
    # mutate(oplcat = eng) %>% 
    # mutate(oplcat = fct_relevel(oplcat, c("Primary (basisonderwijs)", 
    #                                       "Pre-Vocational (vmbo)", 
    #                                       "Secondary (havo/vwo)", 
    #                                       "Vocational (mbo)", 
    #                                       "Applied Sciences (hbo)", 
    #                                       "University (wo)"))) %>% 
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten.</em>"),
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten. Politics category is based on 10-scale: 0-3: left; 4-6: center; 7-10: right.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
}




generate_general2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  fin <- data %>% 
    select(var1, var2, var3) %>% 
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
}



by_age2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1, var2, var3) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  # thevarlabs
  
  # mutate(varlabs = c("ChatGPT", "Midjourney")) %>% 
  fin <- data %>% 
    select(var1, var2, var3, age_groups) %>% 
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
  
}


by_gender2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1, var2, var3) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  fin <- data %>% 
    mutate(geslacht = sjmisc::to_label(geslacht)) %>% 
    select(var1, var2, var3, geslacht) %>% 
    drop_na() #%>% 
    # mutate(geslacht = case_when(
    #   geslacht == "Vrouw" ~ "Woman",
    #   geslacht == "Anders" ~ "Other",
    #   T ~ geslacht
    # )) %>% 
    # mutate(geslacht = fct_relevel(geslacht, c("Man", "Woman", "Other"))) %>% 
    # arrange(geslacht)
  
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
  
}


by_edu2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1, var2, var3) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  # education_levels <- c("Primary (basisonderwijs)", 
  #                       "Pre-Vocational (vmbo)", 
  #                       "Secondary (havo/vwo)", 
  #                       "Vocational (mbo)", 
  #                       "Applied Sciences (hbo)", 
  #                       "University (wo)") %>% 
  #   tibble(eng = ., oplcat = c("basisonderwijs",
  #                              "vmbo",
  #                              "havo/vwo",
  #                              "mbo",
  #                              "hbo",
  #                              "wo"))
  
  
  fin <- data %>% 
    drop_na(oplcat) %>% 
    # count(oplcat) %>% 
    mutate(oplcat = sjmisc::to_label(oplcat)) %>% 
    # left_join(education_levels) %>% 
    # mutate(oplcat = eng) %>% 
    # mutate(oplcat = fct_relevel(oplcat, c("Primary (basisonderwijs)", 
    #                                       "Pre-Vocational (vmbo)", 
    #                                       "Secondary (havo/vwo)", 
    #                                       "Vocational (mbo)", 
    #                                       "Applied Sciences (hbo)", 
    #                                       "University (wo)"))) %>% 
    select(var1, var2, var3, oplcat) %>% 
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
}


by_pol2 <- function(data, var1, var2, var3, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1, var2, var3) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  fin <- data %>% 
    mutate(oplcat = sjmisc::to_label(pol_cat)) %>% 
    select(var1, var2, var3, pol_cat) %>% 
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
      text = paste0("<em>N = ", nrow(fin), ". Foutbalken tonen standaardfouten. Politics category is based on 10-scale: 0-3: left; 4-6: center; 7-10: right.</em>"),
      align = "right",  # Change to "left" or "right" as needed
      style = list(fontSize = '10px', color = 'lightgrey')  # You can adjust the font size here and color if needed
    )
}

plot_to_html <- function(plot) {
  if (is.null(plot)) {
    cat("NULL plot detected!\n")
    return("NULL plot")
  }
  as.character(tags$div(
    class = "flex-item2",
    htmltools::tagList(plot)
  ))
}


generate_quarto_elements <- function(list_of_plots, plots_per_page = 2) {

  
  num_plots <<- length(list_of_plots)
  num_pages <<- ceiling(num_plots / plots_per_page)
  
  # for (page in 1:num_pages) {
  #   start_index <- (page - 1) * plots_per_page + 1
  #   end_index <- min(page * plots_per_page, num_plots)
  #   
  #   plots_for_page <- list_of_plots[start_index:end_index]
  #   
  #   html_content <<- paste(
  #     '<table style="width:100%; border-spacing: 1px;">',
  #     '<tr>',
  #     paste(
  #       lapply(plots_for_page, function(plot) {
  #         plot_html <- plot_to_html(plot)
  #         if (str_detect(plot_html, "NULL")) {
  #           cat("NULL detected in plot HTML!\n")
  #         }
  #         paste0('<td style="width:50%; vertical-align: top;">', plot_html, '</td>')
  #       }),
  #       collapse = "\n"
  #     ),
  #     '</tr>',
  #     '</table>'
  #   ) 
  #   
  #   istherenull2 <<- str_detect(html_content, "NULL")
  #   
  #   if(!is.null(html_content)){
  #     cat(paste0("### ", page, "\n"))
  #     cat(html_content)
  #   }
  # }
  page <- 0
  list_of_plots %>% 
    walk(~{
      page <<- page + 1
      # if(!is.null(html_content)){
      cat(paste0("### ", page, "\n"))
      cat(plot_to_html(.x))
      # }
    })
  
}

viz_general <- function(vars, var_labels, categories_dat, grpvar = NULL, groups = F) {
  if(groups){
    varlabdat <- data %>%
      select(vars) %>%
      names() %>%
      tibble(key = ., varlabs = var_labels)
    
    fin <- data %>%
      select(vars, contains("thevar"), thevar = grpvar) %>%
      drop_na() %>% 
      set_names(c(thevarlabs, "thevar"))
    
    media_rec_ages <- fin %>%
      gather(key, value,-thevar) %>%
      mutate(
        value = case_when(
          value %in% 1:3 ~ categories_dat[3],
          value == 4 ~ categories_dat[2],
          value %in% 5:7 ~ categories_dat[1]
        )
      ) %>%
      count(key, thevar, value) %>%
      group_by(key, thevar) %>%
      mutate(perc = n / sum(n) * 100) %>%
      ungroup()
  } else {
    fin <- data %>% 
      select(vars) %>% 
      drop_na() %>% 
      set_names(var_labels)
    
    media_rec <- fin %>% 
      gather(key, value) %>% 
      mutate(value = case_when(
        value %in% 1:3 ~ categories_dat[3],
        value == 4 ~ categories_dat[2],
        value %in% 5:7 ~ categories_dat[1]
      )) %>% 
      group_by(key) %>% 
      count(value) %>% 
      ungroup()  %>%
      mutate(value = fct_relevel(value, categories_dat)) %>%
      group_by(key) %>%
      mutate(perc = n/sum(n)*100) %>%
      ungroup()
    
    theorder <<- media_rec %>% 
      filter(value == categories_dat[3]) %>% 
      arrange(desc(perc)) %>% 
      pull(key)
  }
  
  if(groups){
    hc <<- media_rec_ages %>% 
      mutate(key = fct_relevel(key, theorder)) %>%
      arrange(key) %>%
      pull(key) %>% 
      levels() %>% 
      map(~{
        hc <- media_rec_ages %>%
          filter(key == .x) %>%
          mutate(value = fct_relevel(value, categories_dat)) %>%
          hchart("bar", hcaes(x = thevar, y = perc, group = value)) %>%
          hc_title(text = .x) %>%
          hc_chart(style = list(width = "100%", height = "100%"))  %>% 
          hc_plotOptions(bar = list(stacking = "percent")) %>%
          hc_yAxis(title = list(text = ""), max = 100) %>%
          hc_colors(colors) %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_legend(enabled = TRUE) %>%
          hc_plotOptions(
            column = list(
              colorByPoint = TRUE
            ),
            series = list(
              tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}%</b><br/>'),
              dataLabels = list(
                enabled = TRUE,
                format = '{y:.0f}%',
                color = 'white',
                style = list(
                  fontSize = '10px',
                  fontWeight = 'bold'
                ),
                inside = TRUE,
                verticalAlign = 'middle',
                align = 'center',
                filter = list(
                  property = 'y',
                  operator = '>',
                  value = 5
                )
              )
            )
          ) %>%
          hc_caption(
            text = paste0("<em>N = ", nrow(fin), ".</em>"),
            align = "right",
            style = list(fontSize = '10px', color = 'lightgrey')
          ) %>%
          hc_exporting(
            enabled = TRUE,
            buttons = list(contextButton = list(menuItems = myMenuItems))
          )
        
        hc$x$hc_opts$series <- lapply(hc$x$hc_opts$series, function(series) {
          if (series$name == categories_dat[3]) {
            series$legendIndex <- 0
          } else if (series$name == categories_dat[2]) {
            series$legendIndex <- 1
          } else if (series$name == categories_dat[1]) {
            series$legendIndex <- 2
          }
          return(series)
        })
        
        return(hc)
      })
    
  } else {
    hc <- media_rec %>%
      mutate(key = fct_relevel(key, theorder)) %>%
      arrange(key) %>%
      hchart("bar", hcaes(x = key, y = perc, group = value))  %>% 
      hc_plotOptions(bar = list(stacking = "percent")) %>%
      hc_yAxis(title = list(text = ""), max = 100) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(
        column = list(
          colorByPoint = TRUE
        ),
        series = list(
          tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}%</b><br/>'),
          dataLabels = list(
            enabled = TRUE,
            format = '{y:.0f}%',
            color = 'white',
            style = list(
              fontSize = '12px',
              fontWeight = 'bold'
            ),
            inside = TRUE,
            verticalAlign = 'middle',
            align = 'center',
            filter = list(
              property = 'y',
              operator = '>',
              value = 5
            )
          )
        )
      ) %>%
      hc_caption(
        text = paste0("<em>N = ", nrow(fin), ".</em>"),
        align = "right",
        style = list(fontSize = '10px', color = 'lightgrey')
      ) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(contextButton = list(menuItems = myMenuItems))
      )
    
    hc$x$hc_opts$series <- lapply(hc$x$hc_opts$series, function(series) {
      if (series$name == categories_dat[3]) {
        series$legendIndex <- 0
      } else if (series$name == categories_dat[2]) {
        series$legendIndex <- 1
      } else if (series$name == categories_dat[1]) {
        series$legendIndex <- 2
      }
      return(series)
    })
  }
  
  if(groups){
    generate_quarto_elements(hc, plots_per_page = 2)
  } else {
    return(hc)
  }
  
  
}

vecc <- function(name, num) {
  paste0(name, num)
}



generate_general <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  media_rec <- data %>% 
    select(var1:var2) %>% 
    gather() %>% 
    group_by(key) %>% 
    summarize(mean_val = mean(value, na.rm =T),
              sd_val = std.error(value)) %>% 
    ungroup() %>% 
    mutate(varlabs = thevarlabs) %>% 
    mutate(varlabs = fct_reorder(varlabs, mean_val)) %>% 
    arrange(desc(varlabs))
  
  theorder <<- media_rec %>% distinct(varlabs) %>% pull(varlabs) 
  
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



by_age <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1:var2) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  # thevarlabs
  
  # mutate(varlabs = c("ChatGPT", "Midjourney")) %>% 
  media_rec_ages <- data %>% 
    select(var1:var2, age_groups) %>% 
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


by_gender <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1:var2) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  media_rec_genders <- data %>% 
    # count(geslacht) %>% 
    mutate(geslacht = sjmisc::to_label(geslacht)) %>% 
    select(var1:var2, geslacht) %>% 
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


by_edu <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1:var2) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  
  media_rec_edu <- data %>% 
    drop_na(oplcat) %>% 
    # count(oplcat) %>% 
    mutate(oplcat = sjmisc::to_label(oplcat)) %>% 
    select(var1:var2, oplcat) %>% 
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


by_pol <- function(data, var1, var2, thevarlabs, thelabel = "<- 1: never    -    7: very often ->") {
  
  varlabdat <- data %>% 
    select(var1:var2) %>% 
    names() %>% 
    tibble(key = ., varlabs = thevarlabs)
  
  
  media_rec_edu <- data %>% 
    drop_na(pol_cat) %>% 
    # count(oplcat) %>% 
    mutate(oplcat = sjmisc::to_label(pol_cat)) %>% 
    select(var1:var2, pol_cat) %>% 
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


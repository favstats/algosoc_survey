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



std.error <- function(x) sd(x, na.rm =T)/sqrt(length(x))


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
    # varlabdat <- data %>%
    #   select(vars) %>%
    #   names() %>%
    #   tibble(key = ., varlabs = var_labels)
    
    fin <- data %>%
      select(vars, thevar = grpvar) %>%
      drop_na() %>% 
      set_names(c(var_labels, "thevar"))
    
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


polvals <- read_sav("../../cv23o_EN_1.0p.sav") %>% 
  mutate(left_right = cv23o101) #%>% 
# mutate(left_right = ifelse(is.na(left_right), "99", left_right))

data <- read_sav("../../L_AlgoSoc_wave1_1.0p.sav") %>% #table()
  mutate(age_groups = case_when(
    leeftijd %in% 16:17 ~ "16-17",
    leeftijd %in% 18:24 ~ "18-24",
    leeftijd %in% 25:34 ~ "25-34",
    leeftijd %in% 35:44 ~ "35-44",
    leeftijd %in% 45:54 ~ "45-54",
    leeftijd %in% 55:64 ~ "55-64",
    leeftijd %in% 65:150 ~ "65+",
  )) %>% 
  left_join(polvals %>% select(nomem_encr, left_right)) %>% 
  mutate(pol_cat = case_when(
    left_right %in% 0:3 ~ "Left",
    left_right %in% 4:6 ~ "Center",
    left_right %in% 7:10 ~ "Right"
  )) %>% 
  mutate(geslacht = sjmisc::to_label(geslacht)) %>% 
  mutate(geslacht = case_when(
    geslacht == "Vrouw" ~ "Woman",
    geslacht == "Anders" ~ "Other",
    T ~ geslacht
  )) %>% 
  mutate(geslacht = fct_relevel(geslacht, c("Man", "Woman", "Other"))) %>% 
  arrange(geslacht)  %>% 
  mutate(oplcat = sjmisc::to_label(oplcat)) %>% 
  left_join(education_levels) %>%
  mutate(oplcat = eng) %>%
  mutate(oplcat = fct_relevel(oplcat, c("Primary (basisonderwijs)",
                                        "Pre-Vocational (vmbo)",
                                        "Secondary (havo/vwo)",
                                        "Vocational (mbo)",
                                        "Applied Sciences (hbo)",
                                        "Left (wo)"))) %>% 
  mutate(pol_cat = fct_relevel(pol_cat, c("left", "Center", "Right"))) %>% 
  mutate(important1 = sjmisc::to_label(ADSVR_1))

colors <- c("#2ca02c", "#1f77b4", "#ff7f0e") # Add more colors if needed

# Create a stacked bar chart with highcharter
myMenuItems <- c("downloadPNG", "downloadJPEG", "downloadSVG", "downloadPDF")



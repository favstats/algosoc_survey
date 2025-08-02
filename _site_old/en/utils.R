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
  html <- paste0(
    '<div class="flex-item2">',
    htmltools::renderTags(plot)$html,  # Render the plot as HTML
    '<div style="text-align: center">',
    '<span style="display: inline-block; background-color: #f0f2f5; padding: 5px 10px; border-radius: 10px; color: #6c757d;">Choose Item, Grouping (e.g. by age, gender), and Timeframe:</span>',
    '</div>',
    '</div>'
  )
  html
}


generate_quarto_elements <- function(list_of_plots, plots_per_page = 2) {
  num_plots <<- length(list_of_plots)
  num_pages <<- ceiling(num_plots / plots_per_page)
  page <- 0
  list_of_plots %>% 
    walk(~{
      page <<- page + 1
      cat(paste0("### Item ", page, "\n"))
      cat(plot_to_html(.x))
    })
}


# Load the necessary libraries at the top of your R script/chunk
library(bslib)
library(htmltools)

library(htmltools)

library(htmltools)
library(purrr)

create_tabs_with_nav <- function(plots, title = NULL) {
  
  # Part 1: Manually load plot dependencies to make charts visible
  dependencies <- map(plots, ~ {
    if ("htmlwidget" %in% class(.x)) {
      tryCatch(
        htmlwidgets::getDependency(.x),
        error = function(e) { NULL }
      )
    }
  })
  cat(as.character(renderDependencies(dependencies)))
  
  # Part 2: Your Title and Navigation Buttons
  viewer_id <- paste0("viewer-", sample.int(1e6, 1))
  cat(sprintf(
    '
    <div class="d-flex justify-content-between align-items-center my-3">
      <button id="%s-prev" class="btn btn-sm btn-light border">← Previous</button>
      <h5 class="mb-0 fw-bold">%s</h5>
      <button id="%s-next" class="btn btn-sm btn-light border">Next →</button>
    </div>
    ',
    viewer_id, htmlEscape(title), viewer_id
  ))
  
  # Part 3: Your Original Tab Generation Logic
  cat(paste0('<div id="', viewer_id, '">'))
  
  # This is your original loop creating ### headings and plot HTML
  walk2(plots, seq_along(plots), ~ {
    cat(paste0("\n\n### Question ", .y, "\n\n"))
    plot_html <- as.character(tags$div(class = "flex-item2", .x))
    cat(plot_html)
  })
  
  cat('</div>')
  
  # Part 4: JavaScript to Power the Buttons
  cat(sprintf(
    '
    <script>
    document.addEventListener("DOMContentLoaded", () => {
      const viewerId = "%s";
      const container = document.getElementById(viewerId);
      if (!container) return;

      const prevBtn = document.getElementById(`${viewerId}-prev`);
      const nextBtn = document.getElementById(`${viewerId}-next`);
      // Find the tabs Quarto created inside the specific container
      const tabLinks = container.parentElement.querySelectorAll(".nav-tabs .nav-link");

      if (!prevBtn || !nextBtn || tabLinks.length === 0) return;

      const updateButtons = () => {
        const activeTab = container.parentElement.querySelector(".nav-tabs .nav-link.active");
        const currentIndex = Array.from(tabLinks).indexOf(activeTab);
        prevBtn.disabled = (currentIndex === 0);
        nextBtn.disabled = (currentIndex === tabLinks.length - 1);
      };

      prevBtn.addEventListener("click", () => {
        const currentIndex = Array.from(tabLinks).indexOf(container.parentElement.querySelector(".nav-tabs .nav-link.active"));
        if (currentIndex > 0) {
          bootstrap.Tab.getOrCreateInstance(tabLinks[currentIndex - 1]).show();
        }
      });

      nextBtn.addEventListener("click", () => {
        const currentIndex = Array.from(tabLinks).indexOf(container.parentElement.querySelector(".nav-tabs .nav-link.active"));
        if (currentIndex < tabLinks.length - 1) {
          bootstrap.Tab.getOrCreateInstance(tabLinks[currentIndex + 1]).show();
        }
      });
      
      tabLinks.forEach(tab => tab.addEventListener("shown.bs.tab", updateButtons));
      updateButtons();
    });
    </script>
    ',
    viewer_id
  ))
}

generate_quarto_dropdown <- function(list_of_plots) {
  ids <- paste0("chart_", seq_along(list_of_plots))
  titles <- map_chr(list_of_plots, ~ .x$x$hc_opts$title$text %||% "Untitled")
  
  # Dropdown menu
  cat('<label for="chartSelector"><strong>Select Chart:</strong></label><br>')
  cat('<select id="chartSelector" onchange="showChart(this.value)">\n')
  walk2(ids, titles, \(id, title) {
    cat(sprintf('<option value="%s">%s</option>\n', id, htmltools::htmlEscape(title)))
  })
  cat('</select>\n<br><br>')
  
  # Chart containers
  walk2(list_of_plots, ids, \(plot, id) {
    display <- if (id == ids[1]) "block" else "none"
    cat(sprintf('<div id="%s" style="display: %s;">\n', id, display))
    cat(as.character(highcharter::renderHighchart(plot)))
    cat('</div>\n')
  })
  
  # JavaScript toggle function
  cat("
<script>
function showChart(id) {
  const charts = document.querySelectorAll('div[id^=\"chart_\"]');
  charts.forEach(c => c.style.display = 'none');
  document.getElementById(id).style.display = 'block';
}
</script>
")
}

viz_general <- function(vars, var_labels, categories_dat, grpvar = NULL, groups = F, data = dat) {
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
    theorder <- var_labels
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
        
        return(hc %>%
                 hc_size(height = 650) )
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
    return(hc%>%
             hc_size(height = 650) )
  }
  
  
}

vecc <- function(name, num) {
  paste0(name, num)
}


polvals <- read_sav("../../cv23o_EN_1.0p.sav") %>% 
  mutate(left_right = cv23o101) #%>% 
# mutate(left_right = ifelse(is.na(left_right), "99", left_right))

dat <- read_sav("../../L_AlgoSoc_wave1_1.0p.sav") %>% #table()
  # mutate(age_groups = case_when(
  #   leeftijd %in% 16:17 ~ "16-17",
  #   leeftijd %in% 18:24 ~ "18-24",
  #   leeftijd %in% 25:34 ~ "25-34",
  #   leeftijd %in% 35:44 ~ "35-44",
  #   leeftijd %in% 45:54 ~ "45-54",
  #   leeftijd %in% 55:64 ~ "55-64",
  #   leeftijd %in% 65:150 ~ "65+",
  # )) %>% 
  mutate(age_groups = case_when(
    leeftijd %in% 16:29 ~ "16-29",
    leeftijd %in% 30:49 ~ "30-49",
    leeftijd %in% 50:64 ~ "50-64",
    leeftijd %in% 65:150 ~ "65+"
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
  # mutate(oplcat = fct_relevel(oplcat, c("Primary (basisonderwijs)",
  #                                       "Pre-Vocational (vmbo)",
  #                                       "Secondary (havo/vwo)",
  #                                       "Vocational (mbo)",
  #                                       "Applied Sciences (hbo)",
  #                                       "Left (wo)"))) %>% 
  mutate(oplcat = case_when(
    oplcat %in% c("Primary (basisonderwijs)", "Pre-Vocational (vmbo)") ~ "Lower (primary or vmbo)",
    oplcat %in% c("Secondary (havo/vwo)", "Vocational (mbo)") ~ "Middle (havo/vwo or mbo)",
    oplcat %in% c("Applied Sciences (hbo)", "Left (wo)") ~ "Higher (hbo or university)"
  )) %>%
  mutate(oplcat = factor(oplcat, levels = c(
    "Lower (primary or vmbo)",
    "Middle (havo/vwo or mbo)",
    "Higher (hbo or university)"
  ))) %>% 
  mutate(pol_cat = fct_relevel(pol_cat, c("Left", "Center", "Right"))) %>% 
  mutate(important1 = sjmisc::to_label(ADSVR_1)) %>% 
  mutate_if(is.numeric, ~ifelse(.x < 0, NA, .x))


dat2 <- read_sav("../../L_AlgoSoc_wave2_1.0p_PUBLIC.sav") %>% #table()
  # mutate(age_groups = case_when(
  #   leeftijd %in% 16:17 ~ "16-17",
  #   leeftijd %in% 18:24 ~ "18-24",
  #   leeftijd %in% 25:34 ~ "25-34",
  #   leeftijd %in% 35:44 ~ "35-44",
  #   leeftijd %in% 45:54 ~ "45-54",
  #   leeftijd %in% 55:64 ~ "55-64",
  #   leeftijd %in% 65:150 ~ "65+",
  # )) %>% 
  mutate(age_groups = case_when(
    leeftijd %in% 16:29 ~ "16-29",
    leeftijd %in% 30:49 ~ "30-49",
    leeftijd %in% 50:64 ~ "50-64",
    leeftijd %in% 65:150 ~ "65+"
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
  # mutate(oplcat = fct_relevel(oplcat, c("Primary (basisonderwijs)",
  #                                       "Pre-Vocational (vmbo)",
  #                                       "Secondary (havo/vwo)",
  #                                       "Vocational (mbo)",
  #                                       "Applied Sciences (hbo)",
  #                                       "Left (wo)"))) %>% 
  mutate(oplcat = case_when(
    oplcat %in% c("Primary (basisonderwijs)", "Pre-Vocational (vmbo)") ~ "Lower (primary or vmbo)",
    oplcat %in% c("Secondary (havo/vwo)", "Vocational (mbo)") ~ "Middle (havo/vwo or mbo)",
    oplcat %in% c("Applied Sciences (hbo)", "Left (wo)") ~ "Higher (hbo or university)"
  )) %>%
  mutate(oplcat = factor(oplcat, levels = c(
    "Lower (primary or vmbo)",
    "Middle (havo/vwo or mbo)",
    "Higher (hbo or university)"
  ))) %>% 
  mutate(pol_cat = fct_relevel(pol_cat, c("Left", "Center", "Right"))) %>%
  mutate(important1 = sjmisc::to_label(ADSVR_1)) %>% 
  mutate_if(is.numeric, ~ifelse(.x < 0, NA, .x))


dat3 <- read_sav("../../L_AlgoSoc_wave3_1.0p.sav") %>% #table()
  # mutate(age_groups = case_when(
  #   leeftijd %in% 16:17 ~ "16-17",
  #   leeftijd %in% 18:24 ~ "18-24",
  #   leeftijd %in% 25:34 ~ "25-34",
  #   leeftijd %in% 35:44 ~ "35-44",
  #   leeftijd %in% 45:54 ~ "45-54",
  #   leeftijd %in% 55:64 ~ "55-64",
  #   leeftijd %in% 65:150 ~ "65+",
  # )) %>% 
  mutate(age_groups = case_when(
    leeftijd %in% 16:29 ~ "16-29",
    leeftijd %in% 30:49 ~ "30-49",
    leeftijd %in% 50:64 ~ "50-64",
    leeftijd %in% 65:150 ~ "65+"
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
  # mutate(oplcat = fct_relevel(oplcat, c("Primary (basisonderwijs)",
  #                                       "Pre-Vocational (vmbo)",
  #                                       "Secondary (havo/vwo)",
  #                                       "Vocational (mbo)",
  #                                       "Applied Sciences (hbo)",
  #                                       "Left (wo)"))) %>% 
  mutate(oplcat = case_when(
    oplcat %in% c("Primary (basisonderwijs)", "Pre-Vocational (vmbo)") ~ "Lower (primary or vmbo)",
    oplcat %in% c("Secondary (havo/vwo)", "Vocational (mbo)") ~ "Middle (havo/vwo or mbo)",
    oplcat %in% c("Applied Sciences (hbo)", "Left (wo)") ~ "Higher (hbo or university)"
  )) %>%
  mutate(oplcat = factor(oplcat, levels = c(
    "Lower (primary or vmbo)",
    "Middle (havo/vwo or mbo)",
    "Higher (hbo or university)"
  ))) %>% 
  mutate(pol_cat = fct_relevel(pol_cat, c("Left", "Center", "Right"))) %>% 
  mutate(important1 = sjmisc::to_label(ADSVR_1)) %>% 
  mutate_if(is.numeric, ~ifelse(.x < 0, NA, .x))



# 2. The Complete Visualization Function
viz_wave_change3 <- function(data,
                             vars,
                             var_labels,
                             categories_dat,
                             wave_var,
                             group_by_var = NULL,
                             facet_by_group = FALSE,
                             display_range = NULL, # ## NEW ## Specify a range to display, e.g., 5:7
                             range1 = 1:2,
                             range2 = 3,
                             range3 = 4:5,
                             weight_var = NULL,
                             y_axis_label = "Percentage") {
  
  # --- Data Preparation (Largely unchanged) ---
  group_by_sym <- if (!is.null(group_by_var)) sym(group_by_var) else NULL
  wave_sym <- sym(wave_var)
  
  cols_to_select <- c(vars, wave_var)
  if (!is.null(group_by_var)) {
    cols_to_select <- c(cols_to_select, group_by_var)
  }
  
  fin <- data %>%
    select(all_of(cols_to_select)) %>%
    drop_na()
  
  if (!is.null(group_by_var)) {
    fin <- fin %>%
      rename(wave = {{ wave_sym }}, group_col = {{ group_by_sym }}) %>%
      set_names(c(var_labels, "wave", "group_col"))
  } else {
    fin <- fin %>%
      rename(wave = {{ wave_sym }}) %>%
      set_names(c(var_labels, "wave"))
  }
  
  media_wave_base <- fin %>%
    pivot_longer(
      cols = all_of(var_labels),
      names_to = "key",
      values_to = "value"
    ) %>%
    mutate(
      value = case_when(
        value %in% range1 ~ categories_dat[3],
        value %in% range2 ~ categories_dat[2],
        value %in% range3 ~ categories_dat[1],
        TRUE ~ NA_character_
      ),
      value = fct_relevel(value, categories_dat)
    ) %>%
    drop_na(value)
  
  if (!is.null(group_by_var)) {
    media_wave <- media_wave_base %>%
      count(group_col, wave, key, value) %>%
      group_by(group_col, wave, key) %>%
      mutate(perc = n / sum(n) * 100) %>%
      ungroup()
  } else {
    media_wave <- media_wave_base %>%
      count(wave, key, value) %>%
      group_by(wave, key) %>%
      mutate(perc = n / sum(n) * 100) %>%
      ungroup()
  }
  
  # ## NEW LOGIC BLOCK ##
  # If a specific display_range is requested, filter the data to only that category.
  if (!is.null(display_range)) {
    # First, determine which text category corresponds to the numeric range provided.
    # Using identical() ensures the ranges match exactly.
    target_category <- case_when(
      identical(sort(display_range), sort(range1)) ~ categories_dat[3],
      identical(sort(display_range), sort(range2)) ~ categories_dat[2],
      identical(sort(display_range), sort(range3)) ~ categories_dat[1],
      TRUE ~ {
        warning("`display_range` does not exactly match any predefined ranges (range1, range2, range3).")
        NA_character_
      }
    )
    
    # If a valid category was found, filter the main dataframe.
    if (!is.na(target_category)) {
      media_wave <- media_wave %>% filter(value == target_category)
    }
  }
  # ## END OF NEW LOGIC BLOCK ##
  
  
  latest_wave <- max(media_wave$wave)
  theorder <- media_wave %>%
    { if (!is.null(group_by_var)) group_by(., wave, key, value) %>% summarise(n = sum(n)) %>% group_by(wave, key) %>% mutate(perc = n/sum(n)*100) else . } %>%
    # Use the first category as the default for ordering if only one is present
    filter(wave == latest_wave, value == (if(!is.null(display_range)) head(media_wave$value, 1) else categories_dat[3])) %>%
    arrange(desc(perc)) %>%
    pull(key) %>%
    unique()
  
  theorder <- var_labels
  
  max_perc <- media_wave %>% arrange(desc(perc)) %>% pull(perc) %>% .[1]
  
  if(max_perc < 50){
    max_perc <- 50
  } else {
    max_perc <- 100
  }
  
  media_wave <- media_wave %>%
    mutate(
      wave = case_when(
        wave == 1 ~ "March 2024",
        wave == 2 ~ "December 2024",
        wave == 3 ~ "May 2025"
      ) 
    ) |> mutate(wave = factor(wave, levels = c("March 2024","December 2024","May 2025")))
  
  # --- Plotting Logic (Modified to handle single-category data) ---
  hcs <- map(theorder, function(varkey) {
    
    # Logic for Faceting
    if (facet_by_group && !is.null(group_by_var)) {
      question_data <- media_wave %>% filter(key == varkey)
      group_levels <- unique(question_data$group_col)
      
      facet_plots <- map(group_levels, function(g_level) {
        plot_data_facet <- question_data %>% filter(group_col == g_level)
        
        # If we are only showing one category, the 'group' aesthetic is no longer needed.
        hc <- hchart(plot_data_facet, "line", hcaes(x = as.character(wave), y = perc))
        
        hc %>%
          hc_title(text = as.character(g_level)) %>%
          hc_xAxis(title = list(text = "Wave")) %>%
          hc_yAxis(title = list(text = y_axis_label), max = max_perc) %>%
          # Legend is redundant when only one line is shown per facet.
          hc_legend(enabled = FALSE) %>%
          hc_size(height = 650) %>% 
          hc_exporting(enabled = TRUE)
      })
      
      return(
        tagList(
          tags$h3(varkey, style = "font-weight: bold;"),
          tags$div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(400px, 1fr)); gap: 20px;",
            facet_plots
          )
        )
      )
      
      # Logic for Non-Faceted plots
    } else {
      plot_data <- media_wave %>% filter(key == varkey)
      
      # Determine the grouping aesthetic based on user choices
      if (!is.null(display_range)) {
        # If showing one category, group by the demographic variable (if it exists)
        if (!is.null(group_by_var)) {
          hc <- hchart(plot_data, "line", hcaes(x = as.character(wave), y = perc, group = group_col))
        } else {
          # No demographic group, so no grouping aesthetic needed
          hc <- hchart(plot_data, "line", hcaes(x = as.character(wave), y = perc))
        }
      } else {
        # Original logic: showing all categories
        if (!is.null(group_by_var)) {
          plot_data <- plot_data %>% mutate(interaction_group = interaction(value, group_col, sep = ": "))
          hc <- hchart(plot_data, "line", hcaes(x = as.character(wave), y = perc, group = interaction_group))
        } else {
          hc <- hchart(plot_data, "line", hcaes(x = as.character(wave), y = perc, group = value))
        }
      }
      
      return(
        hc %>%
          hc_title(text = varkey) %>%
          hc_xAxis(title = list(text = "Wave")) %>%
          hc_yAxis(title = list(text = y_axis_label), max = max_perc) %>%
          hc_tooltip(
            shared = TRUE, valueDecimals = 1, valueSuffix = "%",
            headerFormat = '<b>Wave {point.key}</b><br/>',
            pointFormat = '<span style="color:{series.color}">●</span> {series.name}: <b>{point.y}</b><br/>'
          ) %>%
          hc_colors(thecolors) %>% 
          hc_plotOptions(
            line = list(
              dataLabels = list(enabled = TRUE, format = '{y:.0f}%', style = list(fontSize = '10px')),
              marker = list(enabled = TRUE)
            )
          ) %>%
          # Smart legend: only enable if there's something to differentiate
          hc_legend(enabled = is.null(display_range) || !is.null(group_by_var)) %>%
          hc_caption(text = paste0("<em>N = ", nrow(fin), ".</em>"), align = "right", style = list(fontSize = '10px', color = 'lightgrey')) %>%
          hc_size(height = 650) %>% 
          hc_exporting(enabled = TRUE)
      )
    }
  })
  
  return(hcs)
}

colors <- c("#2ca02c", "#1f77b4", "#ff7f0e") # Add more colors if needed

# Create a stacked bar chart with highcharter
myMenuItems <- c("downloadPNG", "downloadJPEG", "downloadSVG", "downloadPDF")




viz_wave_change <- function(vars, var_labels, categories_dat, wavevar,
                            range1 = 1:2, range2 = 3, range3 = 4:5,
                            weight_var = NULL) {
  
  # Prepare data
  fin <- data %>%
    select(all_of(vars), wave = !!sym(wavevar)) %>%
    drop_na() %>%
    set_names(c(var_labels, "wave"))
  
  media_wave <- fin %>%
    pivot_longer(cols = all_of(var_labels), names_to = "key", values_to = "value") %>%
    mutate(
      value = case_when(
        value %in% range1 ~ categories_dat[1],
        value %in% range2 ~ categories_dat[2],
        value %in% range3 ~ categories_dat[3],
        TRUE ~ NA_character_
      ),
      value = fct_relevel(value, categories_dat)
    ) %>%
    drop_na(value) %>%
    count(wave, key, value) %>%
    group_by(wave, key) %>%
    mutate(perc = n / sum(n) * 100) %>%
    ungroup()
  
  media_wave <- media_wave %>%
    mutate(
      wave_label = case_when(
        wave == 1 ~ "March 2024",
        wave == 2 ~ "December 2024",
        wave == 3 ~ "May 2025",
        TRUE      ~ as.character(wave)  # fallback if other waves appear
      ),
      wave_label = factor(wave_label, levels = c("March 2024", "December 2024", "May 2025"))
    )
  
  # Order variables by category3 in latest wave
  latest_wave <- max(media_wave$wave)
  # theorder <<- media_wave %>%
  #   filter(wave == latest_wave, value == categories_dat[3]) %>%
  #   ## TODO: what about the order?
  #   # arrange(desc(perc)) %>%
  #   pull(key) %>%
  #   unique() %>% 
  #   rev()
  
  theorder <- var_labels
  
  # Plot one line chart per variable (key)
  hcs <- map(theorder, function(varkey) {
    media_wave %>%
      filter(key == varkey) %>%
      hchart("line", hcaes(x = wave_label, y = perc, group = value)) %>%
      hc_title(text = varkey) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text = "Wave")) %>%
      hc_yAxis(title = list(text = "Percentage"), max = max(media_wave$perc)+5) %>%
      hc_tooltip(
        shared = TRUE,
        valueDecimals = 1,
        valueSuffix = "%"
      ) %>%
      hc_plotOptions(
        line = list(
          dataLabels = list(
            enabled = TRUE,
            format = '{y:.0f}%',
            style = list(fontSize = '10px')
          ),
          marker = list(enabled = TRUE)
        )
      ) %>%
      hc_legend(enabled = TRUE) %>%
      hc_caption(
        text = paste0("<em>N = ", nrow(fin), ".</em>"),
        align = "right",
        style = list(fontSize = '10px', color = 'lightgrey')
      ) %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = myMenuItems))) %>%
      hc_size(height = 650) 
  })
  
  return(hcs)
}




generate_quarto_dropdown <- function(list_of_plots) {
  ids <- paste0("chart_", seq_along(list_of_plots))
  
  # Safe title extraction
  get_title <- function(plot) {
    if (!is.null(plot$x$hc_opts$title) && !is.null(plot$x$hc_opts$title$text)) {
      as.character(plot$x$hc_opts$title$text)
    } else {
      "Untitled"
    }
  }
  
  titles <- vapply(list_of_plots, get_title, FUN.VALUE = character(1))
  
  # Dropdown
  cat('<label for="chartSelector"><strong>Select Chart:</strong></label><br>')
  cat('<select id="chartSelector" onchange="showChart(this.value)" class="form-select" style="width: 50%;">\n')
  for (i in seq_along(ids)) {
    cat(sprintf('<option value="%s">%s</option>\n', ids[i], htmltools::htmlEscape(titles[i])))
  }
  cat('</select>\n<br><br>')
  
  # Chart containers
  for (i in seq_along(list_of_plots)) {
    display <- if (i == 1) "block" else "none"
    cat(sprintf('<div id="%s" style="display: %s;">\n', ids[i], display))
    # Using print() on the widget is often more robust inside functions
    print(list_of_plots[[i]])
    cat('</div>\n')
  }
  
  # JavaScript for switching charts
  cat("
<script>
function showChart(id) {
  const charts = document.querySelectorAll('div[id^=\"chart_\"]');
  charts.forEach(c => c.style.display = 'none');
  document.getElementById(id).style.display = 'block';

  // Optional: Trigger a reflow of the chart when it becomes visible
  // This can fix resizing issues if the chart is complex.
  const chart = Highcharts.charts.find(c => c && c.renderTo.id === id);
  if (chart) {
    chart.reflow();
  }
}
</script>
")
}
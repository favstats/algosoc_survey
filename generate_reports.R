# ==============================================================================
# SCRIPT TO DYNAMICALLY GENERATE A QUARTO AI USAGE REPORT (v3 - Corrected)
#
# Instructions:
# 1. Make sure you have a file named 'utils.R' in the same directory.
#    This file must:
#      - Load your dataframes (e.g., dat, dat2, dat3).
#      - Define your visualization functions (viz_general, viz_wave_change, etc.).
#      - Define any helper functions (e.g., vecc, generate_quarto_elements).
# 2. Run this entire script in R.
# 3. A file named 'dynamic_ai_report.qmd' will be created.
# 4. Open and render 'dynamic_ai_report.qmd' using Quarto.
# ==============================================================================

# --- 1. PREPARATION: LOAD NECESSARY PACKAGES ---

# This ensures the required packages for the script itself are installed and loaded.
if (!require(pacman)) install.packages("pacman")
pacman::p_load(stringr, purrr)


# --- 2. DEFINITION: THE CORE REPORT GENERATION FUNCTION ---

#' Create a Quarto Markdown section with tabbed visualizations.
#'
#' This function generates the complete markdown and R code for a single
#' section of the report, including tabs for over-time analysis and
#' breakdowns by individual time waves.
#' Create a Quarto Markdown section with tabbed visualizations.
#'
#' This function generates the complete markdown and R code for a single
#' section of the report, including tabs for over-time analysis and
#' breakdowns by individual time waves.
create_report_section <- function(section_title, section_icon, question_text, vars_prefix,
                                  vars_indices, var_labels, category_labels, wave_data_list,
                                  group_vars, analyze_over_time = TRUE, time_tab_order = "over_time_first",
                                  y_axis_label_time, tip_text_time, reverse_cats_time = FALSE, rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7") {
  
  vec_to_str <- function(vec) {
    sanitized_vec <- stringr::str_replace_all(vec, '"', '\\\\"')
    if (is.character(vec)) {
      paste0('c("', paste(sanitized_vec, collapse = '", "'), '")')
    } else {
      paste0("c(", paste(vec, collapse = ", "), ")")
    }
  }
  
  vars_string <- sprintf('vecc("%s", %s)', vars_prefix, 
                         if(is.character(vars_indices)) vars_indices else paste(vars_indices, collapse = ":"))
  var_labels_string <- vec_to_str(var_labels)
  category_labels_string <- vec_to_str(category_labels)
  counter <- 0
  list_content <- {
    list_items <- mapply(
      function(label) {
        counter <<- counter + 1
        safe_label <- htmltools::htmlEscape(label)
        stringr::str_glue('<li>Item {counter}: {safe_label}</li>')
      },
      var_labels
    )
    stringr::str_glue('<ul class="details-content" style="margin-top: 1em; padding-left: 20px; display: none;">{paste(list_items, collapse = "\\n")}</ul>')
  }
  
  collapse_button <- stringr::str_glue(
    '<button class="collapse-button" type="button"><span class="arrow"></span></button>'
  )
  
  main_header <- paste(
    str_glue('\n\n# {{{{< iconify {section_icon} >}}}} {section_title} {{.tabset .tabset-fade .tabset-pills .break .btn-green}}'),
    str_glue('## {{{{< iconify {section_icon} >}}}} {section_title} {{.tabset .tabset-fade .tabset-pills .break .btn-green}}\n'),
    str_glue('<blockquote class="question-text">\n {question_text}\n {collapse_button}\n {list_content}</blockquote>\n'),
    '::: {.panel-tabset}',
    sep = '\n'
  )
  
  over_time_tab_content <- ""
  if (analyze_over_time && length(wave_data_list) > 1) {
    mutate_calls <- mapply(
      function(df_name, wave_num) {
        stringr::str_glue('{df_name} %>% mutate(wave = {wave_num})')
      },
      unlist(wave_data_list), 
      seq_along(wave_data_list)
    )
    data_merge_code <- stringr::str_glue('bind_rows({paste(mutate_calls, collapse = ", ")})')
    time_plot_cats <- if (reverse_cats_time) "rev(categories_dat)" else "categories_dat"
    
    over_time_header <- paste(
      '# Over Time',
      '```{r}',
      'var_labels <-', var_labels_string,
      'categories_dat <-', category_labels_string,
      'data <-', data_merge_code,
      '```',
      '::: {.panel-tabset}',
      sep = '\n'
    )
    
    overall_chunk <- paste(
      '## Overall',
      '::: {.panel-tabset}',
      '```{r, results="asis"}',
      str_glue('graphics <- viz_wave_change({vars_string}, var_labels, {time_plot_cats}, wavevar = "wave", range1 = {rangeone}, range2 = {rangetwo}, range3 = {rangethree})'),
      'generate_quarto_elements(graphics)',
      '```',
      ':::',
      sep = '\n'
    )
    
    group_chunks <- purrr::map_chr(names(group_vars), function(group_name) {
      group_var_name <- group_vars[[group_name]]
      data_filter_code <- if (group_var_name == "geslacht") ' %>% filter(geslacht != "Other")' else ''
      paste(
        str_glue('\n## {group_name}'),
        '::: {.panel-tabset}',
        '```{r, results="asis"}',
        str_glue('list_of_plots <- viz_wave_change3(data = data{data_filter_code}, vars = {vars_string}, range3 = {drange}, display_range = {drange}, var_labels = var_labels, categories_dat = categories_dat, wave_var = "wave", group_by_var = "{group_var_name}", y_axis_label = "{y_axis_label_time}", facet_by_group = F)'),
        str_glue('tiptext <- "{tip_text_time}"'),
        'generate_quarto_elements(list_of_plots)',
        '```',
        ':::',
        str_glue('\n<center>\n\n> Tip: `r HTML(tiptext)`\n\n</center>'),
        sep = '\n'
      )
    })
    over_time_tab_content <- paste(over_time_header, overall_chunk, paste(group_chunks, collapse="\n"), ':::', sep = '\n')
  }
  
  individual_wave_tabs <- purrr::map_chr(names(wave_data_list), function(wave_name) {
    data_name <- wave_data_list[[wave_name]]
    wave_header <- paste(
      str_glue('\n# {wave_name} {{.tabset .tabset-fade .tabset-pills .break}}'),
      '::: {.panel-tabset}',
      '## Overall',
      '```{r}',
      str_glue('viz_general({vars_string}, {var_labels_string}, {category_labels_string}, data = {data_name})'),
      '```',
      sep = '\n'
    )
    group_chunks <- purrr::map_chr(names(group_vars), function(group_name) {
      group_var_name <- group_vars[[group_name]]
      paste(
        str_glue('\n## {group_name}'),
        '::: {.panel-tabset}',
        '```{r, results="asis"}',
        str_glue('viz_general({vars_string}, {var_labels_string}, {category_labels_string}, "{group_var_name}", T, data = {data_name})'),
        '```',
        ':::',
        sep = '\n'
      )
    })
    paste(wave_header, paste(group_chunks, collapse = "\n"), ':::', sep = '\n')
  })
  
  tab_order <- if (time_tab_order == "over_time_first") {
    c(over_time_tab_content, individual_wave_tabs)
  } else {
    c(individual_wave_tabs, over_time_tab_content)
  }
  
  final_content <- paste(
    main_header,
    paste(tab_order, collapse = "\n\n"),
    ':::',
    sep = '\n'
  )
  return(final_content)
}


# --- 3. CONFIGURATION: REPORT HEADER AND SECTIONS ---

# FIX: Your full YAML, including the custom TOC javascript, is restored.
# Added new CSS and JavaScript for a custom collapsible button with a fixed arrow.
yaml_header <- '---
title: ""
format:
  html:
    theme: lumen
    page-layout: full
    self-contained: true
    code-fold: true
    code-overflow: wrap
    html-math-method: mathjax
    include-after-body:
      text: |
        <script>
          document.addEventListener("DOMContentLoaded", function () {
              var toc = document.createElement("div");
              toc.id = "custom-toc";
              toc.style.position = "fixed";
              toc.style.top = "0";
              toc.style.right = "-300px";  // Start hidden
              toc.style.width = "260px";
              toc.style.height = "100%";
              toc.style.background = "white";
              toc.style.padding = "10px";
              toc.style.boxShadow = "0px 2px 5px rgba(0, 0, 0, 0.2)";
              toc.style.transition = "right 0.3s ease-in-out";
              toc.style.overflowY = "auto";
            
              var tocTitle = document.createElement("h3");
              tocTitle.innerText = "Navigation";
              toc.appendChild(tocTitle);
            
              var headers = document.querySelectorAll("h2, h3, h4");
              headers.forEach(function (header, index) {
                  if (!header.id) {
                      header.id = "section-" + index;
                  }
                
                  var link = document.createElement("a");
                  link.innerText = header.innerText;
                  link.href = "#" + header.id;
                  link.style.display = "block";
                  link.style.padding = "5px 0";
                  link.style.color = "#007bff";
                  link.style.textDecoration = "none";
                
                  toc.appendChild(link);
              });
            
              document.body.appendChild(toc);
            
              // Create toggle button (📑 icon)
              var button = document.createElement("button");
              button.id = "toggle-toc";
              button.innerHTML = "📑";
              button.style.position = "fixed";
              button.style.top = "15px";
              button.style.right = "15px";
              button.style.backgroundColor = "white";
              button.style.color = "#333";
              button.style.border = "2px solid #ccc"; // Slightly thicker border
              button.style.padding = "10px 12px"; // Increased padding
              button.style.cursor = "pointer";
              button.style.borderRadius = "8px"; // More rounded
              button.style.fontSize = "22px"; // Larger icon
              button.style.zIndex = "1000";
              button.style.boxShadow = "0px 3px 6px rgba(0, 0, 0, 0.2)";
              button.style.width = "50px"; // Increase button size
              button.style.height = "50px"; // Make it square
            
              document.body.appendChild(button);
            
              // Click event to toggle the ToC
              button.addEventListener("click", function () {
                  if (toc.style.right === "0px") {
                      toc.style.right = "-300px"; // Hide ToC
                  } else {
                      toc.style.right = "0px"; // Show ToC
                  }
              });

              // Toggle collapsible content
              document.querySelectorAll(".collapse-button").forEach(button => {
                  button.addEventListener("click", function () {
                      const content = this.nextElementSibling;
                      const arrow = this.querySelector(".arrow");
                      const isExpanded = this.getAttribute("aria-expanded") === "true";
                      content.style.display = isExpanded ? "none" : "block";
                      this.setAttribute("aria-expanded", !isExpanded);
                  });
                  button.setAttribute("aria-expanded", "false");
              });
          });
          document.addEventListener("DOMContentLoaded", function () {
              document.querySelectorAll("a[href=\'https://favstats.github.io/de25/\']").forEach(function(link) {
                  link.addEventListener("click", function(event) {
                      event.preventDefault(); // Prevent default behavior
                      window.open(link.href, "_blank"); // Open in a new tab/window
                  });
              });
          });
        </script>
    includes:
      in-header: |
        <script src="https://cdn.jsdelivr.net/npm/apexcharts"></script>
        <style>
          .question-text {
            position: relative;
            padding-right: 25px;
            margin-right: 0;
          }
          .collapse-button {
            position: absolute;
            top: 50%;
            right: 0;
            transform: translateY(-50%);
            width: 20px;
            height: 20px;
            background: none;
            border: none;
            cursor: pointer;
            padding: 0;
            z-index: 10;
            transition: transform 0.3s ease;
          }
          .collapse-button .arrow {
            display: inline-block;
            width: 0;
            height: 0;
            border-left: 6px solid transparent;
            border-right: 6px solid transparent;
            border-bottom: 10px solid #4a90e2; /* Gradient base color */
            transition: transform 0.3s ease, border-bottom-color 0.3s ease;
            position: relative;
          }
          .collapse-button:hover .arrow {
            border-bottom-color: #9013fe; /* Gradient end color on hover */
            box-shadow: 0 0 5px rgba(74, 144, 226, 0.5); /* Glow effect */
            transform: scale(1.1);
          }
          .collapse-button[aria-expanded="true"] .arrow {
            transform: rotate(180deg) scale(1.1);
          }
          .details-content {
            margin-top: 0.5em;
            padding-left: 20px;
            transition: opacity 0.3s ease;
          }
          .details-content[style*="display: block"] {
            opacity: 1;
          }
          .details-content[style*="display: none"] {
            opacity: 0;
          }
        </style>
---
'

# FIX #1: Moved introductory text outside of the setup chunk.
setup_chunk <- '
```{r setup, include=FALSE}
# Global options and library loading
knitr::opts_chunk$set(cache = F, echo = F, warning = F, message = F, cache.lazy = FALSE)
pacman::p_load(htmltools, tidyverse, highcharter, gt, gtExtras, rvest, haven)
options(scipen = 999)

# !!! IMPORTANT !!!
# This script assumes your data (dat, dat2, dat3) and custom functions 
# (viz_general, viz_wave_change3, vecc, etc.) are loaded via this utility script.
source("utils.R")
```

The page is divided into four main sections. Each section allows you to examine overall trends as well as breakdowns by age, gender, education level, and political leaning.

::: {.panel-tabset .top .button-tabs}
'

sections_config <- list(
  section1 = list(
    section_title = "Ability to Recognize and Influence AI",
    section_icon = "fluent text-grammar-settings-20-regular",
    question_text = "With artificial intelligence (AI), computer programs can perform tasks themselves and also learn themselves. AI is increasingly used in society, and also in online media. Many websites use AI to show you more content and ads you like. The following questions are about the offers (such as articles, films or songs) that are shown to you on websites and apps for news and entertainment (e.g. Netflix or Spotify).<br><br><b>To what extent do you agree or disagree with the following statements?</b>",
    vars_prefix = "DIGIQ",
    vars_indices = "1:5",
    var_labels = c(
      "I recognize it when a website or app uses AI to adapt the content to me.",
      "I recognize it when specific content is recommended to me by AI.",
      "I know where to find the settings to change or disable AI personalization.",
      "I know how to access the data that AI systems use to tailor content to me.",
      "I know how I can influence what content is recommended to me by AI."
    ),
    category_labels = c("Agree (5-7)", "Neither agree or disagree (4)", "Disagree (1-3)"),
    y_axis_label_time = "% of Respondents Agreeing (5–7)",
    tip_text_time = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.<br><strong>Percentage shows those that Agree (5-7) with the statement.</strong>",
    reverse_cats_time = T # No reversal needed for this section
  ),
  section2 = list(
    section_title = "Usage of AI Systems",
    section_icon = "material-symbols-light interactive-space-outline-rounded",
    question_text = "<b>How often in the past year did you use...</b>",
    vars_prefix = "ADM",
    vars_indices = "1:2",
    var_labels = c("...text generators like ChatGPT?", "...image generators like Midjourney?"),
    category_labels = c("Often (5-7)", "Sometimes (4)", "Rare (1-3)"),
    y_axis_label_time = "% of Respondents who chose Often (5-7)",
    tip_text_time = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.<br><strong>Percentage shows those that chose Often (5-7).</strong>",
    reverse_cats_time = T # Reversal needed for this section's time plot
  ),
  section3 = list(
    section_title = "Exposure to AI",
    section_icon = "lets-icons view-light",
    question_text = "Artificial Intelligence (AI) can be used to create artificial content such as text, images and videos.<br><br><b>How often do you think you have come across the following things on social media in the past year?</b>",
    vars_prefix = "EXPO",
    vars_indices = "1:3",
    var_labels = c("Texts made by AI", "Photos or images made by AI", "Videos made by AI"),
    category_labels = c("Often (5-7)", "Sometimes (4)", "Rare (1-3)"),
    y_axis_label_time = "% of Respondents who chose Often (5-7)",
    tip_text_time = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.<br><strong>Percentage shows those that chose Often (5-7).</strong>",
    reverse_cats_time = T # Reversal needed for this section's time plot
  ),
  section4 = list(
    section_title = "Familiarity with AI terms",
    section_icon = "carbon ibm-watson-knowledge-studio",
    question_text = "<b>How familiar are you with the following terms?</b>",
    vars_prefix = "KAI",
    vars_indices = "1:5",
    var_labels = c("Generative AI", "ChatGPT", "Deepfakes", "Algorithms", "Chatbots"),
    category_labels = c("Familiar (5-7)", "Somewhat familiar (4)", "Unfamiliar (1-3)"),
    y_axis_label_time = "% of Respondents who chose Familiar (5-7)",
    tip_text_time = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.<br><strong>Percentage shows those that chose Familiar (5-7).</strong>",
    reverse_cats_time = T # Reversal needed for this section's time plot
  )
)

# --- 4. EXECUTION: GENERATE THE REPORT FILE ---
  global_params <- list(
    wave_data_list = list("March 2024" = "dat", "December 2024" = "dat2", "May 2025" = "dat3"),
    group_vars = c("By Age" = "age_groups", "By Gender" = "geslacht", "By Education" = "oplcat", "By Politics" = "pol_cat"),
    analyze_over_time = TRUE,
    time_tab_order = "over_time_first"
  )

output_file <- "_site/en/engagement.qmd"
file_conn <- file(output_file, "w", encoding = "UTF-8")

writeLines(c(yaml_header, setup_chunk), file_conn)

for (section_params in sections_config) {
  full_params <- c(section_params, global_params)
  section_md <- do.call(create_report_section, full_params)
  writeLines(section_md, file_conn)
}

closing_elements <- '
:::

<br>
<br>
<center>
<img src="algosoclogo.png" width="13%">
</center>
'
writeLines(closing_elements, file_conn)

close(file_conn)

# --- 5. COMPLETION ---
  cat(paste("\n✅ Success! The report has been generated at:", normalizePath(output_file), "\n"))

quarto::quarto_render(output_file)


dat3 %>% 
  select(DIGIQ1) %>% 
  mutate(thecat = case_when(
    DIGIQ1 %in% 1:3 ~ "Low",
    DIGIQ1 %in% 4 ~ "Mid",
    DIGIQ1 %in% 5:7 ~ "Hi"
  )) %>% 
  count(thecat, sort = T) %>% 
  drop_na() %>% 
  mutate(perc = n/sum(n)*100)

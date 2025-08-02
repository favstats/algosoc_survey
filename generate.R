# ==============================================================================
# SCRIPT TO DYNAMICALLY GENERATE THE 'ATTITUDES' QUARTO REPORT
#
# Instructions:
# 1. Ensure you have a 'utils.R' script in the same directory that:
#    - Loads dataframes: `dat`, `dat2`, `dat3`.
#    - Defines visualization functions: `viz_general`, `viz_wave_change`,
#      `viz_wave_change3`.
#    - Defines helper functions like `vecc` and `generate_quarto_elements`.
# 2. Run this entire R script.
# 3. A file named 'attitudes_report.qmd' will be created in your project's
#    root directory.
# 4. Open and render 'attitudes_report.qmd' using Quarto.
# ==============================================================================

# --- 1. PREPARATION: LOAD NECESSARY PACKAGES ---
if (!require(pacman)) install.packages("pacman")
pacman::p_load(stringr, purrr, htmltools)


# --- 2. DEFINITION: THE CORE REPORT GENERATION FUNCTION ---

create_report_section <- function(section_title, section_icon, question_text, vars_prefix,
                                  vars_indices, var_labels, category_labels, wave_data_list,
                                  group_vars, analyze_over_time = TRUE, time_tab_order = "not_first",
                                  y_axis_label_time, tip_text_time, reverse_cats_time = FALSE,
                                  rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7",
                                  vars_list = NULL) {
  
  vec_to_str <- function(vec) {
    sanitized_vec <- stringr::str_replace_all(vec, '"', '\\\\"')
    if (is.character(vec)) {
      paste0('c("', paste(sanitized_vec, collapse = '", "'), '")')
    } else {
      paste0("c(", paste(vec, collapse = ", "), ")")
    }
  }
  
  if (!is.null(vars_list)) {
    vars_string <- vec_to_str(vars_list)
  } else {
    vars_string <- sprintf('vecc("%s", %s)', vars_prefix,
                           if(is.character(vars_indices)) vars_indices else paste(vars_indices, collapse = ":"))
  }
  
  var_labels_string <- vec_to_str(var_labels)
  category_labels_string <- vec_to_str(category_labels)
  
  # Modified list content generation to handle single item
  list_content <- if (length(var_labels) == 1) {
    safe_label <- htmltools::htmlEscape(var_labels[1])
    stringr::str_glue('<div class="details-content" style="margin-top: 1em; padding-left: 20px; display: none;">{safe_label}</div>')
  } else {
    counter <- 0
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
      '```',
      '::: {.panel-tabset}',
      sep = '\n'
    )
    
    overall_chunk <- paste(
      '## Overall',
      if (length(var_labels) == 1) '' else '::: {.panel-tabset}',
      '```{r, results="asis"}',
      str_glue('graphics <- viz_wave_change({vars_string}, var_labels, {time_plot_cats}, wavevar = "wave", range1 = {rangeone}, range2 = {rangetwo}, range3 = {rangethree})'),
      'generate_quarto_elements(graphics)',
      '```',
      if (length(var_labels) == 1) '' else ':::',
      sep = '\n'
    )
    
    group_chunks <- purrr::map_chr(names(group_vars), function(group_name) {
      group_var_name <- group_vars[[group_name]]
      data_filter_code <- if (group_var_name == "geslacht") ' %>% filter(geslacht != "Other")' else ''
      paste(
        str_glue('\n## {group_name}'),
        if (length(var_labels) == 1) '' else '::: {.panel-tabset}',
        '```{r, results="asis"}',
        str_glue('list_of_plots <- viz_wave_change3(data = data{data_filter_code}, vars = {vars_string}, range3 = {drange}, display_range = {drange}, var_labels = var_labels, categories_dat = categories_dat, wave_var = "wave", group_by_var = "{group_var_name}", y_axis_label = "{y_axis_label_time}", facet_by_group = F)'),
        str_glue('tiptext <- "{tip_text_time}"'),
        'generate_quarto_elements(list_of_plots)',
        '```',
        if (length(var_labels) == 1) '' else ':::',
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
    
    # --- MODIFIED BLOCK START ---
    # This is the section where the conditional logic was missing.
    group_chunks <- purrr::map_chr(names(group_vars), function(group_name) {
      group_var_name <- group_vars[[group_name]]
      paste(
        str_glue('\n## {group_name}'),
        # This conditional wrapper was added to fix the nesting issue.
        if (length(var_labels) == 1) '' else '::: {.panel-tabset}',
        '```{r, results="asis"}',
        str_glue('viz_general({vars_string}, {var_labels_string}, {category_labels_string}, "{group_var_name}", T, data = {data_name})'),
        '```',
        if (length(var_labels) == 1) '' else ':::',
        sep = '\n'
      )
    })
    # --- MODIFIED BLOCK END ---
    
    paste(wave_header, paste(group_chunks, collapse = "\n"), ':::', sep = '\n')
  })
  
  tab_order <- if (time_tab_order == "time_first") {
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

# --- MODIFIED UTILS FUNCTION ---





# --- 3. CONFIGURATION: REPORT HEADER AND SECTIONS ---

# This YAML header includes your custom JavaScript for the slide-out TOC and collapsible question text.
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
        </style>
---
'

# This chunk loads libraries and sources your utility script for the Quarto file.
setup_chunk <- '
```{r setup, include=FALSE}
# Global options and library loading
knitr::opts_chunk$set(cache = F, echo = F, warning = F, message = F, cache.lazy = FALSE)
pacman::p_load(htmltools, tidyverse, highcharter, gt, gtExtras, rvest, haven)
options(scipen = 999)

# This script assumes your data (dat, dat2, dat3) and custom functions
# are loaded via this utility script.
source("utils.R")
```
The page is divided into several main sections. Each section allows you to examine overall trends as well as breakdowns by time, age, gender, education level, and political leaning.

::: {.panel-tabset .top .button-tabs}
'

######### Atittudes ###########

# All report content is defined here. Each list item is a major section of the report.
sections_config <- list(
  section1 = list(
    section_title = "Detecting AI-Generated Content",
    section_icon = "fluent-emoji-high-contrast detective",
    question_text = "How confident are you that the following groups can tell the difference between content created by AI and content created by people?",
    vars_prefix = "CONF",
    vars_indices = "1:8",
    var_labels = c(
      "People from the general population", "Members of Parliament", "Judges", "Journalists", "Police officers", "Doctors", "Friends/family", "Yourself"
    ),
    category_labels = c("Confident (5-7)", "Somewhat confident (4)", "Not confident (1-3)"),
    wave_data_list = list("May 2025" = "dat3", "December 2024" = "dat2", "March 2024" = "dat"),
    analyze_over_time = TRUE,
    time_tab_order = "not_first",
    y_axis_label_time = "% of Respondents who chose Confident (5-7)",
    tip_text_time = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.<br><strong>Percentage shows those that chose Confident (5-7).</strong>",
    reverse_cats_time = TRUE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section2 = list(
    section_title = "Trust in ADM Systems",
    section_icon = "mingcute safe-lock-fill",
    question_text = "We ask you a number of additional questions about automatic decision-making (ADM) systems. These are computer programs that make decisions that used to be made by people. These decisions are made automatically by computers based on data.<br><br><b>How much do you trust automated decision-making systems to properly determine...</b>",
    vars_prefix = "TADS",
    vars_indices = "1:3",
    var_labels = c("Social Welfare", "The News You See", "Cancer Diagnosis"),
    category_labels = c("Trust (5-7)", "Somewhat trust (4)", "Little trust (1-3)"),
    wave_data_list = list("Results (Wave 1)" = "dat"),
    analyze_over_time = FALSE, # This creates a section with only one time point
    time_tab_order = "not_first",
    y_axis_label_time = "", tip_text_time = "", reverse_cats_time = FALSE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section3 = list(
    section_title = "Future of ADM Systems",
    section_icon = "game-icons sands-of-time",
    question_text = "If automated decision-making (ADM) becomes more common in the future, what do you think will happen?",
    vars_prefix = "ADSO",
    vars_indices = "1:5",
    var_labels = c(
      "If the news is automated, journalists will be able to focus more on investigative journalism.",
      "I will only get news items that interest me.",
      "It will make reporting fairer and more balanced.",
      "If certain law enforcement tasks are automated, the police will have more time to focus on major crimes.",
      "It will make the determination of social benefits fairer and more balanced."
    ),
    category_labels = c("Agree (5-7)", "Neither agree or disagree (4)", "Disagree (1-3)"),
    wave_data_list = list("Results (Wave 1)" = "dat"), # Assuming single wave, change if needed
    analyze_over_time = FALSE,
    time_tab_order = "not_first",
    y_axis_label_time = "", tip_text_time = "", reverse_cats_time = FALSE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  # --- NEW SECTION ADDED HERE ---
  section4 = list(
    section_title = "Responsibility for Harmful AI Content",
    section_icon = "mdi scale-balance",
    question_text = "The responsibility for harmful content (e.g., fake news or manipulated videos) created by Artificial Intelligence (AI) lies primarily with...",
    vars_prefix = "HARAI",
    vars_indices = "1:4",
    var_labels = c(
      "...the company behind the AI technology.",
      "...the person who used the AI to create the content.",
      "...the social media platforms that spread the content.",
      "...the government for not creating enough rules."
    ),
    category_labels = c("Agree (5-7)", "Neither agree or disagree (4)", "Disagree (1-3)"),
    wave_data_list = list("May 2025" = "dat3", "December 2024" = "dat2"),
    
    analyze_over_time = TRUE,
    time_tab_order = "not_first",
    y_axis_label_time = "", 
    tip_text_time = "", 
    reverse_cats_time = FALSE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  # --- NEW SECTION ADDED HERE ---
  section5 = list(
    section_title = "Concerns About AI",
    section_icon = "mdi alert-circle-outline",
    question_text = "There is a lot of talk about the potential disadvantages of Artificial Intelligence (AI). Different people have different ideas about what constitutes a problem.<br><br><b>How worried are you about the following issues?</b>",
    vars_prefix = "PRAI",
    vars_indices = "1:5",
    var_labels = c(
      "Using AI to create content that offends some people",
      "Using AI to mislead or manipulate people into buying something",
      "Using AI to mislead or manipulate people for a political purpose",
      "When it is not clear that content was created by AI",
      "Using AI to take over people's jobs"
    ),
    category_labels = c("Very worried (5-7)", "Somewhat worried (4)", "Not worried (1-3)"),
    wave_data_list = list("May 2025" = "dat3", "December 2024" = "dat2"), # Available in waves 2 and 3
    analyze_over_time = TRUE,
    time_tab_order = "not_first",
    y_axis_label_time = "% of Respondents who are Very worried (5-7)",
    tip_text_time = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.<br><strong>Percentage shows those that are Very worried (5-7).</strong>",
    reverse_cats_time = TRUE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section6 = list(
    section_title = "Attention to AI in the News",
    section_icon = "mdi newspaper-variant-outline",
    question_text = "What do you think of the amount of attention paid to Artificial Intelligence (AI) in the news?",
    vars_list = "MC", 
    var_labels = c("Amount of Attention to AI in News"),
    category_labels = c("Too much (5-7)", "About right (4)", "Too little (1-3)"),
    wave_data_list = list("May 2025" = "dat3", "December 2024" = "dat2", "March 2024" = "dat"),
    analyze_over_time = T,
    time_tab_order = "not_first",
    y_axis_label_time = "% of Respondents who think it is Too much (5-7)",
    tip_text_time = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.<br><strong>Percentage shows those that chose Too much (5-7).</strong>",
    reverse_cats_time = T,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section7 = list(
    section_title = "Perception of AI in the Media",
    section_icon = "mdi movie-open-outline",
    question_text = "In your opinion, what is the image of Artificial Intelligence (AI) in the media?",
    vars_list = "AI_PERCEP",
    var_labels = c("Portrayal of AI in the Media"),
    category_labels = c("Positive (5-7)", "Neutral (4)", "Negative (1-3)"),
    wave_data_list = list("May 2025" = "dat3", "December 2024" = "dat2", "March 2024" = "dat"),
    analyze_over_time = T,
    time_tab_order = "not_first",
    y_axis_label_time = "% of Respondents who think Positively (5-7)",
    tip_text_time = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.<br><strong>Percentage shows those that chose Positive (5-7).</strong>",
    reverse_cats_time = T,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  )
)

# --- 4. EXECUTION: GENERATE THE REPORT FILE ---
#   Define parameters that are the same across all sections
global_params <- list(
  group_vars = c("By Age" = "age_groups", "By Gender" = "geslacht", "By Education" = "oplcat", "By Politics" = "pol_cat")
)

# Set the name for the output file
output_file <- "_site/en/attitudes.qmd"
file_conn <- file(output_file, "w", encoding = "UTF-8")

# Write the initial parts of the Quarto file
writeLines(c(yaml_header, setup_chunk), file_conn)

# Loop through the configuration to create each section
for (section_params in sections_config) {
  
  # Combine the specific section parameters with the global ones
  full_params <- c(section_params, global_params)
  
  # Call the function to generate the markdown and R code for the section
  section_md <- do.call(create_report_section, full_params)
  
  # Write the generated content to the file
  writeLines(section_md, file_conn)
}

# Add the final logo and closing tabset panel
closing_elements <- '
:::

<br>
<center>
<img src="algosoclogo.png" width="13%">
</center>
'
writeLines(closing_elements, file_conn)

# Close the file connection
close(file_conn)

# --- 5. COMPLETION ---
  cat(paste("\n✅ Success! The report has been generated at:", normalizePath(output_file), "\n"))

# Optional: Render the report immediately after creating it
quarto::quarto_render(output_file)


###### Engagement #####


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
  wave_data_list = list("May 2025" = "dat3", "December 2024" = "dat2", "March 2024" = "dat"),
  group_vars = c("By Age" = "age_groups", "By Gender" = "geslacht", "By Education" = "oplcat", "By Politics" = "pol_cat"),
  analyze_over_time = TRUE,
  time_tab_order = "not_first"
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


###### Social Media #####

# All report content is defined here. Each list item is a major section of the report.
sections_config <- list(
  section1 = list(
    section_title = "Content Moderation on Social Media",
    section_icon = "mdi gavel",
    question_text = "Social media companies often engage in 'content moderation', such as removing content that does not comply with the rules. News organizations are sometimes accused of spreading misinformation (disinformation), causing social media companies to remove their news stories.<br><br><b>To what extent do you agree with the following statements?</b>",
    vars_prefix = "CMN",
    vars_indices = "1:6",
    var_labels = c(
      "I trust that a social media platform can determine whether something is disinformation or not.",
      "Social media platforms should restrict or remove content they deem disinformation, even if it comes from news organizations.",
      "Social media platforms should only restrict or remove news content if they notify the news organizations in advance.",
      "The freedom of news organizations is threatened by the content moderation of social media platforms.",
      "News organizations should have the ability to challenge content moderation decisions made by social media platforms.",
      "Certain news organizations should be moderated more than others by social media companies. "
    ),
    category_labels = c("Agree (5-7)", "Neither agree or disagree (4)", "Disagree (1-3)"),
    wave_data_list = list("May 2025" = "dat3", "December 2024" = "dat2"),
    analyze_over_time = TRUE,
    time_tab_order = "not_first",
    y_axis_label_time = "% of Respondents who chose Agree (5-7)",
    tip_text_time = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.<br><strong>Percentage shows those that chose Agree (5-7).</strong>",
    reverse_cats_time = T, # Matches logic in original file
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section2 = list(
    section_title = "Perceived Self-Influence of Political Ads",
    section_icon = "mdi bullhorn",
    question_text = "We will now ask you questions about political advertisements on social media.<br><br><b>These questions are about your own opinion or feeling:</b>",
    # Using vars_list because variable names are not sequentially numbered
    vars_list = c("SM_ADS1a", "SM_ADS1b", "SM_ADS1c"),
    var_labels = c(
      "I always notice when I am being influenced by political ads on social media.",
      "I have liked a political party more after seeing their ads on social media.",
      "I am more likely to support a candidate recommended by influencers I follow."
    ),
    category_labels = c("Agree (5-7)", "Neither (4)", "Disagree (1-3)"),
    wave_data_list = list("May 2025" = "dat3", "December 2024" = "dat2"),
    analyze_over_time = T,
    time_tab_order = "not_first",
    y_axis_label_time = "", 
    tip_text_time = "", 
    reverse_cats_time = T,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  # --- NEW SECTION 10 ADDED HERE ---
  section3 = list(
    section_title = "Perceived Influence of Political Ads on Others",
    section_icon = "mdi bullhorn-outline",
    question_text = "These questions are about your opinion of other people:",
    # Using vars_list because variable names are not sequentially numbered
    vars_list = c("SM_ADS2a", "SM_ADS2b", "SM_ADS2c"),
    var_labels = c(
      "Other people always notice when they are being influenced by political ads on social media.",
      "I think other people will like a political party more after seeing their ads on social media.",
      "I believe others are more likely to support a candidate recommended by influencers they follow."
    ), 
    category_labels = c("Agree (5-7)", "Neither (4)", "Disagree (1-3)"),
    wave_data_list = list("May 2025" = "dat3", "December 2024" = "dat2"),
    analyze_over_time = T,
    time_tab_order = "not_first",
    y_axis_label_time = "", 
    tip_text_time = "", 
    reverse_cats_time = T,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  )
)

# --- 4. EXECUTION: GENERATE THE REPORT FILE ---
#   Define parameters that are the same across all sections
global_params <- list(
  group_vars = c("By Age" = "age_groups", "By Gender" = "geslacht", "By Education" = "oplcat", "By Politics" = "pol_cat")
)

# Set the name for the output file
output_file <- "_site/en/socialmedia.qmd"
file_conn <- file(output_file, "w", encoding = "UTF-8")

# Write the initial parts of the Quarto file
writeLines(c(yaml_header, setup_chunk), file_conn)

# Loop through the configuration to create each section
for (section_params in sections_config) {
  
  # Combine the specific section parameters with the global ones
  full_params <- c(section_params, global_params)
  
  # Call the function to generate the markdown and R code for the section
  section_md <- do.call(create_report_section, full_params)
  
  # Write the generated content to the file
  writeLines(section_md, file_conn)
}

# Add the final logo and closing tabset panel
closing_elements <- '
:::

<br>
<center>
<img src="algosoclogo.png" width="13%">
</center>
'
writeLines(closing_elements, file_conn)

# Close the file connection
close(file_conn)

# --- 5. COMPLETION ---
cat(paste("\n✅ Success! The report has been generated at:", normalizePath(output_file), "\n"))

# Optional: Render the report immediately after creating it
quarto::quarto_render(output_file)

# source("generate_nl.R")




## values ######


# New custom function specifically for the "Values" section
# Custom function specifically for the "Values" section structure
# --- UPDATED GENERATOR FUNCTION ---

create_values_section <- function(config, time_tab_order = "time_first") { # <-- Added argument here
  create_chunk <- function(code, label, ...) {
    opts <- list(...)
    opts_str <- paste(names(opts), unlist(opts), sep = "=", collapse = ", ")
    header <- paste0("```{r ", label)
    if (nchar(opts_str) > 0) {
      header <- paste0(header, ", ", opts_str)
    }
    paste0(header, "}\n", code, "\n```")
  }
  
  main_header <- paste(
    str_glue('\n# {{{{< iconify {config$icon} >}}}} {config$title} {{.tabset .tabset-fade .tabset-pills .break}}'),
    str_glue('\n## {{{{< iconify {config$icon} >}}}} {config$title}\n'),
    str_glue('<blockquote class="question-text">{config$question}</blockquote>'),
    '\n::: {.panel-tabset}',
    sep = '\n'
  )
  
  over_time_header <- '\n# Over Time\n\n::: {.panel-tabset}\n'
  overall_time_chunk <- paste(
    '## Overall', '::: {.panel-tabset}',
    create_chunk('plots <- viz_values_over_time(data = data, issue_var = "important1", wave_var_raw = "wave", labels_df = thelabs, final_labels_col = "adsv_var_labels"); generate_quarto_elements(plots)',
                 label = "values-time-overall", results = "'asis'"),
    ':::', sep = "\n"
  )
  group_time_chunks <- imap_chr(config$group_vars, ~paste(
    str_glue('\n## {.y}'), '::: {.panel-tabset}',
    create_chunk(str_glue('plots <- viz_values_over_time(data = data, issue_var = "important1", wave_var_raw = "wave", group_by_var = "{.x}", labels_df = thelabs, final_labels_col = "adsv_var_labels"); generate_quarto_elements(plots)'),
                 label = paste0("values-time-", .y), results = "'asis'"),
    ':::', str_glue('\n<center>\n> Tip: `r HTML("{config$tiptext}")`\n</center>'), sep = "\n"
  ))
  over_time_content <- paste(over_time_header, overall_time_chunk, paste(group_time_chunks, collapse="\n"), ':::', sep="\n")
  
  wave_tabs_content <- imap_chr(config$wave_data, ~{
    wave_name_label <- str_replace_all(tolower(.y), " ", "-")
    wave_header <- paste(
      str_glue('\n# {.y} {{.tabset .tabset-fade .tabset-pills .break}}'),
      '::: {.panel-tabset}', '## Overall',
      create_chunk(str_glue('viz_values_by_wave({.x}, thelabs = thelabs)'), label = paste0("values-", wave_name_label, "-overall")),
      sep = "\n"
    )
    group_wave_chunks <- imap_chr(config$group_vars, function(var_name, display_name) {
      paste(str_glue('\n## {display_name}'), 
            create_chunk(str_glue('viz_values_by_wave({.x}, group_by_var = "{var_name}", thelabs = thelabs)'), 
                         label = paste0("values-", wave_name_label, "-", display_name), results = "'asis'"), 
            sep = "\n")
    })
    paste(wave_header, paste(group_wave_chunks, collapse="\n"), ':::', sep = "\n")
  })
  
  # --- NEW LOGIC FOR TAB ORDER ---
  tabs_ordered <- if (time_tab_order == "time_first") {
    c(over_time_content, wave_tabs_content)
  } else {
    c(wave_tabs_content, over_time_content)
  }
  
  paste(main_header, paste(tabs_ordered, collapse = "\n\n"), ':::', sep = '\n')
}
# --- 3. CONFIGURATION: DEFINE ALL REPORT SECTIONS ---
# --- CONFIGURATION for the full 4-tab report ---

# Part 1: Config for the unique "Most Important Values" section
values_section_config <- list(
  title = "Most Important Values",
  icon = "healthicons world-care-outline",
  question = "What do you consider the 5 most important values for automated decision-making systems? [only top important value shown]",
  tiptext = "You can hover or click on the legend to highlight, remove or add groups, which can make the chart easier to read.",
  group_vars = c("By Age" = "age_groups", "By Gender" = "geslacht", "By Education" = "oplcat", "By Politics" = "pol_cat"),
  wave_data = list("May 2025" = "dat3", "December 2024" = "dat2", "March 2024" = "dat"),
  time_tab_order = "not_first"
)


# Part 2: Config for the three standard sections ("Usefulness", "Riskiness", "Fairness")
standard_sections_config <- list(
  usefulness = list(
    section_title = "Usefulness of AI",
    section_icon = "fluent data-usage-16-regular",
    question_text = "> In most predictions, automated decision-making will become more common in the future. If so, to what extent do you disagree or agree with the following statements:\n\n<blockquote class='question-text'>More automatic decision-making in news media/the legal system/healthcare will be **useful**.</blockquote>",
    vars_list = c("ADSR1", "ADSRt2_1", "ADSRt3_1"),
    var_labels = c("News Media", "Legal System", "Healthcare"),
    category_labels = c("Agree (5-7)", "Neither agree or disagree (4)", "Disagree (1-3)")
  ),
  riskiness = list(
    section_title = "Riskiness of AI",
    section_icon = "solar danger-outline",
    question_text = "In most predictions, automated decision-making will become more common in the future. If so, to what extent do you disagree or agree with the following statements:\n\n<blockquote class='question-text'>More automatic decision-making in news media/the legal system/healthcare will be **risky**.</blockquote>",
    vars_list = c("ADSR2", "ADSRt2_2", "ADSRt3_2"),
    var_labels = c("News Media", "Legal System", "Healthcare"),
    category_labels = c("Agree (5-7)", "Neither agree or disagree (4)", "Disagree (1-3)")
  ),
  fairness = list(
    section_title = "Fairness of AI",
    section_icon = "material-symbols-light balance",
    question_text = "In most predictions, automated decision-making will become more common in the future. If so, to what extent do you disagree or agree with the following statements:\n\n<blockquote class='question-text'>More automatic decision-making in news media/the legal system/healthcare will be **more fair**.</blockquote>",
    vars_list = c("ADSR3", "ADSRt2_3", "ADSRt3_3"),
    var_labels = c("News Media", "Legal System", "Healthcare"),
    category_labels = c("Agree (5-7)", "Neither agree or disagree (4)", "Disagree (1-3)")
  )
)

# Part 3: Global parameters that apply to all standard sections
global_params_for_standard_sections <- list(
  wave_data_list = list("March 2024" = "dat", "December 2024" = "dat2", "May 2025" = "dat3"),
  group_vars = values_section_config$group_vars,
  analyze_over_time = TRUE,
  time_tab_order = "time_first",
  y_axis_label_time = "% of Respondents Agreeing (5–7)",
  tip_text_time = values_section_config$tiptext,
  reverse_cats_time = TRUE,
  drange = "5:7"
)
# --- 4. EXECUTION: GENERATE AND WRITE THE REPORT FILE ---

# --- EXECUTION: Generate and Write the Single Report File ---

output_file <- "_site/en/values.qmd"
file_conn <- file(output_file, "w", encoding = "UTF-8")

# Write the initial parts of the Quarto file
writeLines(c(yaml_header, setup_chunk), file_conn)

# 1. Generate and write the custom "Most Important Values" section
values_md <- create_values_section(values_section_config, time_tab_order = "not_first")
writeLines(values_md, file_conn)

# 2. Generate and write the three standard sections by looping through their config
for (section_params in standard_sections_config) {
  # Combine the specific section config with the global parameters
  full_params <- c(section_params, global_params_for_standard_sections)
  
  # Generate the markdown for this section
  section_md <- do.call(create_report_section, full_params)
  
  # Write it to the file
  writeLines(section_md, file_conn)
}

# Write the final closing elements
writeLines(closing_elements, file_conn)
close(file_conn)

# --- COMPLETION ---
cat(paste("\n✅ Success! The report has been generated at:", normalizePath(output_file), "\n"))

# Optional: Render the report immediately
quarto::quarto_render(output_file)

beepr::beep(21)

source("generate_nl.R")

# dat3 %>% select(pol_cat, left_right) %>% View()
# 
# dat3 %>% 
#   drop_na(DIGIQ1) %>% 
#   select(DIGIQ1, age_groups) %>% 
#   mutate(DIGIQ1 = case_when(
#     DIGIQ1 %in% 1:3 ~ "Low",
#     DIGIQ1 %in% 4 ~ "Mid",
#     DIGIQ1 %in% 5:7 ~ "Hi"
#   )) %>% 
#   group_by(age_groups) %>% 
#   count(DIGIQ1) %>% 
#   mutate(perc = n/sum(n)*100)
# 
# fin %>% 
#   # filter(wave == 3)
#   drop_na(DIGIQ1) %>% 
#   select(DIGIQ1, age_groups, wave)%>% 
#   mutate(DIGIQ1 = case_when(
#     DIGIQ1 %in% 1:3 ~ "Low",
#     DIGIQ1 %in% 4 ~ "Mid",
#     DIGIQ1 %in% 5:7 ~ "Hi"
#   )) %>% 
#   group_by(age_groups, wave) %>% 
#   count(DIGIQ1) %>% 
#   mutate(perc = n/sum(n)*100) %>% 
#   ungroup() %>% 
#   arrange(age_groups, DIGIQ1, wave) %>% 
#   View()
#   
#   ggplot(aes(wave, perc, color = DIGIQ1)) +
#   geom_line() +
#   facet_wrap(~age_groups)

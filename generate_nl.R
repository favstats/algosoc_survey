# ==============================================================================
# SCRIPT OM DYNAMISCH HET 'ATTITUDES' QUARTO-RAPPORT TE GENEREREN
#
# Instructies:
# 1. Zorg ervoor dat je een 'utils.R'-script in dezelfde map hebt dat:
#    - Dataframes laadt: `dat`, `dat2`, `dat3`.
#    - Visualisatiefuncties definieert: `viz_general`, `viz_wave_change`,
#      `viz_wave_change3`.
#    - Hulpfuncties definieert zoals `vecc` en `generate_quarto_elements`.
# 2. Voer dit volledige R-script uit.
# 3. Een bestand met de naam 'attitudes_rapport.qmd' wordt aangemaakt in de
#    hoofdmap van je project.
# 4. Open en render 'attitudes_rapport.qmd' met Quarto.
# ==============================================================================

# --- 1. VOORBEREIDING: LAAD DE BENODIGDE PACKAGES ---
if (!require(pacman)) install.packages("pacman")
pacman::p_load(stringr, purrr, htmltools)


# --- 2. DEFINITIE: DE KERNFUNCTIE VOOR RAPPORTGENERATIE ---

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
  
  # Aangepaste lijstinhoudgeneratie om een enkel item af te handelen
  list_content <- if (length(var_labels) == 1) {
    safe_label <- htmltools::htmlEscape(var_labels[1])
    stringr::str_glue('<div class="details-content" style="margin-top: 1em; padding-left: 20px; display: none;">{safe_label}</div>')
  } else {
    counter <- 0
    list_items <- mapply(
      function(label) {
        counter <<- counter + 1
        safe_label <- htmltools::htmlEscape(label)
        stringr::str_glue('<li>Vraag {counter}: {safe_label}</li>')
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
      '# Over de tijd',
      '```{r}',
      'var_labels <-', var_labels_string,
      'categories_dat <-', category_labels_string,
      '```',
      '::: {.panel-tabset}',
      sep = '\n'
    )
    
    overall_chunk <- paste(
      '## Algemeen',
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
      data_filter_code <- if (group_var_name == "geslacht") ' %>% filter(geslacht != "Anders")' else ''
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
      '## Algemeen',
      '```{r}',
      str_glue('viz_general({vars_string}, {var_labels_string}, {category_labels_string}, data = {data_name})'),
      '```',
      sep = '\n'
    )
    
    # --- AANGEPAST BLOK START ---
    # Dit is de sectie waar de conditionele logica ontbrak.
    group_chunks <- purrr::map_chr(names(group_vars), function(group_name) {
      group_var_name <- group_vars[[group_name]]
      paste(
        str_glue('\n## {group_name}'),
        # Deze conditionele wrapper is toegevoegd om het nestprobleem op te lossen.
        if (length(var_labels) == 1) '' else '::: {.panel-tabset}',
        '```{r, results="asis"}',
        str_glue('viz_general({vars_string}, {var_labels_string}, {category_labels_string}, "{group_var_name}", T, data = {data_name})'),
        '```',
        if (length(var_labels) == 1) '' else ':::',
        sep = '\n'
      )
    })
    # --- AANGEPAST BLOK EINDE ---
    
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

# --- 3. CONFIGURATIE: RAPPORT HEADER EN SECTIES ---

# Deze YAML-header bevat uw aangepaste JavaScript voor de uitschuifbare TOC en inklapbare vraagtekst.
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
              toc.style.right = "-300px";  // Start verborgen
              toc.style.width = "260px";
              toc.style.height = "100%";
              toc.style.background = "white";
              toc.style.padding = "10px";
              toc.style.boxShadow = "0px 2px 5px rgba(0, 0, 0, 0.2)";
              toc.style.transition = "right 0.3s ease-in-out";
              toc.style.overflowY = "auto";
              var tocTitle = document.createElement("h3");
              tocTitle.innerText = "Navigatie";
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
              // Maak toggle-knop (📑 icoon)
              var button = document.createElement("button");
              button.id = "toggle-toc";
              button.innerHTML = "📑";
              button.style.position = "fixed";
              button.style.top = "15px";
              button.style.right = "15px";
              button.style.backgroundColor = "white";
              button.style.color = "#333";
              button.style.border = "2px solid #ccc"; // Iets dikkere rand
              button.style.padding = "10px 12px"; // Verhoogde opvulling
              button.style.cursor = "pointer";
              button.style.borderRadius = "8px"; // Meer afgerond
              button.style.fontSize = "22px"; // Groter icoon
              button.style.zIndex = "1000";
              button.style.boxShadow = "0px 3px 6px rgba(0, 0, 0, 0.2)";
              button.style.width = "50px"; // Verhoog knopgrootte
              button.style.height = "50px"; // Maak het vierkant
              document.body.appendChild(button);
              // Klik-gebeurtenis om de ToC te wisselen
              button.addEventListener("click", function () {
                  if (toc.style.right === "0px") {
                      toc.style.right = "-300px"; // Verberg ToC
                  } else {
                      toc.style.right = "0px"; // Toon ToC
                  }
              });
              // Wissel inklapbare inhoud
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
                      event.preventDefault(); // Voorkom standaardgedrag
                      window.open(link.href, "_blank"); // Open in een nieuw tabblad/venster
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
            border-bottom: 10px solid #4a90e2; /* Verloop basiskleur */
            transition: transform 0.3s ease, border-bottom-color 0.3s ease;
            position: relative;
          }
          .collapse-button:hover .arrow {
            border-bottom-color: #9013fe; /* Verloop eindkleur bij hover */
            box-shadow: 0 0 5px rgba(74, 144, 226, 0.5); /* Gloedeffect */
            transform: scale(1.1);
          }
          .collapse-button[aria-expanded="true"] .arrow {
            transform: rotate(180deg) scale(1.1);
          }
        </style>
---
'

# Dit chunk laadt bibliotheken en bronnen van uw utility-script voor het Quarto-bestand.
setup_chunk <- '
```{r setup, include=FALSE}
# Globale opties en laden van bibliotheken
knitr::opts_chunk$set(cache = F, echo = F, warning = F, message = F, cache.lazy = FALSE)
pacman::p_load(htmltools, tidyverse, highcharter, gt, gtExtras, rvest, haven)
options(scipen = 999)

# Dit script gaat ervan uit dat uw gegevens (dat, dat2, dat3) en aangepaste functies
# via dit utility-script worden geladen.
source("utils.R")
```
De pagina is verdeeld in verschillende hoofdsecties. Elke sectie stelt u in staat om algemene trends en uitsplitsingen naar tijd, leeftijd, geslacht, opleidingsniveau en politieke voorkeur te onderzoeken.

::: {.panel-tabset .top .button-tabs}

'
######### Attitudes ###########
# Alle rapportinhoud wordt hier gedefinieerd. Elk lijstitem is een hoofdsectie van het rapport.
sections_config <- list(
  section1 = list(
    section_title = "Herkennen van door AI gegenereerde inhoud",
    section_icon = "fluent-emoji-high-contrast detective",
    question_text = "Hoe zeker bent u ervan dat de volgende groepen het verschil kunnen zien tussen content gemaakt door AI en content gemaakt door mensen?",
    vars_prefix = "CONF",
    vars_indices = "1:8",
    var_labels = c(
      "Mensen uit de algemene bevolking", "Parlementsleden", "Rechters", "Journalisten", "Politieagenten", "Artsen", "Vrienden/familie", "Uzelf"
    ),
    category_labels = c("Zeker (5-7)", "Enigszins zeker (4)", "Niet zeker (1-3)"),
    wave_data_list = list("Mei 2025" = "dat3", "December 2024" = "dat2", "Maart 2024" = "dat"),
    analyze_over_time = TRUE,
    time_tab_order = "not_first",
    y_axis_label_time = "% van de respondenten die 'Zeker' (5-7) kozen",
    tip_text_time = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.<br><strong>Percentage toont degenen die 'Zeker' (5-7) kozen.</strong>",
    reverse_cats_time = TRUE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section2 = list(
    section_title = "Vertrouwen in ADM-systemen",
    section_icon = "mingcute safe-lock-fill",
    question_text = "We stellen u nog een aantal vragen over geautomatiseerde beslissingssystemen (ADS). Dit zijn computerprogramma's die beslissingen nemen die voorheen door mensen werden genomen. Deze beslissingen worden automatisch door computers genomen op basis van gegevens.<br><br><b>Hoeveel vertrouwen heeft u erin dat geautomatiseerde beslissingssystemen correct bepalen...</b>",
    vars_prefix = "TADS",
    vars_indices = "1:3",
    var_labels = c("Sociale voorzieningen", "Het nieuws dat u ziet", "Kankerdiagnose"),
    category_labels = c("Vertrouwen (5-7)", "Enigszins vertrouwen (4)", "Weinig vertrouwen (1-3)"),
    wave_data_list = list("Resultaten (Wave 1)" = "dat"),
    analyze_over_time = FALSE, # Dit creëert een sectie met slechts één tijdspunt
    time_tab_order = "not_first",
    y_axis_label_time = "", tip_text_time = "", reverse_cats_time = FALSE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section3 = list(
    section_title = "Toekomst van ADM-systemen",
    section_icon = "game-icons sands-of-time",
    question_text = "Als geautomatiseerde besluitvorming (ADS) in de toekomst vaker voorkomt, wat denkt u dan dat er zal gebeuren?",
    vars_prefix = "ADSO",
    vars_indices = "1:5",
    var_labels = c(
      "Als het nieuws geautomatiseerd wordt, kunnen journalisten zich meer richten op onderzoeksjournalistiek.",
      "Ik krijg alleen nieuwsberichten te zien die mij interesseren.",
      "Het zal de verslaggeving eerlijker en evenwichtiger maken.",
      "Als bepaalde rechtshandhavingstaken worden geautomatiseerd, heeft de politie meer tijd om zich te richten op grote misdrijven.",
      "Het zal de toekenning van sociale voorzieningen eerlijker en evenwichtiger maken."
    ),
    category_labels = c("Eens (5-7)", "Neutraal (4)", "Oneens (1-3)"),
    wave_data_list = list("Resultaten (Wave 1)" = "dat"), # Aangenomen enkele wave, wijzig indien nodig
    analyze_over_time = FALSE,
    time_tab_order = "not_first",
    y_axis_label_time = "", tip_text_time = "", reverse_cats_time = FALSE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  # --- NIEUWE SECTIE HIER TOEGEVOEGD ---
  section4 = list(
    section_title = "Verantwoordelijkheid voor schadelijke AI-inhoud",
    section_icon = "mdi scale-balance",
    question_text = "De verantwoordelijkheid voor schadelijke content (bijv. nepnieuws of gemanipuleerde video's) die door Kunstmatige Intelligentie (AI) is gemaakt, ligt vooral bij...",
    vars_prefix = "HARAI",
    vars_indices = "1:4",
    var_labels = c(
      "...het bedrijf achter de AI-technologie.",
      "...de persoon die de AI heeft gebruikt om de content te maken.",
      "...de sociale mediaplatforms die de content hebben verspreid.",
      "...de overheid omdat ze niet genoeg regels heeft opgesteld."
    ),
    category_labels = c("Eens (5-7)", "Neutraal (4)", "Oneens (1-3)"),
    wave_data_list = list("Mei 2025" = "dat3", "December 2024" = "dat2"),
    analyze_over_time = TRUE,
    time_tab_order = "not_first",
    y_axis_label_time = "",
    tip_text_time = "",
    reverse_cats_time = FALSE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  # --- NIEUWE SECTIE HIER TOEGEVOEGD ---
  section5 = list(
    section_title = "Zorgen over AI",
    section_icon = "mdi alert-circle-outline",
    question_text = "Er wordt veel gesproken over de mogelijke nadelen van Kunstmatige Intelligentie (AI). Verschillende mensen hebben verschillende ideeën over wat een probleem is.<br><br><b>Hoeveel zorgen maakt u zich over de volgende zaken?</b>",
    vars_prefix = "PRAI",
    vars_indices = "1:5",
    var_labels = c(
      "AI gebruiken om content te maken die sommige mensen beledigt",
      "AI gebruiken om mensen te misleiden of te manipuleren om iets te kopen",
      "AI gebruiken om mensen te misleiden of te manipuleren voor een politiek doel",
      "Wanneer het niet duidelijk is dat content door AI is gemaakt",
      "AI gebruiken om banen van mensen over te nemen"
    ),
    category_labels = c("Veel zorgen (5-7)", "Enige zorgen (4)", "Geen zorgen (1-3)"),
    wave_data_list = list("Mei 2025" = "dat3", "December 2024" = "dat2"), # Beschikbaar in wave 2 en 3
    analyze_over_time = TRUE,
    time_tab_order = "not_first",
    y_axis_label_time = "% van de respondenten die zich 'Veel zorgen' (5-7) maken",
    tip_text_time = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.<br><strong>Percentage toont degenen die zich 'Veel zorgen' (5-7) maken.</strong>",
    reverse_cats_time = TRUE,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section6 = list(
    section_title = "Aandacht voor AI in het nieuws",
    section_icon = "mdi newspaper-variant-outline",
    question_text = "Wat vindt u van de hoeveelheid aandacht die wordt besteed aan Kunstmatige Intelligentie (AI) in het nieuws?",
    vars_list = "MC",
    var_labels = c("Hoeveelheid aandacht voor AI in het nieuws"),
    category_labels = c("Te veel (5-7)", "Ongeveer goed (4)", "Te weinig (1-3)"),
    wave_data_list = list("Mei 2025" = "dat3", "December 2024" = "dat2", "Maart 2024" = "dat"),
    analyze_over_time = T,
    time_tab_order = "not_first",
    y_axis_label_time = "% van de respondenten die 'Te veel' (5-7) vinden",
    tip_text_time = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.<br><strong>Percentage toont degenen die 'Te veel' (5-7) kozen.</strong>",
    reverse_cats_time = T,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section7 = list(
    section_title = "Perceptie van AI in de media",
    section_icon = "mdi:movie-open-outline",
    question_text = "Wat is volgens u het beeld van Kunstmatige Intelligentie (AI) in de media?",
    vars_list = "AI_PERCEP",
    var_labels = c("Beeldvorming van AI in de media"),
    category_labels = c("Positief (5-7)", "Neutraal (4)", "Negatief (1-3)"),
    wave_data_list = list("Mei 2025" = "dat3", "December 2024" = "dat2", "Maart 2024" = "dat"),
    analyze_over_time = T,
    time_tab_order = "not_first",
    y_axis_label_time = "% van de respondenten die 'Positief' (5-7) denken",
    tip_text_time = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.<br><strong>Percentage toont degenen die 'Positief' (5-7) kozen.</strong>",
    reverse_cats_time = T,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  )
)
# --- 4. UITVOERING: GENEREER HET RAPPORTBESTAND ---
# Definieer parameters die voor alle secties hetzelfde zijn
global_params <- list(
  group_vars = c("Naar Leeftijd" = "age_groups", "Naar Geslacht" = "geslacht", "Naar Opleiding" = "oplcat", "Naar Politiek" = "pol_cat")
)
# Stel de naam voor het uitvoerbestand in
output_file <- "_site/nl/attitudes.qmd"
file_conn <- file(output_file, "w", encoding = "UTF-8")
# Schrijf de initiële delen van het Quarto-bestand
writeLines(c(yaml_header, setup_chunk), file_conn)
# Loop door de configuratie om elke sectie te maken
for (section_params in sections_config) {
  # Combineer de specifieke sectieparameters met de globale
  full_params <- c(section_params, global_params)
  # Roep de functie aan om de markdown en R-code voor de sectie te genereren
  section_md <- do.call(create_report_section, full_params)
  # Schrijf de gegenereerde inhoud naar het bestand
  writeLines(section_md, file_conn)
}
# Voeg het definitieve logo en het sluitende tabsetpaneel toe
closing_elements <- '
:::
<br>
<center>
<img src="algosoclogo.png" width="13%">
</center>
'
writeLines(closing_elements, file_conn)
# Sluit de bestandsverbinding
close(file_conn)
# --- 5. VOLTOOIING ---
cat(paste("\n✅ Succes! Het rapport is gegenereerd op:", normalizePath(output_file), "\n"))
# Optioneel: Render het rapport onmiddellijk na het aanmaken ervan
quarto::quarto_render(output_file)
######### Engagement #########
sections_config <- list(
  section1 = list(
    section_title = "Vermogen om AI te herkennen en te beïnvloeden",
    section_icon = "fluent text-grammar-settings-20-regular",
    question_text = "Met kunstmatige intelligentie (AI) kunnen computerprogramma's zelfstandig taken uitvoeren en ook zelf leren. AI wordt steeds vaker gebruikt in de samenleving, en ook in online media. Veel websites gebruiken AI om u meer content en advertenties te laten zien die u leuk vindt. De volgende vragen gaan over het aanbod (zoals artikelen, films of liedjes) dat u te zien krijgt op websites en apps voor nieuws en entertainment (bijv. Netflix of Spotify).<br><br><b>In welke mate bent u het eens of oneens met de volgende stellingen?</b>",
    vars_prefix = "DIGIQ",
    vars_indices = "1:5",
    var_labels = c(
      "Ik herken het als een website of app AI gebruikt om de content aan mij aan te passen.",
      "Ik herken het als specifieke content mij wordt aanbevolen door AI.",
      "Ik weet waar ik de instellingen kan vinden om personalisatie door AI te wijzigen of uit te schakelen.",
      "Ik weet hoe ik toegang kan krijgen tot de gegevens die AI-systemen gebruiken om content op mij af te stemmen.",
      "Ik weet hoe ik kan beïnvloeden welke content mij door AI wordt aanbevolen."
    ),
    category_labels = c("Eens (5-7)", "Neutraal (4)", "Oneens (1-3)"),
    y_axis_label_time = "% van de respondenten die het ermee eens is (5-7)",
    tip_text_time = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.<br><strong>Percentage toont degenen die het 'Eens' (5-7) zijn met de stelling.</strong>",
    reverse_cats_time = T # Geen omkering nodig voor deze sectie
  ),
  section2 = list(
    section_title = "Gebruik van AI-systemen",
    section_icon = "material-symbols-light interactive-space-outline-rounded",
    question_text = "<b>Hoe vaak heeft u in het afgelopen jaar gebruik gemaakt van...</b>",
    vars_prefix = "ADM",
    vars_indices = "1:2",
    var_labels = c("...tekstgeneratoren zoals ChatGPT?", "...beeldgeneratoren zoals Midjourney?"),
    category_labels = c("Vaak (5-7)", "Soms (4)", "Zelden (1-3)"),
    y_axis_label_time = "% van de respondenten die 'Vaak' (5-7) kozen",
    tip_text_time = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.<br><strong>Percentage toont degenen die 'Vaak' (5-7) kozen.</strong>",
    reverse_cats_time = T # Omkering nodig voor de tijdsgrafiek van deze sectie
  ),
  section3 = list(
    section_title = "Blootstelling aan AI",
    section_icon = "lets-icons:view-light",
    question_text = "Kunstmatige Intelligentie (AI) kan worden gebruikt om kunstmatige inhoud te maken zoals tekst, afbeeldingen en video's.<br><br><b>Hoe vaak denkt u dat u de volgende dingen het afgelopen jaar op sociale media bent tegengekomen?</b>",
    vars_prefix = "EXPO",
    vars_indices = "1:3",
    var_labels = c("Teksten gemaakt door AI", "Foto's of afbeeldingen gemaakt door AI", "Video's gemaakt door AI"),
    category_labels = c("Vaak (5-7)", "Soms (4)", "Zelden (1-3)"),
    y_axis_label_time = "% van de respondenten die 'Vaak' (5-7) kozen",
    tip_text_time = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.<br><strong>Percentage toont degenen die 'Vaak' (5-7) kozen.</strong>",
    reverse_cats_time = T # Omkering nodig voor de tijdsgrafiek van deze sectie
  ),
  section4 = list(
    section_title = "Bekendheid met AI-termen",
    section_icon = "carbon ibm-watson-knowledge-studio",
    question_text = "<b>Hoe bekend bent u met de volgende termen?</b>",
    vars_prefix = "KAI",
    vars_indices = "1:5",
    var_labels = c("Generatieve AI", "ChatGPT", "Deepfakes", "Algoritmen", "Chatbots"),
    category_labels = c("Bekend (5-7)", "Enigszins bekend (4)", "Onbekend (1-3)"),
    y_axis_label_time = "% van de respondenten die 'Bekend' (5-7) kozen",
    tip_text_time = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.<br><strong>Percentage toont degenen die 'Bekend' (5-7) kozen.</strong>",
    reverse_cats_time = T # Omkering nodig voor de tijdsgrafiek van deze sectie
  )
)
# --- 4. UITVOERING: GENEREER HET RAPPORTBESTAND ---
global_params <- list(
  wave_data_list = list("Mei 2025" = "dat3", "December 2024" = "dat2", "Maart 2024" = "dat"),
  group_vars = c("Naar Leeftijd" = "age_groups", "Naar Geslacht" = "geslacht", "Naar Opleiding" = "oplcat", "Naar Politiek" = "pol_cat"),
  analyze_over_time = TRUE,
  time_tab_order = "not_first"
)
output_file <- "_site/nl/engagement.qmd"
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
# --- 5. VOLTOOIING ---
cat(paste("\n✅ Succes! Het rapport is gegenereerd op:", normalizePath(output_file), "\n"))
quarto::quarto_render(output_file)
######### Sociale Media #########
# Alle rapportinhoud wordt hier gedefinieerd. Elk lijstitem is een hoofdsectie van het rapport.
sections_config <- list(
  section1 = list(
    section_title = "Contentmoderatie op sociale media",
    section_icon = "mdi gavel",
    question_text = "Socialemediabedrijven doen vaak aan 'contentmoderatie', zoals het verwijderen van content die niet aan de regels voldoet. Nieuwsorganisaties worden soms beschuldigd van het verspreiden van desinformatie, waardoor socialemediabedrijven hun nieuwsberichten verwijderen.<br><br><b>In welke mate bent u het eens met de volgende uitspraken?</b>",
    vars_prefix = "CMN",
    vars_indices = "1:6",
    var_labels = c(
      "Ik vertrouw erop dat een socialemediaplatform kan bepalen of iets desinformatie is of niet.",
      "Socialemediaplatforms moeten content die zij als desinformatie beschouwen, beperken of verwijderen, zelfs als deze afkomstig is van nieuwsorganisaties.",
      "Socialemediaplatforms mogen alleen nieuwsinhoud beperken of verwijderen als ze de nieuwsorganisaties vooraf op de hoogte stellen.",
      "De vrijheid van nieuwsorganisaties wordt bedreigd door de contentmoderatie van socialemediaplatforms.",
      "Nieuwsorganisaties moeten de mogelijkheid hebben om beslissingen over contentmoderatie van socialemediaplatforms aan te vechten.",
      "Bepaalde nieuwsorganisaties zouden door socialemediabedrijven meer gemodereerd moeten worden dan andere."
    ),
    category_labels = c("Eens (5-7)", "Neutraal (4)", "Oneens (1-3)"),
    wave_data_list = list("Mei 2025" = "dat3", "December 2024" = "dat2"),
    analyze_over_time = TRUE,
    time_tab_order = "not_first",
    y_axis_label_time = "% van de respondenten die 'Eens' (5-7) kozen",
    tip_text_time = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.<br><strong>Percentage toont degenen die 'Eens' (5-7) kozen.</strong>",
    reverse_cats_time = T, # Komt overeen met logica in origineel bestand
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  section2 = list(
    section_title = "Waargenomen zelfbeïnvloeding door politieke advertenties",
    section_icon = "mdi bullhorn",
    question_text = "We stellen u nu enkele vragen over politieke advertenties op sociale media.<br><br><b>Deze stellingen gaan over uw eigen mening of gevoel:</b>",
    # Gebruik van vars_list omdat variabelenamen niet opeenvolgend genummerd zijn
    vars_list = c("SM_ADS1a", "SM_ADS1b", "SM_ADS1c"),
    var_labels = c(
      "Ik heb het altijd door als ik beïnvloed word door politieke advertenties op sociale media.",
      "Het is mij eerder overkomen dat ik een politieke partij leuker vond na het zien van hun advertenties op sociale media.",
      "Ik steun eerder een kandidaat die wordt aanbevolen door influencers die ik volg."
    ),
    category_labels = c("Eens (5-7)", "Neutraal (4)", "Oneens (1-3)"),
    wave_data_list = list("Mei 2025" = "dat3", "December 2024" = "dat2"),
    analyze_over_time = T,
    time_tab_order = "not_first",
    y_axis_label_time = "",
    tip_text_time = "",
    reverse_cats_time = T,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  ),
  # --- NIEUWE SECTIE 10 HIER TOEGEVOEGD ---
  section3 = list(
    section_title = "Waargenomen invloed van politieke advertenties op anderen",
    section_icon = "mdi bullhorn-outline",
    question_text = "Deze stellingen gaan over uw mening over andere mensen:",
    # Gebruik van vars_list omdat variabelenamen niet opeenvolgend genummerd zijn
    vars_list = c("SM_ADS2a", "SM_ADS2b", "SM_ADS2c"),
    var_labels = c(
      "Anderen hebben het altijd door als ze beïnvloed worden door politieke advertenties op sociale media.",
      "Ik denk dat anderen een politieke partij leuker gaan vinden na het zien van hun advertenties op sociale media.",
      "Ik geloof dat anderen eerder een kandidaat steunen die wordt aanbevolen door influencers die ze volgen."
    ),
    category_labels = c("Eens (5-7)", "Neutraal (4)", "Oneens (1-3)"),
    wave_data_list = list("Mei 2025" = "dat3", "December 2024" = "dat2"),
    analyze_over_time = T,
    time_tab_order = "not_first",
    y_axis_label_time = "",
    tip_text_time = "",
    reverse_cats_time = T,
    rangeone = "1:3", rangetwo = "4", rangethree = "5:7", drange = "5:7"
  )
)
# --- 4. UITVOERING: GENEREER HET RAPPORTBESTAND ---
# Definieer parameters die voor alle secties hetzelfde zijn
global_params <- list(
  group_vars = c("Naar Leeftijd" = "age_groups", "Naar Geslacht" = "geslacht", "Naar Opleiding" = "oplcat", "Naar Politiek" = "pol_cat")
)
# Stel de naam voor het uitvoerbestand in
output_file <- "_site/nl/socialmedia.qmd"
file_conn <- file(output_file, "w", encoding = "UTF-8")
# Schrijf de initiële delen van het Quarto-bestand
writeLines(c(yaml_header, setup_chunk), file_conn)
# Loop door de configuratie om elke sectie te maken
for (section_params in sections_config) {
  # Combineer de specifieke sectieparameters met de globale
  full_params <- c(section_params, global_params)
  # Roep de functie aan om de markdown en R-code voor de sectie te genereren
  section_md <- do.call(create_report_section, full_params)
  # Schrijf de gegenereerde inhoud naar het bestand
  writeLines(section_md, file_conn)
}
# Voeg het definitieve logo en het sluitende tabsetpaneel toe
closing_elements <- '
:::
<br>
<center>
<img src="algosoclogo.png" width="13%">
</center>
'
writeLines(closing_elements, file_conn)
# Sluit de bestandsverbinding
close(file_conn)
# --- 5. VOLTOOIING ---
cat(paste("\n✅ Succes! Het rapport is gegenereerd op:", normalizePath(output_file), "\n"))
# Optioneel: Render het rapport onmiddellijk na het aanmaken ervan
quarto::quarto_render(output_file)
# source("generate_nl.R")
######### Waarden #########
# Nieuwe aangepaste functie specifiek voor de sectie "Waarden"
# Aangepaste functie specifiek voor de structuur van de sectie "Waarden"
# --- BIJGEWERKTE GENERATORFUNCTIE ---
create_values_section <- function(config, time_tab_order = "time_first") { # <-- Argument hier toegevoegd
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
  over_time_header <- '\n# Over de tijd\n\n::: {.panel-tabset}\n'
  overall_time_chunk <- paste(
    '## Algemeen', '::: {.panel-tabset}',
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
      '::: {.panel-tabset}', '## Algemeen',
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
  # --- NIEUWE LOGICA VOOR TABVOLGORDE ---
  tabs_ordered <- if (time_tab_order == "time_first") {
    c(over_time_content, wave_tabs_content)
  } else {
    c(wave_tabs_content, over_time_content)
  }
  paste(main_header, paste(tabs_ordered, collapse = "\n\n"), ':::', sep = '\n')
}
# --- 3. CONFIGURATIE: DEFINIEER ALLE RAPPORTSECTIES ---
# --- CONFIGURATIE voor het volledige 4-tab rapport ---
# Deel 1: Config voor de unieke "Belangrijkste waarden" sectie
values_section_config <- list(
  title = "Belangrijkste waarden",
  icon = "healthicons:world-care-outline",
  question = "Wat beschouwt u als de 5 belangrijkste waarden voor geautomatiseerde besluitvormingssystemen? [alleen de belangrijkste waarde wordt getoond]",
  tiptext = "U kunt over de legenda zweven of erop klikken om groepen te markeren, te verwijderen of toe te voegen, wat de grafiek beter leesbaar kan maken.",
  group_vars = c("Naar Leeftijd" = "age_groups", "Naar Geslacht" = "geslacht", "Naar Opleiding" = "oplcat", "Naar Politiek" = "pol_cat"),
  wave_data = list("Mei 2025" = "dat3", "December 2024" = "dat2", "Maart 2024" = "dat"),
  time_tab_order = "not_first"
)
# Deel 2: Config voor de drie standaardsecties ("Nuttigheid", "Risico's", "Eerlijkheid")
standard_sections_config <- list(
  usefulness = list(
    section_title = "Nuttigheid van AI",
    section_icon = "fluent data-usage-16-regular",
    question_text = "> In de meeste voorspellingen zal geautomatiseerde besluitvorming in de toekomst vaker voorkomen. Als dat zo is, in welke mate bent u het dan oneens of eens met de volgende uitspraken:\n\n<blockquote class='question-text'>Meer automatische besluitvorming in de nieuwsmedia/het rechtssysteem/de gezondheidszorg zal **nuttig** zijn.</blockquote>",
    vars_list = c("ADSR1", "ADSRt2_1", "ADSRt3_1"),
    var_labels = c("Nieuwsmedia", "Rechtssysteem", "Gezondheidszorg"),
    category_labels = c("Eens (5-7)", "Neutraal (4)", "Oneens (1-3)")
  ),
  riskiness = list(
    section_title = "Risico's van AI",
    section_icon = "solar danger-outline",
    question_text = "In de meeste voorspellingen zal geautomatiseerde besluitvorming in de toekomst vaker voorkomen. Als dat zo is, in welke mate bent u het dan oneens of eens met de volgende uitspraken:\n\n<blockquote class='question-text'>Meer automatische besluitvorming in de nieuwsmedia/het rechtssysteem/de gezondheidszorg zal **riskant** zijn.</blockquote>",
    vars_list = c("ADSR2", "ADSRt2_2", "ADSRt3_2"),
    var_labels = c("Nieuwsmedia", "Rechtssysteem", "Gezondheidszorg"),
    category_labels = c("Eens (5-7)", "Neutraal (4)", "Oneens (1-3)")
  ),
  fairness = list(
    section_title = "Eerlijkheid van AI",
    section_icon = "material-symbols-light balance",
    question_text = "In de meeste voorspellingen zal geautomatiseerde besluitvorming in de toekomst vaker voorkomen. Als dat zo is, in welke mate bent u het dan oneens of eens met de volgende uitspraken:\n\n<blockquote class='question-text'>Meer automatische besluitvorming in de nieuwsmedia/het rechtssysteem/de gezondheidszorg zal **eerlijker** zijn.</blockquote>",
    vars_list = c("ADSR3", "ADSRt2_3", "ADSRt3_3"),
    var_labels = c("Nieuwsmedia", "Rechtssysteem", "Gezondheidszorg"),
    category_labels = c("Eens (5-7)", "Neutraal (4)", "Oneens (1-3)")
  )
)
# Deel 3: Globale parameters die van toepassing zijn op alle standaardsecties
global_params_for_standard_sections <- list(
  wave_data_list = list("Maart 2024" = "dat", "December 2024" = "dat2", "Mei 2025" = "dat3"),
  group_vars = values_section_config$group_vars,
  analyze_over_time = TRUE,
  time_tab_order = "time_first",
  y_axis_label_time = "% van de respondenten die het ermee eens is (5–7)",
  tip_text_time = values_section_config$tiptext,
  reverse_cats_time = TRUE,
  drange = "5:7"
)
# --- 4. UITVOERING: GENEREER EN SCHRIJF HET RAPPORTBESTAND ---
# --- UITVOERING: Genereer en schrijf het enkele rapportbestand ---
output_file <- "_site/nl/values.qmd"
file_conn <- file(output_file, "w", encoding = "UTF-8")
# Schrijf de initiële delen van het Quarto-bestand
writeLines(c(yaml_header, setup_chunk), file_conn)
# 1. Genereer en schrijf de aangepaste "Belangrijkste waarden" sectie
values_md <- create_values_section(values_section_config, time_tab_order = "not_first")
writeLines(values_md, file_conn)
# 2. Genereer en schrijf de drie standaardsecties door door hun config te lussen
for (section_params in standard_sections_config) {
  # Combineer de specifieke sectieconfig met de globale parameters
  full_params <- c(section_params, global_params_for_standard_sections)
  # Genereer de markdown voor deze sectie
  section_md <- do.call(create_report_section, full_params)
  # Schrijf het naar het bestand
  writeLines(section_md, file_conn)
}
# Schrijf de definitieve sluitingselementen
writeLines(closing_elements, file_conn)
close(file_conn)
# --- VOLTOOIING ---
cat(paste("\n✅ Succes! Het rapport is gegenereerd op:", normalizePath(output_file), "\n"))
# Optioneel: Render het rapport onmiddellijk
quarto::quarto_render(output_file)
beepr::beep(21)
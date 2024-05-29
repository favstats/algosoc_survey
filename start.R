library(tidyverse)


dir("_site", full.names = T, recursive = T) %>% 
  keep(~str_detect(.x, "\\.qmd")) %>%
  keep(~str_detect(.x, "/en/")) %>%
  discard(~str_detect(.x, "about|index")) %>%
  walk(~{
    
    filename <- str_remove_all(.x, "_site/en/") %>% 
      paste0("_site/nl/", .)
    
    print(filename)
    
    .x %>% 
      # .[3] %>% 
      read_lines() %>% 
      #### engagement
      str_replace_all("The page is divided into four main sections",
                      "De pagina is verdeeld in vier hoofdsecties") %>% 
      
      str_replace_all('<a href="#iconify-fluent-text-grammar-settings-20-regular-ability-to-recognize-and-influence-ai" class="buttontop">',
                      '<a href="#iconify-fluent-text-grammar-settings-20-regular-vermogen-om-ai-te-herkennen-en-te-beinvloeden" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify fluent text-grammar-settings-20-regular >\\}\\} Ability to Recognize and Influence AI',
                      '{{< iconify fluent text-grammar-settings-20-regular >}} Vermogen om AI te herkennen en te beïnvloeden') %>% 
      
      
      str_replace_all('<a href="#iconify-material-symbols-light-interactive-space-outline-rounded-usage-of-ai-systems" class="buttontop">',
                      '<a href="#iconify-material-symbols-light-interactive-space-outline-rounded-gebruik-van-ai-systemen" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify material-symbols-light interactive-space-outline-rounded >\\}\\} Usage of AI Systems',
                      '{{< iconify material-symbols-light interactive-space-outline-rounded >}} Gebruik van AI-systemen') %>% 
      
      
      str_replace_all('<a href="#iconify-lets-icons-view-light-exposure-to-ai" class="buttontop">',
                      '<a href="#iconify-lets-icons-view-light-contact-met-ai" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify lets-icons view-light >\\}\\} Exposure to AI',
                      '{{< iconify lets-icons view-light >}} Contact met AI') %>% 
      
      
      str_replace_all('<a href="#iconify-carbon-ibm-watson-knowledge-studio-familiarity-with-ai-terms" class="buttontop">',
                      '<a href="#iconify-carbon-ibm-watson-knowledge-studio-bekendheid-met-ai-termen" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify carbon ibm-watson-knowledge-studio >\\}\\} Familiarity with AI terms',
                      '{{< iconify carbon ibm-watson-knowledge-studio >}} Bekendheid met AI-termen')%>% 
      
      
      str_replace_all('Each section allows you to examine overall trends as well as breakdowns by age\\, gender\\, education level\\, and political leaning',
                      'Je kunt algemene trends onderzoeken in de verschillende secties, met de mogelijkheid om te differentiëren  naar leeftijd, geslacht, opleidingsniveau en politieke voorkeur') %>% 
      
      
      
      str_replace_all('With artificial intelligence \\(AI\\), computer programs can perform tasks themselves and also learn themselves. AI is increasingly used in society, and also in online media. Many websites use AI to show you more content and ads you like. The following questions are about the offers \\(such as articles, films or songs\\) that are shown to you on websites and apps for news and entertainment \\(e.g. Netflix or Spotify\\)',
                      'Met kunstmatige intelligentie (AI) kunnen computerprogramma’s zelf taken uitvoeren en ook zelf leren. AI wordt steeds meer gebruikt in de samenleving, en ook in de online media. Veel websites gebruiken AI om je meer inhoud en advertenties te laten zien die je leuk vindt. De volgende vragen gaan over het aanbod (zoals artikelen, films of liedjes) dat aan jou getoond wordt op websites en apps voor nieuws en entertainment (bijv. Netflix of Spotify)') %>% 
      str_replace_all('To what extent do you agree or disagree with the following statements',
                      'In welke mate bent u het eens of oneens met de volgende uitspraken')   %>% 
      
      
      str_replace_all('I recognize it when a website or app uses AI to adapt the content to me',
                      'Ik herken het als een website of app AI gebruikt om de content aan mij aan te passen') %>% 
      str_replace_all('I recognize it when specific content is recommended to me by AI',
                      'Ik herken het als specifieke content mij wordt aanbevolen door AI') %>%   
      str_replace_all('I know where to find the settings to change or disable AI personalization',
                      'Ik weet waar ik de instellingen kan vinden om personalisatie door AI te wijzigen of uit te zetten') %>% 
      str_replace_all('I know how to access the data that AI systems use to tailor content to me',
                      'Ik weet hoe ik bij de gegevens kan komen die AI-systemen gebruiken om content aan mij aan te passen') %>% 
      str_replace_all('I know how I can influence what content is recommended to me by AI',
                      'Ik weet hoe ik invloed kan uitoefenen op welke content mij door AI wordt aanbevolen') %>% 
      
      str_replace_all('"Agree \\(5-7\\)", "Neither agree or disagree \\(4\\)", "Disagree \\(1-3\\)"',
                      '"Eens \\(5-7\\)", "Noch eens, noch oneens \\(4\\)", "Oneens \\(1-3\\)"')  %>% 
      
      str_replace_all('How often in the past year did you use...',
                      'Hoe vaak gebruikte u in het afgelopen jaar...')  %>% 
      
      str_replace_all('...text generators like ChatGPT',
                      '...tekst-generatoren zoals ChatGPT')  %>% 
      str_replace_all('...image generators like Midjourney',
                      '...afbeelding-generatoren zoals Midjourney') %>% 
      str_replace_all('"Often \\(5-7\\)","Sometimes \\(4\\)","Rare \\(1-3\\)"',
                      '"Vaak (5-7)", "Soms (4)", "Zeldzaam (1-3)"') %>% 
      
      
      
      str_replace_all('Artificial Intelligence \\(AI\\) can be used to create artificial content such as text, images and videos',
                      "Kunstmatige Intelligentie (AI) kan worden gebruikt om kunstmatige inhoud te maken, zoals tekst,
afbeeldingen en video's")  %>% 
      str_replace_all('How often do you think you have come across the following things on social media in the past year',
                      'Hoe vaak denkt u dat u in het afgelopen jaar de volgende dingen op sociale media bent tegengekomen')   %>% 
      
      str_replace_all('Texts made by AI',
                      'Teksten gemaakt door AI')  %>% 
      str_replace_all('Photos or images made by AI',
                      "Foto's of afbeeldingen gemaakt door AI")  %>% 
      str_replace_all('Videos made by AI',
                      "Video’s gemaakt door AI")    %>% 
      
      str_replace_all('How familiar are you with the following terms',
                      'Hoe bekend bent u met de volgende zaken')  %>% 
      str_replace_all('"Familiar \\(5-7\\)","Somewhat familiar \\(4\\)","Unfamiliar \\(1-3\\)"',
                      '"Bekend \\(5-7\\)", "Enigszins bekend \\(4\\)", "Onbekend \\(1-3\\)"')  %>%      
      str_replace_all('Generative AI',
                      'Generatieve AI')  %>% 
      str_replace_all('Algorithms',
                      'Algoritmen')   %>% 
      
      
      str_replace_all('Overall',
                      'Algemeen')  %>% 
      str_replace_all('By Age',
                      'Op Leeftijd')  %>% 
      str_replace_all('By Gender',
                      'Op Geslacht')  %>% 
      str_replace_all('By Education',
                      'Op Opleiding')  %>% 
      str_replace_all('By Politics',
                      'Op Politiek') %>% 
      
      
      ## attitudes
      str_replace_all("The page is divided into three main sections",
                      "De pagina is verdeeld in drie hoofdsecties") %>%    
      
      str_replace_all('<a href="#iconify-fluent-emoji-high-contrast-detective-detecting-ai-generated-content" class="buttontop">',
                      '<a href="#iconify-fluent-emoji-high-contrast-detecteren-van-door-ai-gegenereerde-content" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify fluent-emoji-high-contrast detective >\\}\\} Detecting AI-Generated Content',
                      '{{< iconify fluent-emoji-high-contrast detective >}} Detecteren van door AI gegenereerde content') %>% 
      
      
      str_replace_all('<a href="#iconify-mingcute-safe-lock-fill-trust-in-adm-systems" class="buttontop">',
                      '<a href="#iconify-mingcute-safe-lock-fill-Vertrouwen-in-ai-systemen" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify mingcute safe-lock-fill >\\}\\} Trust in ADM Systems',
                      '{{< iconify mingcute safe-lock-fill >}} Vertrouwen in AI-systemen') %>% 
      
      
      str_replace_all('<a href="#iconify-game-icons-sands-of-time-future-of-adm-systems" class="buttontop">',
                      '<a href="#iconify-game-icons-sands-of-time-toekomst-van-ai-systemen" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify game-icons sands-of-time >\\}\\} Future of ADM Systems',
                      '{{< iconify game-icons sands-of-time >}} Toekomst van AI-systemen') %>% 
      
      str_replace_all("How confident are you that the following groups can tell the difference between content created by AI and content created by people",
                      "Hoe zeker bent u ervan dat de volgende groepen het verschil kunnen zien tussen content gemaakt door AI en content gemaakt door mensen") %>%
      str_replace_all("People from the general population",
                      "Mensen uit de algemene bevolking") %>%
      str_replace_all("Members of Parliament",
                      "Parlementsleden") %>%
      str_replace_all("Judges",
                      "Rechters") %>%
      str_replace_all("Journalists",
                      "Journalisten") %>%
      str_replace_all("Police officers",
                      "Politieagenten") %>%
      str_replace_all("Doctors",
                      "Artsen") %>%
      str_replace_all("Friends\\/family",
                      "Vrienden/familie") %>%
      str_replace_all("Yourself",
                      "Uzelf") %>%
      str_replace_all('"Confident \\(5-7\\)", "Somewhat confident \\(4\\)", "Not confident \\(1-3\\)"',
                      '"Zeker \\(5-7\\)", "Enigszins zeker \\(4\\)", "Niet zeker \\(1-3\\)"')  %>% 
      
      
      str_replace_all('We ask you a number of additional questions about automatic decision-making \\(ADM\\) systems. These are computer programs that make decisions that used to be made by people. These decisions are made automatically by computers based on data',
                      "We stellen u nog een aantal extra vragen over automatische besluitvormingssystemen. Dat zijn computerprogramma's die beslissingen maken die vroeger door mensen werden genomen. Deze beslissingen worden automatisch door computers genomen op basis van gegevens")  %>%   
      str_replace_all("How much do you trust automated decision-making systems to properly determine\\.\\.\\.",
                      "Hoeveel vertrouwen hebt u erin dat automatische besluitvormingssystemen goed kunnen bepalen...") %>%
      str_replace_all("Social Welfare",
                      "...of iemand in aanmerking komt voor een bijstandsuitkering") %>%
      str_replace_all("The News You See",
                      "...welke nieuwsberichten aanbevolen worden aan iemand") %>%
      str_replace_all("Cancer Diagnosis",
                      "...of iemand kanker heeft") %>%
      str_replace_all('"Trust \\(5-7\\)", "Somewhat trust \\(4\\)", "Little trust \\(1-3\\)"',
                      '"Vertrouwen \\(5-7\\)", "Enigszins vertrouwen \\(4\\)", "Weinig vertrouwen \\(1-3\\)"')  %>% 
      
      str_replace_all('If automated decision-making \\(ADM\\) becomes more common in the future, what do you think will happen',
                      'Als geautomatiseerde besluitvorming in de toekomst vaker voorkomt, wat denkt u dan dat er zal gebeuren')  %>% 
      str_replace_all("If the news is automated, journalists will be able to focus more on investigative journalism",
                      "Als het nieuws wordt geautomatiseerd, zullen journalisten zich meer kunnen richten op onderzoeksjournalistiek") %>%
      str_replace_all("I will only get news items that interest me",
                      "Ik zal alleen nog maar nieuwsberichten krijgen die me interesseren") %>%
      str_replace_all("It will make reporting fairer and more balanced",
                      "Het zal de verslaggeving eerlijker en evenwichtiger maken") %>%
      str_replace_all("If certain law enforcement tasks are automated, the police will have more time to focus on major crimes",
                      "Als bepaalde taken voor handhaven van de wet worden geautomatiseerd, zal de politie meer
tijd hebben om zich op grote misdrijven te richten") %>%
      str_replace_all("It will make the determination of social benefits fairer and more balanced",
                      "De bepaling van sociale uitkeringen zal er eerlijker en evenwichtiger door worden") %>%
      
      
      #### values
      str_replace_all('<a href="#iconify-healthicons-world-care-outline-most-important-values" class="buttontop">',
                      '<a href="#iconify-healthicons-world-care-outline-belangrijkste-waarden" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify healthicons world-care-outline >\\}\\} Most Important Values',
                      '{{< iconify healthicons world-care-outline >}} Belangrijkste Waarden') %>% 
      
      
      str_replace_all('<a href="#iconify-fluent-data-usage-16-regular-usefulness-of-ai" class="buttontop">',
                      '<a href="#iconify-fluent-data-usage-16-regular-nuttigheid-van-ai" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify fluent data-usage-16-regular >\\}\\} Usefulness of AI',
                      '{{< iconify fluent data-usage-16-regular >}} Nuttigheid van AI') %>% 
      
      
      str_replace_all('<a href="#iconify-solar-danger-outline-riskiness-of-ai" class="buttontop">',
                      '<a href="#iconify-solar-danger-outline-risicos-van-ai" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify solar danger-outline >\\}\\} Riskiness of AI',
                      "{{< iconify solar danger-outline >}} Risico's van AI") %>% 
      
      str_replace_all('<a href="#iconify-material-symbols-light-balance-fairness-of-ai" class="buttontop">',
                      '<a href="#iconify-material-symbols-light-balance-eerlijkheid-van-ai" class="buttontop">') %>% 
      str_replace_all('\\{\\{< iconify material-symbols-light balance >\\}\\} Fairness of AI',
                      '{{< iconify material-symbols-light balance >}} Eerlijkheid van AI') %>% 
      
      
      
      str_replace_all("What do you consider the 5 most important values for automated decision-making systems\\? \\[only top important value shown\\]",
                      "Wat beschouw je als de 5 belangrijkste waarden voor geautomatiseerde besluitvormingssystemen? [alleen de belangrijkste waarde wordt getoond]") %>%
      str_replace_all("Respect for user privacy",
                      "Respect voor privacy van gebruikers") %>%
      str_replace_all("Ease of use",
                      "Gebruiksvriendelijkheid") %>%
      str_replace_all("Politically neutral",
                      "Politiek neutraal") %>%
      str_replace_all("Freedom to choose information",
                      "Vrijheid om te kiezen welke informatie je krijgt") %>%
      str_replace_all("Non-discriminatory",
                      "Niet-discriminerend") %>%
      str_replace_all("Accessibility",
                      "Toegankelijkheid (voor bijv. voor mensen met een beperking of minderheden)") %>%
      str_replace_all("Transparency about decision making",
                      "Transparantie over hoe beslissingen worden genomen") %>%
      str_replace_all("Human oversight",
                      "Menselijk toezicht op deze systemen") %>%
      str_replace_all("% Naming As Number 1 Important Issue",
                      "% Aangeven als Nummer 1 Belangrijkste Kwestie") %>%
      
      
      str_replace_all("In most predictions, automated decision-making will become more common in the future. If so, to what extent do you disagree or agree with the following statements",
                      "In de meeste voorspellingen zal geautomatiseerde besluitvorming in de toekomst vaker gaan voorkomen. Als dat inderdaad zo is, in welke mate bent u het dan oneens of eens met de volgende uitspraken") %>%
      str_replace_all("More automatic decision-making in news media/the legal system/healthcare will be \\*\\*useful\\*\\*",
                      "Meer automatische besluitvorming in de nieuwsmedia/het rechtsstelsel/de gezondheidszorg zal **nuttig** zijn") %>%      
      str_replace_all("More automatic decision-making in news media/the legal system/healthcare will be \\*\\*risky\\*\\*",
                      "Meer automatische besluitvorming in de nieuwsmedia/het rechtsstelsel/de gezondheidszorg zal **riskant** zijn") %>%       
      str_replace_all("More automatic decision-making in news media/the legal system/healthcare will be \\*\\*more fair\\*\\*",
                      "Meer automatische besluitvorming in de nieuwsmedia/het rechtsstelsel/de gezondheidszorg zal **eerlijker** zijn") %>%    
      str_replace_all('"News Media", "Legal System", "Healthcare"',
                      '"Nieuwsmedia", "Rechtsstelsel", "Gezondheidszorg"') %>%
      str_replace_all('https://www.centerdata.nl/en/',
                      'https://www.centerdata.nl/') %>%
      
      
      
      
      
      write_lines(filename)
    
  })


dir("_site", full.names = T, recursive = T) %>% 
  keep(~str_detect(.x, "\\.qmd")) %>% 
  keep(~str_detect(.x, "/en/|/nl/")) %>%
  walk_progress(quarto::quarto_render)

gert::git_add(".")
gert::git_commit("plsfix")
gert::git_push()




  
  
  
  
  

  
  
  


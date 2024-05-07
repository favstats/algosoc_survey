

dir("_site", full.names = T, recursive = T) %>% 
  keep(~str_detect(.x, "\\.qmd")) %>% 
  walk_progress(quarto::quarto_render)

gert::git_add(".")
gert::git_commit("plsfix")
gert::git_push()



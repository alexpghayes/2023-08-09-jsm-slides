library(targets)

tar_option_set(
  packages = c("glue", "ggdag", "ggraph", "here", "tidygraph", "tidyverse")
)

data(glasgow, package = "netmediate")

options(clustermq.scheduler = "multicore")

tar_source()

list(
  
  tar_target(
    glasgow1,
    clean_glasgow(glasgow[[1]])
  ),
  
  tar_target(
    mediating_figure,
    make_mediating_figure(),
    format = "file"
  ),
  
  tar_target(
    homophily_mediating_figure,
    make_homophily_mediating_figure(),
    format = "file"
  ),
  
  tar_target(
    homophily_mediating_contagion_peer_figur,
    make_homophily_mediating_contagion_peer_figure(),
    format = "file"
  ),
  
  tar_target(
    homophily_mediating_interference_peer_figure,
    make_homophily_mediating_interference_peer_figure(),
    format = "file"
  ),
  
  tar_target(
    tobacco_figure,
    make_tobacco_figure(glasgow1),
    format = "file"
  ),
  
  tar_target(
    sex_figure,
    make_sex_figure(glasgow1),
    format = "file"
  )
)

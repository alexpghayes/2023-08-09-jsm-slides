library(tidygraph)
library(tidyverse)
library(here)
library(ggraph)
library(ggdag)

make_dag_figures <- function() {
  c(
    make_mediating_figure(),
    make_homophily_mediating_figure(),
    make_homophily_mediating_contagion_peer_figure(),
    make_homophily_mediating_contagion_peer_noisy_figure(),
    make_homophily_mediating_contagion_latent_figure(),
    make_homophily_mediating_contagion_latent_noisy_figure(),
    make_homophily_mediating_interference_peer_figure(),
    make_homophily_mediating_interference_latent_figure(),
    make_homophily_confounding_figure()
  )
}

status_colors <- c(
  "exposure" = "firebrick",
  "outcome" = "steelblue",
  "observed" = "black",
  "latent" = "grey"
)

make_mediating_figure <- function() {

  coords <- tibble(
    name = c("Ci", "Ti", "Xi", "Yi"),
    x = c(1, 0, 2, 1),
    y = c(2, 1, 1, 0)
  )

  #  example from the dagitty package
  dag <- dagify(
    Yi ~ Ti + Ci + Xi,
    Xi ~ Ti + Ci,
    Ti ~ Ci,
    coords = coords,
    labels = c(
      Ci = "C[i %.% phantom(j)]",
      Ti = "T[i]",
      Xi = "X[i %.% phantom(j)]",
      Yi = "Y[i]"
    ),
    exposure = "Ti",
    outcome = "Yi"
  )

  dag |>
    tidy_dagitty() |>
    node_status() |>
    mutate(
      status = replace_na(as.character(status), "observed")
    ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = status)) +
    scale_color_manual(values = status_colors, guide = "none") +
    geom_dag_edges_diagonal(
      arrow = grid::arrow(length = grid::unit(7, "pt"), type = "closed")
    ) +
    geom_dag_text(aes(label = label), parse = TRUE, size = 5) +
    theme_dag(base_size = 22, base_family = "Computer Modern")

  path <- here::here("figures", "dags", "mediating.png")

  ggsave(
    path,
    height = 3.5,
    width = 3.5,
    dpi = 500
  )

  path
}

make_homophily_mediating_figure <- function() {

  coords <- tibble(
    name = c("Ci", "Ti", "Xi", "Yi", "Aij", "Xj", "Cj", "Yj", "Tj"),
    x = c(1, 0, 2, 1, 3, 4, 5, 5, 6),
    y = c(2, 1, 1, 0, 0, 1, 2, 0, 1)
  )

  #  example from the dagitty package
  dag <- dagify(
    Yi ~ Ti + Ci + Xi,
    Aij ~ Xi + Xj,
    Xi ~ Ti + Ci,
    Ti ~ Ci,
    Yj ~ Tj + Cj + Xj,
    Xj ~ Tj + Cj,
    Tj ~ Cj,
    coords = coords,
    latent = c("Xi", "Xj"),
    labels = c(
      Ci = "C[i %.% phantom(j)]",
      Ti = "T[i]",
      Xi = "X[i %.% phantom(j)]",
      Yi = "Y[i]",
      Aij = "A[ij]",
      Xj = "X[j %.% phantom(j)]",
      Cj = "C[j %.% phantom(j)]",
      Yj = "Y[j]",
      Tj = "T[j]"
    ),
    exposure = "Ti",
    outcome = "Yi"
  )

  dag |>
    tidy_dagitty() |>
    node_status() |>
    mutate(
      status = replace_na(as.character(status), "observed")
    ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = status)) +
    scale_color_manual(values = status_colors, guide = "none") +
    geom_dag_edges_diagonal(
      arrow = grid::arrow(length = grid::unit(7, "pt"), type = "closed")
    ) +
    geom_dag_text(aes(label = label), parse = TRUE, size = 5) +
    theme_dag(base_size = 22, base_family = "Computer Modern")

  path <- here::here("figures", "dags", "homophily-mediating.png")

  ggsave(
    path,
    height = 3.5,
    width = 3.5 * 16/9,
    dpi = 500
  )

  path
}

make_homophily_confounding_figure <- function() {

  coords <- tibble(
    name = c("Ci", "Ti", "Xi", "Yi", "Aij", "Xj", "Cj", "Yj", "Tj"),
    x = c(1, 0, 2, 1, 3, 4, 5, 5, 6),
    y = c(2, 1, 1, 0, 0, 1, 2, 0, 1)
  )

  #  example from the dagitty package
  dag <- dagify(
    Yi ~ Ti + Ci + Xi,
    Aij ~ Xi + Xj,
    Xi ~ Ci,
    Ti ~ Ci + Xi,
    Yj ~ Tj + Cj + Xj,
    Xj ~ Cj,
    Tj ~ Xj + Cj,
    coords = coords,
    latent = c("Xi", "Xj"),
    labels = c(
      Ci = "C[i %.% phantom(j)]",
      Ti = "T[i]",
      Xi = "X[i %.% phantom(j)]",
      Yi = "Y[i]",
      Aij = "A[ij]",
      Xj = "X[j %.% phantom(j)]",
      Cj = "C[j %.% phantom(j)]",
      Yj = "Y[j]",
      Tj = "T[j]"
    ),
    exposure = "Ti",
    outcome = "Yi"
  )

  dag |>
    tidy_dagitty() |>
    node_status() |>
    mutate(
      status = replace_na(as.character(status), "observed")
    ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = status)) +
    scale_color_manual(values = status_colors, guide = "none") +
    geom_dag_edges_diagonal(
      arrow = grid::arrow(length = grid::unit(7, "pt"), type = "closed")
    ) +
    geom_dag_text(aes(label = label), parse = TRUE, size = 5) +
    theme_dag(base_size = 22, base_family = "Computer Modern")

  path <- here::here("figures", "dags", "homophily-confounding.png")

  ggsave(
    path,
    height = 3.5,
    width = 3.5,
    dpi = 500
  )

  path
}

make_homophily_mediating_interference_peer_figure <- function() {

  coords <- tibble(
    name = c("Ci", "Ti", "Xi", "Yi", "Aij", "Xj", "Cj", "Yj", "Tj"),
    x = c(1, 0, 2, 1, 3, 4, 5, 5, 6),
    y = c(2, 1, 1, 0, 0, 1, 2, 0, 1)
  )

  #  example from the dagitty package
  dag <- dagify(
    Yi ~ Tj + Aij + Ti + Ci + Xi,
    Aij ~ Xi + Xj,
    Xi ~ Ti + Ci,
    Ti ~ Ci,
    Yj ~ Ti + Aij + Tj + Cj + Xj,
    Xj ~ Tj + Cj,
    Tj ~ Cj,
    coords = coords,
    latent = c("Xi", "Xj"),
    labels = c(
      Ci = "C[i %.% phantom(j)]",
      Ti = "T[i]",
      Xi = "X[i %.% phantom(j)]",
      Yi = "Y[i]",
      Aij = "A[ij]",
      Xj = "X[j %.% phantom(j)]",
      Cj = "C[j %.% phantom(j)]",
      Yj = "Y[j]",
      Tj = "T[j]"
    ),
    exposure = "Ti",
    outcome = "Yi"
  )

  dag |>
    tidy_dagitty() |>
    node_status() |>
    mutate(
      status = replace_na(as.character(status), "observed")
    ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = status)) +
    scale_color_manual(values = status_colors, guide = "none") +
    geom_dag_edges_diagonal(
      arrow = grid::arrow(length = grid::unit(7, "pt"), type = "closed")
    ) +
    geom_dag_text(aes(label = label), parse = TRUE, size = 5) +
    theme_dag(base_size = 22, base_family = "Computer Modern")

  path <- here::here("figures", "dags", "homophily-mediating-interference-peer.png")

  ggsave(
    path,
    height = 3.5,
    width = 3.5 * 16/9,
    dpi = 500
  )

  path
}

make_homophily_mediating_interference_latent_figure <- function() {

  coords <- tibble(
    name = c("Ci", "Ti", "Xi", "Yi", "Aij", "Xj", "Cj", "Yj", "Tj"),
    x = c(1, 0, 2, 1, 3, 4, 5, 5, 6),
    y = c(2, 1, 1, 0, 0, 1, 2, 0, 1)
  )

  #  example from the dagitty package
  dag <- dagify(
    Yi ~ Tj + Ti + Ci + Xi + Xj,
    Aij ~ Xi + Xj,
    Xi ~ Ti + Ci,
    Ti ~ Ci,
    Yj ~ Ti + Tj + Cj + Xj + Xi,
    Xj ~ Tj + Cj,
    Tj ~ Cj,
    coords = coords,
    latent = c("Xi", "Xj"),
    labels = c(
      Ci = "C[i %.% phantom(j)]",
      Ti = "T[i]",
      Xi = "X[i %.% phantom(j)]",
      Yi = "Y[i]",
      Aij = "A[ij]",
      Xj = "X[j %.% phantom(j)]",
      Cj = "C[j %.% phantom(j)]",
      Yj = "Y[j]",
      Tj = "T[j]"
    ),
    exposure = "Ti",
    outcome = "Yi"
  )

  dag |>
    tidy_dagitty() |>
    node_status() |>
    mutate(
      status = replace_na(as.character(status), "observed")
    ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = status)) +
    scale_color_manual(values = status_colors, guide = "none") +
    geom_dag_edges_diagonal(
      arrow = grid::arrow(length = grid::unit(7, "pt"), type = "closed")
    ) +
    geom_dag_text(aes(label = label), parse = TRUE, size = 5) +
    theme_dag(base_size = 22, base_family = "Computer Modern")

  path <- here::here("figures", "dags", "homophily-mediating-interference-latent.png")

  ggsave(
    path,
    height = 3.5,
    width = 3.5,
    dpi = 500
  )

  path
}

make_homophily_mediating_contagion_peer_figure <- function() {

  coords <- tibble(
    name = c("Ci", "Ti", "Xi", "Yi", "Aij", "Xj", "Cj", "Yj", "Tj"),
    x = c(1, 0, 2, 1, 3, 4, 5, 5, 6),
    y = c(2, 1, 1, 0, 0.25, 1, 2, 0, 1)
  )

  #  example from the dagitty package
  dag <- dagify(
    Yi ~ Yj + Aij + Ti + Ci + Xi,
    Aij ~ Xi + Xj,
    Xi ~ Ti + Ci,
    Ti ~ Ci,
    Yj ~ Yi + Aij + Tj + Cj + Xj,
    Xj ~ Tj + Cj,
    Tj ~ Cj,
    coords = coords,
    latent = c("Xi", "Xj"),
    labels = c(
      Ci = "C[i %.% phantom(j)]",
      Ti = "T[i]",
      Xi = "X[i %.% phantom(j)]",
      Yi = "Y[i]",
      Aij = "A[ij]",
      Xj = "X[j %.% phantom(j)]",
      Cj = "C[j %.% phantom(j)]",
      Yj = "Y[j]",
      Tj = "T[j]"
    ),
    exposure = "Ti",
    outcome = "Yi"
  )

  dag |>
    tidy_dagitty() |>
    node_status() |>
    mutate(
      status = replace_na(as.character(status), "observed")
    ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = status)) +
    scale_color_manual(values = status_colors, guide = "none") +
    geom_dag_edges_diagonal(
      arrow = grid::arrow(length = grid::unit(7, "pt"), type = "closed")
    ) +
    geom_dag_text(aes(label = label), parse = TRUE, size = 5) +
    theme_dag(base_size = 22, base_family = "Computer Modern")

  path <- here::here("figures", "dags", "homophily-mediating-contagion-peer.png")

  ggsave(
    path,
    height = 3.5,
    width = 3.5 * 16/9,
    dpi = 500
  )

  path
}


make_homophily_mediating_contagion_latent_figure <- function() {

  coords <- tibble(
    name = c("Ci", "Ti", "Xi", "Yi", "Aij", "Xj", "Cj", "Yj", "Tj"),
    x = c(1, 0, 2, 1, 3, 4, 5, 5, 6),
    y = c(2, 1, 1, 0, 0.25, 1, 2, 0, 1)
  )

  #  example from the dagitty package
  dag <- dagify(
    Yi ~ Yj + Ti + Ci + Xi + Xj,
    Aij ~ Xi + Xj,
    Xi ~ Ti + Ci,
    Ti ~ Ci,
    Yj ~ Yi + Tj + Cj + Xj + Xi,
    Xj ~ Tj + Cj,
    Tj ~ Cj,
    coords = coords,
    latent = c("Xi", "Xj"),
    labels = c(
      Ci = "C[i %.% phantom(j)]",
      Ti = "T[i]",
      Xi = "X[i %.% phantom(j)]",
      Yi = "Y[i]",
      Aij = "A[ij]",
      Xj = "X[j %.% phantom(j)]",
      Cj = "C[j %.% phantom(j)]",
      Yj = "Y[j]",
      Tj = "T[j]"
    ),
    exposure = "Ti",
    outcome = "Yi"
  )

  dag |>
    tidy_dagitty() |>
    node_status() |>
    mutate(
      status = replace_na(as.character(status), "observed")
    ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = status)) +
    scale_color_manual(values = status_colors, guide = "none") +
    geom_dag_edges_diagonal(
      arrow = grid::arrow(length = grid::unit(7, "pt"), type = "closed")
    ) +
    geom_dag_text(aes(label = label), parse = TRUE, size = 5) +
    theme_dag(base_size = 22, base_family = "Computer Modern")

  path <- here::here("figures", "dags", "homophily-mediating-contagion-latent.png")

  ggsave(
    path,
    height = 3.5,
    width = 3.5,
    dpi = 500
  )

  path
}


clean_glasgow <- function(graph) {
  graph |>
    activate(edges) |>
    filter(friendship != "Structurally missing") |>
    activate(nodes) |>
    mutate(
      in_degree = centrality_degree(mode = "in"),
      out_degree = centrality_degree(mode = "out")
    ) |>
    filter(in_degree > 0 | out_degree > 0) |>
    mutate(
      tobacco_dimaria = as.numeric(tobacco_int > 1),
      alcohol_dimaria = as.numeric(alcohol_int > 2),
      cannabis_dimaria = as.numeric(cannabis_int > 2)
    )
}

make_tobacco_figure <- function(graph) {
  set.seed(30)

  tobacco_plot <- ggraph(graph, layout = "lgl") +
    geom_edge_fan(
      arrow = arrow(length = unit(1, "mm")),
      end_cap = circle(2.5, "mm"), alpha = 0.2
    ) +
    geom_node_point(aes(size = in_degree, color = tobacco_fct)) +
    scale_color_brewer(type = "seq", palette = "Set1", direction = -1, guide = guide_legend()) +
    guides(colour = guide_legend(override.aes = list(size = 5))) +
    scale_size_continuous(guide = "none") +
    labs(
      # title = "Friendships in a secondary school in Glasgow",
      color = "Tobacco use",
      # subtitle = glue("Teenage Friends and Lifestyle Study, 1995 (wave {time})"),
      # caption = "Each node represents one student"
    ) +
    theme_graph(
      base_size = 26,
      plot_margin = margin(0, 0, 0, 0)
    ) +
    theme(
      legend.position = "bottom"
    )

  path <- here("figures", "glasgow", "tobacco.png")

  ggsave(
    path,
    plot = tobacco_plot,
    dpi = 600,
    width = 9,
    height = 6
  )

  path
}

make_sex_figure <- function(graph) {
  set.seed(30)

  sex_plot <- ggraph(graph, layout = "lgl") +
    geom_edge_fan(
      arrow = arrow(length = unit(1, "mm")),
      end_cap = circle(2.5, "mm"), alpha = 0.2
    ) +
    geom_node_point(aes(size = in_degree, color = sex_fct)) +
    scale_color_brewer(type = "seq", palette = "Set2", direction = -1, guide = guide_legend()) +
    guides(colour = guide_legend(override.aes = list(size = 5))) +
    scale_size_continuous(guide = "none") +
    labs(
      # title = "Friendships in a secondary school in Glasgow",
      color = "Recorded sex",
      # subtitle = glue("Teenage Friends and Lifestyle Study, 1995 (wave {time})"),
      # caption = "Each node represents one student"
    ) +
    theme_graph(
      base_size = 26,
      plot_margin = margin(0, 0, 0, 0)
    ) +
    theme(
      legend.position = "bottom"
    )

  path <- here("figures", "glasgow", "sex.png")

  ggsave(
    path,
    plot = sex_plot,
    dpi = 600,
    width = 9,
    height = 6
  )

  path
}

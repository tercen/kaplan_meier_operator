suppressPackageStartupMessages({
  library(tercen)
  library(dplyr, warn.conflicts = FALSE)
  library(survival)
  library(broom)
  library(survminer)
  library(tim)
})

ctx = tercenCtx()

plot.ci <- ctx$op.value("plot.ci", as.logical, F)
plot.risk.table <- ctx$op.value("plot.risk.table", as.logical, F)

df_list <- ctx$select(c(".y", ".x", ".ci", ".ri", ctx$colors)) %>% 
  group_by(.ci, .ri) %>%
  group_split()

cov_names <- unlist(ctx$colors)
form_right <- paste0(cov_names, collapse = " + ")
form <- formula(paste0("Surv(.x, .y) ~ ", form_right))

grid.draw.ggsurvplot <- function(x) {
  survminer:::print.ggsurvplot(x, newpage = FALSE)
}


table.list <- lapply(df_list, function(df) {
  fit <- surv_fit(form, data = df)
  p <- ggsurvplot(
    fit,
    data = df,
    size = 0.5,
    conf.int = plot.ci,
    pval = FALSE,
    risk.table = plot.risk.table,
    font.title = 10,
    font.subtitle = 8,
    font.caption = 8,
    font.legend = 8,
    font.x = 8,
    font.y = 8,
    font.tickslab = 8,
    fontsize = 3,
    ggtheme = theme_classic()
  )
  
  p$table <- p$table +
    theme(
      plot.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )
  
  plot_file <- tim::save_plot(
    p,
    width = 3,
    height = ifelse(plot.risk.table, 4, 3),
    units = "in"
  )
  
  df_plot <- tim::plot_file_to_df(plot_file) %>%
    mutate(.ci = df$.ci[1], .ri = df$.ri[1]) %>%
    ctx$addNamespace()
  
  df_out <- tidy(fit) %>%
    mutate(.ci = df$.ci[1], .ri = df$.ri[1]) %>%
    ctx$addNamespace()
  
  return(list(df_out, df_plot))
  
})

df_out <- lapply(table.list, "[[", 1) %>% bind_rows()
df_plot <- lapply(table.list, "[[", 2) %>% bind_rows()

ctx$save(list(df_out, df_plot))

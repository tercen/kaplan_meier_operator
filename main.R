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
width.scale.factor <- ctx$op.value("width.scale.factor", as.numeric, 1)
heigth.scale.factor <- ctx$op.value("heigth.scale.factor", as.numeric, 1)
text.scale.factor <- ctx$op.value("text.scale.factor", as.numeric, 1)

df_list <- ctx$select(c(".y", ".x", ".ci", ".ri", ctx$colors)) %>% 
  group_by(.ci, .ri) %>%
  group_split()

cov_names <- unlist(ctx$colors)
form_right <- paste0(cov_names, collapse = " + ")
form <- formula(paste0("Surv(.x, .y) ~ ", form_right))

grid.draw.ggsurvplot <- function(x) {
  survminer:::print.ggsurvplot(x, newpage = FALSE)
}

text.size.large <-text.scale.factor * 10
text.size.small <-text.scale.factor * 8

table.list <- lapply(df_list, function(df) {
  fit <- surv_fit(form, data = df)
  p <- ggsurvplot(
    fit,
    data = df,
    size = 0.5,
    conf.int = plot.ci,
    pval = FALSE,
    risk.table = plot.risk.table,
    font.title = text.size.large,
    font.subtitle = text.size.small,
    font.caption = text.size.small,
    font.legend = text.size.small,
    font.x = text.size.small,
    font.y = text.size.small,
    font.tickslab = text.size.small,
    fontsize = 3 * text.scale.factor,
    ggtheme = theme_classic()
  )
  
  p$table <- p$table +
    theme(
      plot.title = element_text(size = text.size.large),
      axis.text = element_text(size = text.size.small),
      axis.title = element_text(size = text.size.small)
    )
  
  plot_file <- tim::save_plot(
    p,
    width = width.scale.factor * 3,
    height = ifelse(plot.risk.table, heigth.scale.factor * 4, heigth.scale.factor * 3),
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

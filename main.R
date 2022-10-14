library(tercen)
library(dplyr, warn.conflicts = FALSE)
library(survival)
library(broom)
library(survminer)
library(tim)

ctx = tercenCtx()

df <- ctx$select(c(".y", ".x", ".ci", ".ri", ctx$colors))

cov_names <- unlist(ctx$colors)
form_right <- paste0(cov_names, collapse = " + ")
form <- formula(paste0("Surv(.x, .y) ~ ", form_right))

fit <- surv_fit(form, data = df)

p <- ggsurvplot(
  fit,
  data = df,
  size = 0.5,
  conf.int = TRUE,
  pval = FALSE,
  risk.table = FALSE,
  font.title = 10,
  font.subtitle = 8,
  font.caption = 8,
  font.legend = 8,
  font.x = 8,
  font.y = 8,
  font.tickslab = 8,
  ggtheme = theme_bw()
)

plot_file <- tim::save_plot(p$plot, width = 3, height = 3, units = "in")

df_plot <- tim::plot_file_to_df(plot_file) %>%
  mutate(.ci = 0L, .ri = 0L) %>%
  ctx$addNamespace()

df_out <- tidy(fit) %>%
  mutate(.ci = 0L, .ri = 0L) %>%
  ctx$addNamespace()

ctx$save(list(df_out, df_plot))

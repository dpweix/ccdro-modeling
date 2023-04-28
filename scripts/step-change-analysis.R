library("tidyverse")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 15),
             plot.subtitle = element_text(hjust = 0.5, size = 10),
             legend.text = element_text(size = 10),
             strip.placement = "outside",
             strip.background = element_blank())
library("here")
library("lubridate")
library("forecast")
library("kableExtra")

# Load Data -------------------------------------------------------------------
df_step_1 <- readRDS(here("data", "step-change-1.rds"))
df_step_2 <- readRDS(here("data", "step-change-2.rds"))

# Prepare data for modeling ---------------------------------------------------

# Separate sequences, remove drain phase & startup,
# remove data to make observations single second instead of quarter second.
n_trim <- 240

prep_data <- function(df) {
  df |> 
    filter(drain_valve == 0) |> 
    select(-drain_valve) |> 
    group_split(sequence) |> 
    map(\(x){
      x |> 
        slice(-c(1:n_trim)) #|> 
        #slice(seq(1, n(), by = 4))
    })
}

df_1 <- prep_data(df_step_1)
df_2 <- prep_data(df_step_1)

# Predict Next Sequence from Previous Sequence --------------------------------

# Focus on predictions of c_r, p_f, p_r.
# Models that we consider: 
# c_r: AR(3), ARIMA(0, 1, 0), Linear, Physics Based, Bilinear
# p_f: AR(3), ARX, Linear no c_r, Linear w/ c_r
# p_r: AR(3), ARX, Linear, Quadratic

start <- 100 # forecast begins at observation number 100
n_seq <- length(df_1)

# naming .pred_ar_c_r, .pred_bi_p_f

### Modeling: c_r

# AR
fit_ar_c_r <- df_1 |> 
  map(\(x) {
    Arima(x$c_r,
          order = c(3, 0, 0),
          include.constant = FALSE,
          method = "ML")
  })

pred_ar_c_r <- 
  map2(df_1[-1], fit_ar_c_r[-n_seq],
       \(x, m) {
         forecast(head(x$c_r, 3),
                  nrow(x) - 3,
                  model = m)$mean |> 
           as.numeric() |> 
           append(rep(NA, 3), 0)
       })

# ARI
fit_ari_c_r <- df_1 |> 
  map(\(x) {
    Arima(x$c_r,
          order = c(3, 1, 0),
          include.constant = TRUE,
          method = "ML")
  })

pred_ari_c_r <- 
  map2(df_1[-1], fit_ari_c_r[-n_seq],
       \(x, m) {
         forecast(head(x$c_r, 3),
                  nrow(x) - 3,
                  model = m)$mean |> 
           as.numeric() |> 
           append(rep(NA, 3), 0)
       })

# Physics Based Model
c.in    <- 2.71
v.loop  <- 15
delta.t <- .25
R       <- map_dbl(df_1, \(x) { median((x$c_r - x$c_p)/x$c_r)})

pred_phy_c_r <- map2(df_1[-1], R[-1],
                     \(x, R) {
                       y <- x$c_r
                       
                       for(i in 1:(nrow(x)-1)) {
                         y[i+1] <- y[i] + (delta.t*(c.in-(1-R)*y[i])*(x$f_p[i]/60))/v.loop
                       }
                       y
                     })

# Bilinear Model
fit_bi_c_r <- df_1 |> 
  map(\(x) {
    lm(c_r ~ lag(c_r)*lag(f_p), data = x)
  })

pred_bi_c_r <- map2(df_1[-1], fit_bi_c_r[-n_seq],
                    \(x, m) {
                      y <- x$c_r
                      a <- m$coefficients
                      
                      for(i in 1:(nrow(x)-1)) {
                        y[i+1] = a[1] + a[2]*y[i] + a[3]*x$f_p[i] + a[4]*y[i]*x$f_p[i]
                      }
                      
                      y
                    })

### Modeling: p_f

# ARX
fit_arx_p_f <- df_1 |> 
  map(\(x) {
    Arima(x$p_f,
          order = c(3, 1, 0),
          xreg = select(x, "c_r", "f_p") |> as.matrix(),
          include.constant = FALSE,
          method = "ML")
  })

pred_arx_p_f <- ### MUTATE TO FIX C_R
  map2(df_1[-1], fit_arx_p_f[-n_seq],
       \(x, m) {
         Arima(x$p_f[1:3],
               xreg = select(x[c(1:3), ], "c_r", "f_p") |> as.matrix(),
               model = m) |> 
           forecast(h = nrow(x) - 3,
                    xreg = select(x[-c(1:3), ], "c_r", "f_p") |> as.matrix(),
                    model = m) |> 
           getElement("mean") |> 
           as.numeric() |> 
           append(rep(NA, 3), 0)
       })

# Linear model, w/o c_r
fit_lnc_p_f <- df_1 |> 
  map(\(x) {
    lm(p_f ~ lag(f_p), data = x)
  })

pred_lnc_p_f <-
  map2(df_1[-1], fit_lnc_p_f[-n_seq], 
       \(x, m) {
         predict(m, x) |> as.numeric()
       })

# Linear model, w/ c_r
fit_lwc_p_f <- df_1 |> 
  map(\(x) {
    lm(p_f ~ lag(f_p) + lag(c_r), data = x)
  })

pred_phy_p_f <- 
  pmap(list(df_1[-1], pred_phy_c_r, fit_lwc_p_f[-n_seq]),
       \(x, y, m) {
    predict(m, mutate(x, c_r = y)) |> as.numeric()
  })


pred_bi_p_f <- 
  pmap(list(df_1[-1], pred_bi_c_r, fit_lwc_p_f[-n_seq]),
       \(x, y, m) {
         predict(m, mutate(x, c_r = y)) |> as.numeric()
       })

### Modeling: p_r
# ARX
fit_arx_p_r <- df_1 |> 
  map(\(x) {
    Arima(x$p_r,
          order = c(3, 1, 0),
          xreg = select(x, "c_r", "f_r") |> as.matrix(),
          include.constant = FALSE,
          method = "ML")
  })

pred_arx_p_r <- 
  map2(df_1[-1], fit_arx_p_r[-n_seq],
       \(x, m) {
         Arima(x$p_r[1:3],
               xreg = select(x[1:3, ], "c_r", "f_r") |> as.matrix(),
               model = m) |> 
           forecast(h = nrow(x) - 3,
                    xreg = select(x[-c(1:3), ], "c_r", "f_r") |> as.matrix(),
                    model = m) |> 
           getElement("mean") |> 
           as.numeric() |> 
           append(rep(NA, 3), 0)
       })

# Linear model, w or w/o quadratic
fit_lin_p_r <- df_1 |> 
  map(\(x) {
    lm(p_r ~ lag(f_r), data = x)
  })

fit_quad_p_r <- df_1 |> 
  map(\(x) {
    lm(p_r ~ lag(f_r) + I(lag(f_r)^2) + 0, data = x)
  })

pred_lin_p_r <- 
  map2(df_1[-1], fit_lin_p_r[-n_seq], 
       \(x, m) {
         predict(m, x) |> as.numeric()
       })

pred_quad_p_r <- 
  map2(df_1[-1], fit_quad_p_r[-n_seq], 
       \(x, m) {
         predict(m, x) |> as.numeric()
       })

# Add predictions to df
df_pred_1 <-
  pmap_dfr(list(df_1[-1],
                pred_ar_c_r , pred_ari_c_r, pred_phy_c_r, pred_bi_c_r,
                pred_arx_p_f, pred_lnc_p_f, pred_phy_p_f, pred_bi_p_f,
                pred_arx_p_r, pred_lin_p_r, pred_quad_p_r),
           \(x, 
             ar_c_r , ari_c_r, phy_c_r, bi_c_r,
             arx_p_f, lnc_p_f, phy_p_f, bi_p_f,
             arx_p_r, lin_p_r, quad_p_r) {
             mutate(x,
                    .pred_ar_c_r   = ar_c_r,
                    .pred_ari_c_r  = ari_c_r,
                    .pred_phy_c_r  = phy_c_r,
                    .pred_bi_c_r   = bi_c_r,
                    .pred_arx_p_f  = arx_p_f,
                    .pred_lnc_p_f  = lnc_p_f,
                    .pred_phy_p_f  = phy_p_f,
                    .pred_bi_p_f   = bi_p_f,
                    .pred_arx_p_r  = arx_p_r,
                    .pred_lin_p_r  = lin_p_r,
                    .pred_quad_p_r = quad_p_r)
           })

# Visualizations --------------------------------------------------------------

# Model Colors
color <- viridis::turbo(8)

model_colors <- c("Observed" = "black",
                  "AR" = color[2],
                  "ARI" = color[3],
                  "ARX" = color[4],
                  "Univariate" = color[5],
                  "Bilinear" = color[6],
                  "Physics" = color[7],
                  "Quadratic" = color[8])


# c_r: Predictions
df_pred_1 |> 
  filter(sequence == 6) |> 
  pivot_longer(contains("c_r")) |> 
  mutate(name = str_replace_all(name, c(".pred_ar_c_r"  = "AR",
                                        ".pred_ari_c_r" = "ARI",
                                        ".pred_phy_c_r" = "Physics",
                                        ".pred_bi_c_r"  = "Bilinear",
                                        "c_r" = "Observed"))) |> 
  mutate(name = factor(name, c("AR", "ARI", "Physics", "Bilinear", "Observed"))) |> 
  ggplot(aes(hr, value, color = name)) +
  geom_line() +
  scale_color_manual(values = model_colors) +
  labs(x = "Time (Hours)", y = "Conductivity (mS/cm)", color = "",
       title = "Model Comparison: Conductivity")

ggsave(here("figures", "model-pred-c_r.png"),
       width = 25, height = 10, units = 'cm')

# p_f: Predictions
df_pred_1 |> 
  filter(sequence == 6) |> 
  pivot_longer(contains("p_f")) |> 
  mutate(name = str_replace_all(name, c(".pred_arx_p_f" = "ARX",
                                        ".pred_lnc_p_f" = "Univariate",
                                        ".pred_phy_p_f" = "Physics",
                                        ".pred_bi_p_f" = "Bilinear",
                                        "p_f" = "Observed"))) |> 
  mutate(name = factor(name, c("ARX", "Univariate", "Physics", "Bilinear", "Observed"))) |> 
  ggplot(aes(hr, value, color = name)) +
  geom_line() +
  scale_color_manual(values = model_colors) +
  labs(x = "Time (Hours)", y = "Feed Pump Power Consumption (kW)", color = "",
       title = "Model Comparison: Feed Pump Power Consumption")

ggsave(here("figures", "model-pred-p_f.png"),
       width = 25, height = 10, units = 'cm')

# p_r: Predictions
df_pred_1 |> 
  filter(sequence == 6) |> 
  pivot_longer(contains("p_r")) |> 
  mutate(name = str_replace_all(name, c(".pred_arx_p_r" = "ARX",
                                        ".pred_lin_p_r" = "Univariate",
                                        ".pred_quad_p_r" = "Quadratic",
                                        "p_r" = "Observed"))) |> 
  mutate(name = factor(name, c("ARX", "Univariate", "Quadratic", "Observed"))) |> 
  ggplot(aes(hr, value, color = name)) +
  geom_line() +
  scale_color_manual(values = model_colors) +
  labs(x = "Time (Hours)", y = "Recirc. Pump Power Consumption (kW)", color = "",
       title = "Model Comparison: Recirc. Pump Power Consumption")

ggsave(here("figures", "model-pred-p_r.png"),
       width = 25, height = 10, units = 'cm')

# Tables ----------------------------------------------------------------------
df_pred_tab <- df_pred_1 |> 
  filter(sequence == 6) |> 
  pivot_longer(matches("[c|p]_[r|f]")) |> 
  mutate(variable = str_extract(name, "[c|p]_[r|f]"),
         type = ifelse(str_detect(name, ".pred"), "prediction", "observation")) |> 
  pivot_wider(id_cols = c("hr", "variable"),
              names_from = type,
              values_from = value,
              values_fn = list) |> 
  unnest(cols = c("observation", "prediction")) |> 
  mutate(model = rep(c("ARX", "Univariate", "Physics", "Bilinear",
                       "ARX", "Univariate", "Quadratic",
                       "AR", "ARI", "Physics", "Bilinear"),
                     nrow(filter(df_pred_1, sequence == 6))))

df_pred_tab |> 
  group_by(variable, model) |> 
  summarise(RMSPE = sqrt(mean((observation - prediction)^2, na.rm = TRUE))) |> 
  mutate(variable = paste0("italic(", str_replace(variable, "_", "["), "])")) |> 
  ggplot(aes(model, RMSPE)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ variable, scales = "free", labeller = label_parsed) +
  labs(title = "RMSPE of each model by variable", x = "")

ggsave(here("figures", "model-comparison.png"),
       width = 25, height = 10, units = 'cm')

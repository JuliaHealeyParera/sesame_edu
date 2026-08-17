library(here)
source('scripts/load_packages.R')

# Creating the visualizations and analysis used in report

# Final models
body_final <- readRDS("models/final/body_fin_betamod.rds")
letters_final <- readRDS( "models/final/letters_fin_betamod.rds")
form_final <- readRDS("models/final/form_fin_betamod.rds")
numb_final <- readRDS("models/final/numb_fin_betamod.rds")
relat_final <- readRDS("models/final/relat_fin_betamod.rds")
clasf_final <- readRDS("models/final/clasf_fin_betamod.rds")


form_initial <- readRDS("models/initial/form_betamod.rds")
numb_initial <- readRDS("models/initial/numb_betamod.rds")

model_objects <- list(
  body    = body_final,
  letters = letters_final,
  form    = form_initial,   
  numb    = numb_initial,   
  relat   = relat_final,
  clasf   = clasf_final
)

extract_la_terms_safe <- function(mod) {
  ftxt <- paste(deparse(formula(mod)), collapse = " ")
  matches <- str_match_all(ftxt,
                           "la\\s*\\(\\s*([^,\\)]+)\\s*,\\s*type\\s*=\\s*['\"]?single['\"]?\\s*\\)")
  if (length(matches) == 0 || nrow(matches[[1]]) == 0) return(character(0))
  terms <- trimws(matches[[1]][,2])
  unique(terms)
}

terms_by_model <- lapply(model_objects, extract_la_terms_safe)
for (nm in names(terms_by_model)) {
  cat("\n---", nm, "(", length(terms_by_model[[nm]]), "terms) ---\n")
  if (length(terms_by_model[[nm]]) == 0) cat("(no la(...) terms detected)\n")
  else cat(paste0(terms_by_model[[nm]], collapse = "\n"), "\n")
}

all_terms <- sort(unique(unlist(terms_by_model)))
presence_tbl <- expand.grid(Predictor = all_terms, Model = names(model_objects),
                            stringsAsFactors = FALSE) |>
  rowwise() |>
  mutate(Kept = ifelse(Predictor %in% terms_by_model[[Model]], "✔", "✖")) |>
  ungroup() |>
  pivot_wider(names_from = Model, values_from = Kept)

### Visualizations ###

# age & viewing frequency interaction effect (Number Improvement)
gg_age_viewcat_improve_numb <- ggplot(sesame_edit, aes(x = age, y = improve_numb, color = viewcat)) +
  geom_point(alpha = 0.25) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c(
    "rare" = "thistle3",
    "once_twice" = "lightskyblue3",
    "three_five" = "darkseagreen3",
    "over_five" = "#D8A7B1"
  )) +
  labs(
    title = "Plot X: Age × Viewing Frequency on Number Improvement",
    x = "Age (months)",
    y = "Number Improvement (%)",
    color = "Viewing\nFrequency"
  ) +
  theme_minimal()


gg_age_viewcat_improve_form <- ggplot(sesame_edit, aes(x = age, y = improve_form, color = viewcat)) +
  geom_point(alpha = 0.25) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c(
    "rare" = "thistle3",
    "once_twice" = "lightskyblue3",
    "three_five" = "darkseagreen3",
    "over_five" = "#D8A7B1"
  )) +
  labs(
    title = "Age × Viewing Frequency on Form Improvement",
    x = "Age (months)",
    y = "Form Improvement (%)",
    color = "Viewing\nFrequency"
  ) +
  theme_minimal()

gg_age_viewcat_improve_relat <- ggplot(sesame_edit, aes(x = age, y = improve_relat, color = viewcat)) +
  geom_point(alpha = 0.25) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c(
    "rare" = "thistle3",
    "once_twice" = "lightskyblue3",
    "three_five" = "darkseagreen3",
    "over_five" = "#D8A7B1"
  )) +
  labs(
    title = "Age × Viewing Frequency on Relations Improvement",
    x = "Age (months)",
    y = "Relations Improvement (%)",
    color = "Viewing\nFrequency"
  ) +
  theme_minimal()

gg_age_viewcat_improve_clasf <- ggplot(sesame_edit, aes(x = age, y = improve_clasf, color = viewcat)) +
  geom_point(alpha = 0.25) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c(
    "rare" = "thistle3",
    "once_twice" = "lightskyblue3",
    "three_five" = "darkseagreen3",
    "over_five" = "#D8A7B1"
  )) +
  labs(
    title = "Age × Viewing Frequency on Classification Improvement",
    x = "Age (months)",
    y = "Classification Improvement (%)",
    color = "Viewing\nFrequency"
  ) +
  theme_minimal()

# setting & viewing frequency interaction effect
gg_setting_viewcat_improve_let <- ggplot(sesame_edit, aes(x = viewcat, y = improve_let, fill = setting)) +
  geom_boxplot(outlier.color = "dimgrey") +
  scale_fill_manual(values = c(
    "home"   = "lightskyblue3",
    "school" = "thistle3"
  )) +
  labs(
    title = "Plot J: Viewing Frequency × Setting on Letter Improvement",
    x = "Viewing Frequency",
    y = "Letter Improvement (%)",
    fill = "Viewing\nSetting"
  ) +
  theme_minimal()

gg_setting_viewcat_improve_numb <- ggplot(sesame_edit, aes(x = viewcat, y = improve_numb, fill = setting)) +
  geom_boxplot(outlier.color = "dimgrey") +
  scale_fill_manual(values = c(
    "home"   = "lightskyblue3",
    "school" = "thistle3"
  )) +
  labs(
    title = "Viewing Frequency × Setting on Number Improvement",
    x = "Viewing Frequency",
    y = "Number Improvement (%)",
    fill = "Viewing\nSetting"
  ) +
  theme_minimal()

gg_setting_viewcat_improve_form <- ggplot(sesame_edit, aes(x = viewcat, y = improve_form, fill = setting)) +
  geom_boxplot(outlier.color = "dimgrey") +
  scale_fill_manual(values = c(
    "home"   = "lightskyblue3",
    "school" = "thistle3"
  )) +
  labs(
    title = "Viewing Frequency × Setting on Form Improvement",
    x = "Viewing Frequency",
    y = "Form Improvement (%)",
    fill = "Viewing\nSetting"
  ) +
  theme_minimal()

gg_setting_viewcat_improve_relat <-ggplot(sesame_edit, aes(x = viewcat, y = improve_relat, fill = setting)) +
  geom_boxplot(outlier.color = "dimgrey") +
  scale_fill_manual(values = c(
    "home"   = "lightskyblue3",
    "school" = "thistle3"
  )) +
  labs(
    title = "Viewing Frequency × Setting on Relations Improvement",
    x = "Viewing Frequency",
    y = "Relations Improvement (%)",
    fill = "Viewing\nSetting"
  ) +
  theme_minimal()

gg_setting_viewcat_improve_clasf <- ggplot(sesame_edit, aes(x = viewcat, y = improve_clasf, fill = setting)) +
  geom_boxplot(outlier.color = "dimgrey") +
  scale_fill_manual(values = c(
    "home"   = "lightskyblue3",
    "school" = "thistle3"
  )) +
  labs(
    title = "Viewing Frequency × Setting on Classification Improvement",
    x = "Viewing Frequency",
    y = "Classification Improvement (%)",
    fill = "Viewing\nSetting"
  ) +
  theme_minimal()

# sex & viewing frequency interaction effect
gg_sex_viewcat_improve_let <- ggplot(sesame_edit, aes(x = viewcat, y = improve_let, fill = sex)) +
  geom_boxplot(alpha = 0.75) +
  scale_fill_manual(values = c("female" = "thistle3", "male" = "lightskyblue3")) +
  labs(
    title = "Plot B: Sex × Viewing Frequency on Letter Improvement",
    y = "Letter Improvement (%)",
    x = "Viewing Frequency",
    fill = "Sex"
  ) +
  theme_minimal()

gg_sex_viewcat_improve_numb <-  ggplot(sesame_edit, aes(x = viewcat, y = improve_numb, fill = sex)) +
  geom_boxplot(alpha = 0.75) +
  scale_fill_manual(values = c("female" = "thistle3", "male" = "lightskyblue3")) +
  labs(
    title = "Sex × Viewing Frequency on Number Improvement",
    y = "Number Improvement (%)",
    x = "Viewing Frequency",
    fill = "Sex"
  ) +
  theme_minimal()

gg_sex_viewcat_improve_form <- ggplot(sesame_edit, aes(x = viewcat, y = improve_form, fill = sex)) +
  geom_boxplot(alpha = 0.75) +
  scale_fill_manual(values = c("female" = "thistle3", "male" = "lightskyblue3")) +
  labs(
    title = "Sex × Viewing Frequency on Form Improvement",
    y = "Form Improvement (%)",
    x = "Viewing Frequency",
    fill = "Sex"
  ) +
  theme_minimal()

gg_sex_viewcat_improve_relat <- ggplot(sesame_edit, aes(x = viewcat, y = improve_relat, fill = sex)) +
  geom_boxplot(alpha = 0.75) +
  scale_fill_manual(values = c("female" = "thistle3", "male" = "lightskyblue3")) +
  labs(
    title = "Sex × Viewing Frequency on Relations Improvement",
    y = "Relations Improvement (%)",
    x = "Viewing Frequency",
    fill = "Sex"
  ) +
  theme_minimal()

gg_sex_viewcat_improve_clasf <- ggplot(sesame_edit, aes(x = viewcat, y = improve_clasf, fill = sex)) +
  geom_boxplot(alpha = 0.75) +
  scale_fill_manual(values = c("female" = "thistle3", "male" = "lightskyblue3")) +
  labs(
    title = "Sex × Viewing Frequency on Classification Improvement",
    y = "Classification Improvement (%)",
    x = "Viewing Frequency",
    fill = "Sex"
  ) +
  theme_minimal()

### Coefficient extracts ###

get_clean_coefs <- function(model) {
  cf <- coef(model)[, 1]            
  cf <- cf[!grepl("alpha|tau21|Intercept", names(cf))]
  return(cf)
}

body_coef <- get_clean_coefs(body_final)
letters_coef <- get_clean_coefs(letters_final)
form_coef <- get_clean_coefs(form_final)
numb_coef <- get_clean_coefs(numb_final)
relat_coef <- get_clean_coefs(relat_final)
clasf_coef <- get_clean_coefs(clasf_final)


body_letters <- outer(body_coef, letters_coef, `-`)
body_form <- outer(body_coef, form_coef, `-`)
body_numb <- outer(body_coef, numb_coef, `-`)
body_relat <- outer(body_coef, relat_coef, `-`)
body_clasf <- outer(body_coef, clasf_coef, `-`)

letters_form <- outer(letters_coef, form_coef, `-`)
letters_numb <- outer(letters_coef, numb_coef, `-`)
letters_relat <- outer(letters_coef, relat_coef, `-`)
letters_clasf <- outer(letters_coef, clasf_coef, `-`)

form_numb <- outer(form_coef, numb_coef, `-`)
form_relat <- outer(form_coef, relat_coef, `-`)
form_clasf <- outer(form_coef, clasf_coef, `-`)

numb_relat <- outer(numb_coef,  relat_coef, `-`)
numb_clasf <- outer(numb_coef,  clasf_coef, `-`)

relat_clasf <- outer(relat_coef, clasf_coef, `-`)

mat_list <- list(
  body_letters = body_letters,
  body_form    = body_form,
  body_numb    = body_numb,
  body_relat   = body_relat,
  body_clasf   = body_clasf,
  letters_form = letters_form,
  letters_numb = letters_numb,
  letters_relat= letters_relat,
  letters_clasf= letters_clasf,
  form_numb    = form_numb,
  form_relat   = form_relat,
  form_clasf   = form_clasf,
  numb_relat   = numb_relat,
  numb_clasf   = numb_clasf,
  relat_clasf  = relat_clasf
)

# Convert to numeric from list
safe_to_numeric_matrix <- function(M, matrixname = NA_character_) {
  if (is.data.frame(M)) M <- as.matrix(M)
  if (is.atomic(M) && !is.matrix(M)) {
    M <- as.matrix(M)
  }
  if (is.numeric(M)) return(M)
  
  el_is_list <- any(vapply(M, is.list, logical(1)))
  if (!el_is_list) {
    # Not lists, attempt a vectorized numeric coercion (may produce NAs)
    coerced <- suppressWarnings(as.numeric(M))
    if (all(is.na(coerced) & !is.na(as.character(M)))) {
      stop(sprintf("Matrix '%s' cannot be coerced to numeric.", matrixname))
    }
    return(matrix(coerced, nrow = nrow(M), ncol = ncol(M)))
  }
  
  n <- length(M)
  coerced_vec <- numeric(n)
  for (i in seq_len(n)) {
    el <- M[i]
    if (is.list(el)) {
      val <- el[[1]]
      if (is.atomic(val) && length(val) == 1) {
        coerced_val <- suppressWarnings(as.numeric(val))
        coerced_vec[i] <- coerced_val
      } 
    } else {
      # not a list; coerce directly
      coerced_val <- suppressWarnings(as.numeric(el))
      coerced_vec[i] <- coerced_val
    }
  }
  
  matrix(coerced_vec, nrow = nrow(M), ncol = ncol(M))
}

top_k_from_mat_abs <- function(M_raw, k = 3, matrixname = NA_character_) {
  M <- safe_to_numeric_matrix(M_raw, matrixname = matrixname)
  
  signed_all <- as.numeric(M)
  abs_all <- abs(signed_all)
  valid <- !is.na(abs_all)
  
  lin_idx_all <- seq_along(M)
  ordered_lin <- lin_idx_all[valid][order(abs_all[valid], decreasing = TRUE)]
  k_use <- min(k, length(ordered_lin))
  idx_lin <- ordered_lin[seq_len(k_use)]
  idx_mat <- arrayInd(idx_lin, dim(M))
  
  rn <- rownames(M)
  cn <- colnames(M)
  row_names <- rn[idx_mat[,1]]
  col_names <- cn[idx_mat[,2]]
  
  signed_vals <- signed_all[idx_lin]
  abs_vals_sel <- abs(signed_vals)
  
  data.frame(
    matrix_name = rep(matrixname, k_use),
    row_index = idx_mat[,1],
    col_index = idx_mat[,2],
    row_name = row_names,
    column_name = col_names,
    absolutevalue_max = abs_vals_sel,
    signed_max = signed_vals
  )
}

top_k_table_for_list <- function(mat_list, k = 3) {
  nm <- names(mat_list)
  res_list <- lapply(nm, function(nm_i) {
    top_k_from_mat_abs(mat_list[[nm_i]], k = k, matrixname = nm_i) 
  })
  
  result <- do.call(rbind, res_list)
}


result_table <- top_k_table_for_list(mat_list, k = 3)

### Pseudo R² values ###

get_pseudo_r2 <- function(model, y) {
  mu_hat <- predict(model, model = "mu", type = "parameter")
  r2 <- cor(mu_hat, y, use = "complete.obs")^2
  return(r2)
}

# Compute pseudo-R² with simple names
r2_body    <- get_pseudo_r2(body_final,   sesame_edit$body_betatransf)
r2_letters <- get_pseudo_r2(letters_final,sesame_edit$let_betatransf)
r2_forms   <- get_pseudo_r2(form_initial, sesame_edit$form_betatransf)
r2_numbers <- get_pseudo_r2(numb_initial, sesame_edit$numb_betatransf)
r2_relat   <- get_pseudo_r2(relat_final,  sesame_edit$relat_betatransf)
r2_class   <- get_pseudo_r2(clasf_final,  sesame_edit$clasf_betatransf)

pseudo_r2_table <- data.frame(
  Outcome = c("Body", "Letters", "Forms", "Numbers", "Relational", "Classification"),
  Pseudo_R2 = c(r2_body, r2_letters, r2_forms, r2_numbers, r2_relat, r2_class)
)

### BIC plots ###
mod <- if (exists("letters_adj_betamod")) {
  letters_adj_betamod
} else if (exists("letters_final")) {
  letters_final
} else {
  stop("No letters model object found in environment.")
}

# Plot BIC vs log(lambda) for the mu component
bic_plot <- pathplot(mod, which = "criterion", model = "mu",
         main = "Figure 4: BIC vs. log(lambda) For The Letters model", xlab = "log(lambda)", ylab = "BIC")

chosen_lambda <- NULL
possible_names <- c("lambda", "lambda_mu", "lam", "opt.lambda")
for (nm in possible_names) {
  if (!is.null(mod[[nm]])) { chosen_lambda <- mod[[nm]]; break }
  if (!is.null(mod$opt) && !is.null(mod$opt[[nm]])) { chosen_lambda <- mod$opt[[nm]]; break }
}

if (!is.null(chosen_lambda) && length(chosen_lambda)>0) {
  abline(v = log(chosen_lambda[1]), lty = 2, lwd = 1)
  legend("topright", legend = "Selected λ (BIC)", lty = 2, bty = "n")
}

### Top 5 coefficients per model ###

model_list <- list(
  body    = body_final,       # adjusted
  letters = letters_final,    # adjusted
  form    = form_initial,     # INITIAL model
  numb    = numb_initial,     # INITIAL model
  relat   = relat_final,      # adjusted
  clasf   = clasf_final       # adjusted
)

get_mu_coefs <- function(mod) {
  cf <- tryCatch(coef(mod, model = "mu"), error = function(e) coef(mod))
  
  # cf can be a matrix, vector, or list$mu — handle all:
  if (is.list(cf) && "mu" %in% names(cf)) {
    mat <- cf$mu
    vec <- as.numeric(mat[,1])
    names(vec) <- rownames(mat)
  } else if (is.matrix(cf)) {
    vec <- as.numeric(cf[,1])
    names(vec) <- rownames(cf)
  } else if (is.numeric(cf) && !is.null(names(cf))) {
    vec <- cf
  } else {
    stop("Could not extract coefficients for this model.")
  }
  
  # Drop nuisance parameters
  vec <- vec[!grepl("alpha|tau|Intercept", names(vec), ignore.case = TRUE)]
  
  return(vec)
}

# extract top 3 coefficients per model
top3_list <- map(names(model_list), function(m) {
  cf <- get_mu_coefs(model_list[[m]])
  
  # pick top 3 BY ABSOLUTE VALUE
  top_terms <- names(sort(abs(cf), decreasing = TRUE))[1:3]
  
  tibble(
    model = m,
    term = top_terms,
    estimate = cf[top_terms],
    abs_est = abs(cf[top_terms])
  )
})

top3_table <- bind_rows(top3_list)

kable(top3_table, digits = 4,
      caption = "Table 4: Top 3 Most Influential Predictors in Each Final Model")

# extract top 5 coefficients per model
top5_list <- map(names(model_list), function(m) {
  cf <- get_mu_coefs(model_list[[m]])
  
  # pick top 5 BY ABSOLUTE VALUE
  top_terms <- names(sort(abs(cf), decreasing = TRUE))[1:5]
  
  tibble(
    model = m,
    term = top_terms,
    estimate = cf[top_terms],
    abs_est = abs(cf[top_terms])
  )
})

top5_table <- bind_rows(top5_list)

### Interaction investigations ###

# Site x Encouragement
my_cols3 <- c(
  "Main1"      = "#D8A7B1",
  "Main2"      = "lightskyblue3",
  "Interaction"= "thistle3"
)

summarize_site_enc_effects <- function(coef_vec) {
  nms <- names(coef_vec)
  
  site_terms <- nms[grepl("site", nms, ignore.case=TRUE) &
                      !grepl("enc|viewenc", nms, ignore.case=TRUE) &
                      (!grepl("[:~]", nms)) ]
  
  enc_terms  <- nms[grepl("encour|viewenc|enc", nms, ignore.case=TRUE) &
                      !grepl("site", nms, ignore.case=TRUE) &
                      (!grepl("[:~]", nms)) ]
  
  int_terms  <- nms[grepl("site", nms, ignore.case=TRUE) &
                      grepl("enc|viewenc", nms, ignore.case=TRUE)]
  
  get_signed <- function(terms) {
    if (length(terms)==0) return(0)
    coef_vec[terms][which.max(abs(coef_vec[terms]))]   # signed
  }
  
  tibble(
    effect = c("Main1","Main2","Interaction"),
    estimate = c(
      get_signed(site_terms),
      get_signed(enc_terms),
      get_signed(int_terms)
    ),
    magnitude = abs(estimate)
  )
  
}

df_siteenc3 <- imap_dfr(models_to_use, \(mod,name)
                        summarize_site_enc_effects(get_mu_coefs(mod)) |> mutate(model=name))

df_siteenc3$effect <- factor(df_siteenc3$effect,
                             levels = c("Main1","Main2","Interaction"))


df_siteenc3$model <- factor(df_siteenc3$model,
                            levels=c("Body","Letters","Form","Numbers","Relat","Classif"))

p_siteenc3 <- ggplot(df_siteenc3,
                     aes(x=model, y=magnitude, fill=effect)) +
  geom_col(position=position_dodge(width=.7), width=.6, color="grey40") +
  geom_text(aes(label = sprintf("%.3f", estimate)),
            position = position_dodge(width = .7),
            vjust = -0.6, size = 1.8) +
  scale_fill_manual(
    values = my_cols3,
    breaks = c("Main1","Main2","Interaction"),
    labels = c("Site","Encouragement","Interaction"),
    name = "Effect"
  ) +
  labs(title="Figure 5: Strength of Site × Encouragement Effects Across Models",
       x="Model", y="Coefficient magnitude (logit-scale)") +
  theme_minimal(base_size=13) +
  theme(legend.position="top")

# Sex x Viewcat 
summarize_sex_view_effects <- function(coef_vec) {
  nms <- names(coef_vec)
  
  sex_terms  <- nms[ (grepl("^sex|\\bsex\\b", nms, ignore.case=TRUE)) &
                       (!grepl("viewcat", nms, ignore.case=TRUE)) &
                       (!grepl("[:~]", nms)) ]
  
  view_terms <- nms[ (grepl("viewcat", nms, ignore.case=TRUE)) &
                       (!grepl("\\bsex\\b|^sex", nms, ignore.case=TRUE)) &
                       (!grepl("[:~]", nms)) ]
  
  int_terms  <- nms[ grepl("sex", nms, ignore.case=TRUE) &
                       grepl("viewcat", nms, ignore.case=TRUE) ]
  
  get_signed <- function(terms) {
    if (length(terms)==0) return(0)
    coef_vec[terms][which.max(abs(coef_vec[terms]))]   # signed
  }
  
  tibble(
    effect = c("Main1","Main2","Interaction"),
    estimate = c(
      get_signed(sex_terms),
      get_signed(view_terms),
      get_signed(int_terms)
    ),
    magnitude = abs(estimate)
  )
  
}


df_sexview3 <- imap_dfr(models_to_use, \(mod,name)
                        summarize_sex_view_effects(get_mu_coefs(mod)) |> mutate(model=name))

df_sexview3$effect <- factor(df_sexview3$effect,
                             levels = c("Main1","Main2","Interaction"))

df_sexview3$model <- factor(df_sexview3$model,
                            levels=c("Body","Letters","Form","Numbers","Relat","Classif"))

p_sexview3 <- ggplot(df_sexview3,
                     aes(x=model, y=magnitude, fill=effect)) +
  geom_col(position=position_dodge(width=.7), width=.6, color="grey40") +
  geom_text(aes(label = sprintf("%.3f", estimate)),
            position = position_dodge(width = .7),
            vjust = -0.6, size = 1.8) +
  scale_fill_manual(values=my_cols3,
                    breaks = c("Main1","Main2","Interaction"),
                    labels=c("Sex","View Frequency","Interaction")) +
  labs(title="Figure 6: Strength of Sex × Viewcat Effects Across Models",
       x="Model", y="Coefficient magnitude (logit-scale)") +
  theme_minimal(base_size=13) +
  theme(legend.position="top")

# Setting x Vocab 
summarize_setting_peabody_effects <- function(coef_vec) {
  nms <- names(coef_vec)
  
  # MAIN setting terms: contain "setting" but NOT "peabody" and NOT interaction markers (':' or '~')
  set_terms <- nms[
    grepl("setting", nms, ignore.case = TRUE) &
      !grepl("peabody", nms, ignore.case = TRUE) &
      !grepl("[:~]", nms)
  ]
  
  # PEABODY terms: contain "peabody" but NOT "setting" and NOT interaction markers
  peabody_terms <- nms[
    grepl("peabody", nms, ignore.case = TRUE) &
      !grepl("setting", nms, ignore.case = TRUE) &
      !grepl("[:~]", nms)
  ]
  
  # INTERACTIONS: contain both 'setting' and 'peabody'
  int_terms <- nms[
    grepl("setting", nms, ignore.case = TRUE) &
      grepl("peabody", nms, ignore.case = TRUE)
  ]
  
  get_signed <- function(terms) {
    if (length(terms) == 0) return(0)
    coef_vec[terms][which.max(abs(coef_vec[terms]))]   # signed
  }
  
  tibble(
    effect = c("Main1","Main2","Interaction"),
    estimate = c(
      get_signed(set_terms),
      get_signed(peabody_terms),
      get_signed(int_terms)
    ),
    magnitude = abs(estimate)
  )
  
}


df_setpeabody3 <- imap_dfr(models_to_use, \(mod,name)
                           summarize_setting_peabody_effects(get_mu_coefs(mod)) |> mutate(model=name))

df_setpeabody3$effect <- factor(df_setpeabody3$effect,
                                levels = c("Main1","Main2","Interaction"))

df_setpeabody3$model <- factor(df_setpeabody3$model,
                               levels=c("Body","Letters","Form","Numbers","Relat","Classif"))

p_setpeabody3 <- ggplot(df_setpeabody3,
                        aes(x=model, y=magnitude, fill=effect)) +
  geom_col(position=position_dodge(width=.7), width=.6, color="grey40") +
  geom_text(aes(label = sprintf("%.3f", estimate)),
            position = position_dodge(width = .7),
            vjust = -0.6, size = 1.8) +
  scale_fill_manual(values=my_cols3,
                    breaks = c("Main1","Main2","Interaction"),
                    labels=c("Setting","Peabody","Interaction")) +
  labs(title="Figure 7: Strength of Setting × Vocabulary Effects Across Models",
       x="Model", y="Coefficient magnitude (logit-scale)") +
  theme_minimal(base_size=13) +
  theme(legend.position="top")

### RMSEs ###

get_rmse <- function(model, y) {
  preds <- predict(model, model = "mu", type = "parameter")
  sqrt(mean((preds - y)^2, na.rm = TRUE))
}

rmse_body    <- get_rmse(body_final,   sesame_edit$body_betatransf)
rmse_letters <- get_rmse(letters_final,sesame_edit$let_betatransf)
rmse_forms   <- get_rmse(form_initial,  sesame_edit$form_betatransf)    
rmse_numbers <- get_rmse(numb_initial,  sesame_edit$numb_betatransf)    
rmse_relat   <- get_rmse(relat_final,  sesame_edit$relat_betatransf)
rmse_class   <- get_rmse(clasf_final,  sesame_edit$clasf_betatransf)

rmse_table <- data.frame(
  Model = c("Body",
            "Letters",
            "Forms",
            "Numbers",
            "Relational",
            "Classification"),
  RMSE = c(rmse_body, rmse_letters, rmse_forms, rmse_numbers, rmse_relat, rmse_class)
)

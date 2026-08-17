library()
source('scripts/data_cleaning.R')

# Transformed response variables 
resp <- c("body_betatransf","let_betatransf","form_betatransf",
          "numb_betatransf","relat_betatransf","clasf_betatransf")

# Starter formula for body (predictors) extraction to create all formulas
# Start with all main effects and pairwise interaction terms, then perform selection afterwards
form_body <- as.formula(
  body_betatransf ~ 
    la(site, type = 'single') +
    la(sex, type = 'single') + 
    la(age, type = 'single') + 
    la(viewcat, type = 'single') + 
    la(setting, type = 'single') + 
    la(viewenc, type = 'single') + 
    la(peabody_log, type = 'single') + 
    la(site : sex, type = 'single') + 
    la(site : age, type = 'sing le') + 
    la(site : viewcat, type = 'single') + 
    la(site : viewenc, type = 'single') + 
    la(site : peabody_log, type = 'single') + 
    la(sex : age, type = 'single') + 
    la(sex : viewcat, type = 'single') + 
    la(sex : setting, type = 'single') + 
    la(sex : viewenc, type = 'single') + 
    la(sex : peabody_log, type = 'single') + 
    la(age : viewcat, type = 'single') + 
    la(age : setting, type = 'single') + 
    la(age : viewenc, type = 'single') + + 
    la(viewcat : setting, type = 'single') +
    la(viewcat : viewenc, type = 'single') +
    la(viewcat : peabody_log, type = 'single') +
    la(setting : viewenc, type = 'single') +
    la(setting : peabody_log, type = 'single') + 
    la(viewenc : peabody_log, type = 'single'))

rhs_expr <- f_rhs(form_body)
formula_list <- list()

for (response_name in resp) {
  # Take response_name string and make object for injection
  lhs_sym <- sym(response_name)
  
  new_formula <- new_formula(
    lhs = lhs_sym, 
    rhs = rhs_expr
  )
  formula_list[[response_name]] <- new_formula
}

### Running initial models, later trimmed for complexity ###

model_list <- list()

for (curr_formula in formula_list) {
  print(curr_formula)
  currmod <- bamlss(
    curr_formula, 
    family = "beta", 
    data = sesame_edit, 
    optimizer = opt_lasso,
    criterion = "BIC", 
    nlambda = 100, 
    multiple = FALSE)
  
  currresponse <- f_lhs(curr_formula)
  model_list[[currresponse]] <- currmod
}

saveRDS(model_list[[1]], file = "models/initial/body_betamod.rds")
saveRDS(model_list[[2]], file = "models/initial/letters_betamod.rds")
saveRDS(model_list[[3]], file = "models/initial/form_betamod.rds")
saveRDS(model_list[[4]], file = "models/initial/numb_betamod.rds")
saveRDS(model_list[[5]], file = "models/initial/relat_betamod.rds")
saveRDS(model_list[[6]], file = "models/initial/clasf_betamod.rds")

### Model Trimming ### 
# Methodology for model trimming in report  

base_mod <- as.formula(body_betatransf ~ 
                         # main effects 
                         la(site, type = 'single') + 
                         la(sex, type = 'single') + 
                         la(age, type = 'single') + 
                         la(viewcat, type = 'single') + 
                         la(setting, type = 'single') + 
                         la(viewenc, type = 'single') + 
                         la(peabody_log, type = 'single') + 
                         # univerally significant interactions
                         la(site : peabody_log, type = 'single') + 
                         la(sex : setting, type = 'single') + 
                         la(sex : viewenc, type = 'single') + 
                         la(age : viewcat, type = 'single') + 
                         la(age : setting, type = 'single') + 
                         la(age : peabody_log, type = 'single') + 
                         la(setting : peabody_log, type = 'single') + 
                         la(viewenc : peabody_log, type = 'single')
)

body_mod <- update(base_mod, 
                   . ~ . + 
                     la(age : viewenc, type = 'single') +
                     la(viewcat : viewenc, type = 'single') +
                     la(sex : viewcat, type = 'single') +
                     la(viewcat : setting, type = 'single') +
                     la(setting : viewenc, type = 'single') +
                     la(sex : peabody_log, type = 'single')
)

let_mod <- update(base_mod, 
                  let_betatransf ~ . + 
                    la(site : age, type = 'single') +
                    la(sex : age, type = 'single') +
                    la(viewcat : peabody_log, type = 'single') +
                    la(sex : viewcat, type = 'single')  +
                    la(viewcat : setting, type = 'single') +
                    la(setting : viewenc, type = 'single') +
                    la(sex : peabody_log, type = 'single')
)

form_mod <- update(base_mod, 
                   form_betatransf ~ . + 
                     la(site : age, type = 'single') +
                     la(sex : age, type = 'single') +
                     la(viewcat : peabody_log, type = 'single') +
                     la(age : viewenc, type = 'single')  +
                     la(viewcat : viewenc, type = 'single') +
                     la(viewcat : setting, type = 'single') +
                     la(setting : viewenc, type = 'single') +
                     la(sex : peabody_log, type = 'single')
)

numb_mod <- update(base_mod, 
                   numb_betatransf ~ . + 
                     la(site : age, type = 'single') +
                     la(sex : age, type = 'single') +
                     la(viewcat : peabody_log, type = 'single') +
                     la(age : viewenc, type = 'single')  +
                     la(viewcat : viewenc, type = 'single') +
                     la(sex : viewcat, type = 'single')  +
                     la(setting : viewenc, type = 'single') +
                     la(sex : peabody_log, type = 'single')
)

relat_mod <- update(base_mod, 
                    relat_betatransf ~ . + 
                      la(site : age, type = 'single') + 
                      la(viewcat : peabody_log, type = 'single') +
                      la(viewcat : viewenc, type = 'single') +
                      la(sex : viewcat, type = 'single')  +
                      la(viewcat : setting, type = 'single') +
                      la(sex : peabody_log, type = 'single')
)

clasf_mod <- update(base_mod, 
                    clasf_betatransf ~ . + 
                      la(site : age, type = 'single') +
                      la(sex : age, type = 'single') +
                      la(viewcat : peabody_log, type = 'single') +
                      la(age : viewenc, type = 'single')  +
                      la(viewcat : viewenc, type = 'single') +
                      la(sex : viewcat, type = 'single')  +
                      la(setting : viewenc, type = 'single')
)

mods <- list(body_mod, let_mod, form_mod, numb_mod, relat_mod, clasf_mod)

adj_models <- list()

for (mod in mods) {
  curr_adjmod <- bamlss(
    mod, 
    family = "beta", 
    data = sesame_edit, 
    optimizer = opt_lasso,
    criterion = "BIC", 
    nlambda = 100, 
    multiple = FALSE
  )
  
  adj_models[[f_lhs(mod)]] <- curr_adjmod
}

saveRDS(adj_models[[1]], file = "models/adjusted/body_adj_betamod.rds")
saveRDS(adj_models[[2]], file = "models/adjusted/letters_adj_betamod.rds")
saveRDS(adj_models[[3]], file = "models/adjusted/form_adj_betamod.rds")
saveRDS(adj_models[[4]], file = "models/adjusted/numb_adj_betamod.rds")
saveRDS(adj_models[[5]], file = "models/adjusted/relat_adj_betamod.rds")
saveRDS(adj_models[[6]], file = "models/adjusted/clasf_adj_betamod.rds")

### Compare initial to adjusted models, select based on RMSE ###

# Initial RMSEs
rmse_body_initial <- mean((predict(model_list[[1]], model = "mu", type = "parameter") - sesame_edit$body_betatransf)^2)^(1/2)
rmse_letters_initial <- mean((predict(model_list[[2]], model = "mu", type = "parameter") - sesame_edit$let_betatransf)^2)^(1/2)
rmse_form_initial <- mean((predict(model_list[[3]], model = "mu", type = "parameter") - sesame_edit$form_betatransf)^2)^(1/2)
rmse_numb_initial <- mean((predict(model_list[[4]], model = "mu", type = "parameter") - sesame_edit$numb_betatransf)^2)^(1/2)
rmse_relat_initial <- mean((predict(model_list[[5]], model = "mu", type = "parameter") - sesame_edit$relat_betatransf)^2)^(1/2)
rmse_clasf_initial <- mean((predict(model_list[[6]], model = "mu", type = "parameter") - sesame_edit$clasf_betatransf)^2)^(1/2)

# Adjusted RMSEs
rmse_body_adj <- mean((predict(adj_models[[1]], model = "mu", type = "parameter") - sesame_edit$body_betatransf)^2)^(1/2)
rmse_letters_adj <- mean((predict(adj_models[[2]], model = "mu", type = "parameter") - sesame_edit$let_betatransf)^2)^(1/2)
rmse_form_adj <- mean((predict(adj_models[[3]], model = "mu", type = "parameter") - sesame_edit$form_betatransf)^2)^(1/2)
rmse_numb_adj <- mean((predict(adj_models[[4]], model = "mu", type = "parameter") - sesame_edit$numb_betatransf)^2)^(1/2)
rmse_relat_adj <- mean((predict(adj_models[[5]], model = "mu", type = "parameter") - sesame_edit$relat_betatransf)^2)^(1/2)
rmse_clasf_adj <- mean((predict(adj_models[[6]], model = "mu", type = "parameter") - sesame_edit$clasf_betatransf)^2)^(1/2)

# Selection of final models 
saveRDS(adj_models[[1]], file = "models/final/body_fin_betamod.rds")
saveRDS(adj_models[[2]], file = "models/final/letters_fin_betamod.rds")
saveRDS(model_list[[3]], file = "models/final/form_fin_betamod.rds")
saveRDS(model_list[[4]], file = "models/final/numb_fin_betamod.rds")
saveRDS(adj_models[[5]], file = "models/final/relat_fin_betamod.rds")
saveRDS(adj_models[[6]], file = "models/final/clasf_fin_betamod.rds")

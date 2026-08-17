library(here)
source('scripts/load_packages')
sesame <- read.csv("data/sesame.csv")

# Standardizing test scores
max_scores <- c(
  body  = 32,
  let   = 58,
  form  = 20,
  numb  = 54,
  relat = 17, 
  clasf = 24
)

# Convert scores to percent
sesame_transf <- sesame |>
  mutate(
    prebody_p  = 100 * prebody  / max_scores["body"],
    postbody_p = 100 * postbody / max_scores["body"],
    
    prelet_p   = 100 * prelet   / max_scores["let"],
    postlet_p  = 100 * postlet  / max_scores["let"],
    
    preform_p  = 100 * preform  / max_scores["form"],
    postform_p = 100 * postform / max_scores["form"],
    
    prenumb_p  = 100 * prenumb  / max_scores["numb"],
    postnumb_p = 100 * postnumb / max_scores["numb"],
    
    prerelat_p  = 100 * prerelat  / max_scores["relat"],
    postrelat_p = 100 * postrelat / max_scores["relat"],
    
    preclasf_p  = 100 * preclasf  / max_scores["clasf"],
    postclasf_p = 100 * postclasf / max_scores["clasf"]
  )

# Create variables for percent improvement for each test
sesame_transf <- sesame_transf |>
  mutate(
    improve_body  = postbody_p  - prebody_p,
    improve_let   = postlet_p   - prelet_p,
    improve_form  = postform_p  - preform_p,
    improve_numb  = postnumb_p  - prenumb_p,
    improve_relat = postrelat_p - prerelat_p,
    improve_clasf = postclasf_p - preclasf_p
  )

# Transformations
sesame_transf <- sesame_transf |> 
  mutate(
    # For each response, rescale (add 1 and divide by 2) to model with logit
    body_betatransf = (improve_body/100 + 1) / 2, 
    let_betatransf = (improve_let/100 + 1) / 2, 
    form_betatransf = (improve_form/100 + 1) / 2, 
    numb_betatransf = (improve_numb/100 + 1) / 2,
    relat_betatransf = (improve_relat/100 + 1) / 2, 
    clasf_betatransf = (improve_clasf/100 + 1) / 2,
    # Alt: for each response, rescale to approximate normal dist for tobit model
    # Independent variable transformations
    peabody_log = log(peabody),
    viewcat = factor(
      case_when(viewcat == 1 ~ 'rare', 
                viewcat == 2 ~ 'once_twice', 
                viewcat == 3 ~ 'three_five', 
                viewcat == 4 ~ 'over_five'), 
      levels = c('rare','once_twice','three_five', 'over_five')),
    site = factor(
      case_when(site == 1 ~ 'disadv_city', 
                site == 2 ~ 'adv_suburb', 
                site == 3 ~ 'adv_rural', 
                site == 4 ~ 'disadv_rural', 
                site == 5 ~ 'disadv_span'), 
      levels = c('disadv_span','disadv_city','disadv_rural','adv_rural','adv_suburb')), 
    sex = if_else(sex == 1, 'male', 'female'), 
    setting = factor(if_else(setting == 1, 'home', 'school'), levels = c('home', 'school')), 
    viewenc = if_else(viewenc == 1, 'encouraged', 'not_encouraged')
  )

write.csv(sesame_edit, 'data/sesame_edit.csv')
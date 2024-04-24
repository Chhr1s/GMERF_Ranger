library(tidyverse)
library(ranger)

gmerf_ranger <- 
  function(y, data, gr, fam, b0=NULL, toll=1e-4, itmax=100, ...) {
    
    
    
    original_order <- unique(pull(data, !!sym(gr)))
    Z <- matrix(rep(1, nrow(distinct(data, !!sym(gr)))))
    rownames(Z) <- original_order
    
    if (is.null(b0)) {
      b <- rep(0, length = length(Z))
    } else {
      b <- b0
    }
    
    names(b) <- original_order
    
    all.b <- list()
    all.b[[1]] <- b
    variables_ <- names(data)
    
    formula_ <- 
      formula(
        paste0(y, ' ~ ', 
               paste0(variables_[which(!variables_ %in% c(gr, y, 'student_id'))], collapse = ' + ')
        )
      )
    
    glm_fit <- glm(formula = formula_, data = data %>% select(-contains('_id')), family=fam)
    
    eta_df <- 
      data %>% 
      mutate(
        !!sym(gr) := as.character(!!sym(gr)),
        eta = predict(glm_fit, data = ., type="link")
      )
    
    eta <- eta_df %>% pull(eta) 
    
    it <- 1
    conv <- FALSE
    
    while (it < itmax && !conv) {
      
      targ_step1 <- Z * b
      
      rf_df <- 
        data.frame(targ_tmp = targ_step1[,1]) %>% 
        rownames_to_column(gr) %>% 
        inner_join(eta_df, by = gr) %>% 
        mutate(targ = eta - targ_tmp) 
      
      
      rf_fit <- 
        ranger(
          y = rf_df$targ, 
          x = select(rf_df, -OTG, -targ, -eta, -targ_tmp, -contains('_id')),
          importance = "impurity", 
          seed = 123, 
          ...
        )
      
      rf_df <- 
        rf_df %>% 
        mutate(
          fx = predict(rf_fit, data=.)$predictions, 
          outcome_tmp = eta - fx
        )
      
      
      
      formula_2_ <- 
        formula(
          paste0('outcome_tmp ~ 0 + (1 | ', gr, ')')
        )
      
      
      glmm_fit <- 
        lmer(formula_2_, data = rf_df)
      
      b <- matrix(ranef(glmm_fit)[[gr]][['(Intercept)']])
      
      all.b[[it+1]] <- b
      # this is only going to work in the random intercept space .. I think ?
      M <- max(abs(b - all.b[[it]]))
      i <- which.max(abs(b - all.b[[it]]))
      tr <- M / all.b[[it]][i]
      
      if (tr < toll) {
        conv <- TRUE
      } else {
        conv <- FALSE
      }
      
      it <- it + 1
    }
    
    if (!conv) {
      warning("The algorithm did not converge.")
    }
    
    result <- list(
      final_glmm = glmm_fit,
      final_rf = rf_fit,
      b = b,
      #final_targ = select(rd_df, school_id, targ,),
      it = it
    )
    
    return(result)
  }



predict_gmerf <- function(object, newdata, gr = 'school_id', link_fn = NULL) {
  # Extract the final GLMM and RF models from the gmerf_result object
  final_glmm <- object$final_glmm
  final_rf <- object$final_rf
  
  
  # Predict the fixed effects with random forest component
  
  
  logit_preds <- 
    newdata %>% 
    mutate(
      !!sym(gr) := factor(school_id),
      rf_preds = predict(final_rf, data=.)$predictions, 
    ) %>% 
    inner_join(
      data.frame(ranef(final_glmm))%>%
        select(!!sym(gr) := grp, condval), 
      by = gr
    ) %>% 
    mutate(
      logit_pred = condval + rf_preds
    ) %>% 
    pull(logit_pred)
  
  # transform
  if (is.null(link_fn)){
    return(logit_preds)
  }
  
  if (link_fn == 'binomial'){
    return(plogis(logit_preds))
  }
}
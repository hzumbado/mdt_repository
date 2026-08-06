# function ----------------------------------------------------------------

tss_metrics <- function(vars) {
  
  calc_threshold_table <- function(pres, bg) {
    
    df <- tibble::tibble(
      obs  = c(rep(1, length(pres)), rep(0, length(bg))),
      pred = c(pres, bg)
    )
    
    thrs <- sort(unique(df$pred))
    
    purrr::map_dfr(thrs, function(th) {
      
      pred_bin <- ifelse(df$pred >= th, 1, 0)
      
      tp <- sum(pred_bin == 1 & df$obs == 1)
      fn <- sum(pred_bin == 0 & df$obs == 1)
      tn <- sum(pred_bin == 0 & df$obs == 0)
      fp <- sum(pred_bin == 1 & df$obs == 0)
      
      sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
      spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
      tss  <- sens + spec - 1
      
      tibble::tibble(
        threshold   = th,
        sensitivity = sens,
        specificity = spec,
        tss         = tss
      )
      
    }) |>
      dplyr::filter(!is.na(tss))
  }
  
  eval_at_threshold <- function(pres, bg, th) {
    
    obs  <- c(rep(1, length(pres)), rep(0, length(bg)))
    pred <- c(pres, bg)
    
    pred_bin <- ifelse(pred >= th, 1, 0)
    
    tp <- sum(pred_bin == 1 & obs == 1)
    fn <- sum(pred_bin == 0 & obs == 1)
    tn <- sum(pred_bin == 0 & obs == 0)
    fp <- sum(pred_bin == 1 & obs == 0)
    
    sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
    tss  <- sens + spec - 1
    
    tibble::tibble(
      sensitivity = sens,
      specificity = spec,
      tss         = tss
    )
  }
  
  # validation max TSS ----------------------------------------------------
  
  val_tbl <- calc_threshold_table(
    vars$occs.val.pred,
    vars$bg.val.pred
  )
  
  best_val <- val_tbl |>
    dplyr::slice_max(
      order_by = tss,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::rename(
      thr_max_tss_val  = threshold,
      sens_max_tss_val = sensitivity,
      spec_max_tss_val = specificity,
      tss_max_val      = tss
    )
  
  # MTSS from training ----------------------------------------------------
  
  train_tbl <- calc_threshold_table(
    vars$occs.train.pred,
    vars$bg.train.pred
  )
  
  mtss_train <- train_tbl |>
    dplyr::slice_max(
      order_by = tss,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::rename(
      mtss_thr_train = threshold
    )
  
  mtss_val <- eval_at_threshold(
    vars$occs.val.pred,
    vars$bg.val.pred,
    mtss_train$mtss_thr_train
  ) |>
    dplyr::rename(
      sens_mtss_val = sensitivity,
      spec_mtss_val = specificity,
      tss_mtss_val  = tss
    )
  
  # 10-percentile training presence --------------------------------------
  
  thr_10ppt <- stats::quantile(
    vars$occs.train.pred,
    probs = 0.10,
    na.rm = TRUE,
    names = FALSE
  )
  
  tenppt_val <- eval_at_threshold(
    vars$occs.val.pred,
    vars$bg.val.pred,
    thr_10ppt
  ) |>
    dplyr::rename(
      sens_10ppt_val = sensitivity,
      spec_10ppt_val = specificity,
      tss_10ppt_val  = tss
    )
  
  dplyr::bind_cols(
    best_val,
    mtss_train,
    mtss_val,
    tibble::tibble(thr_10ppt_train = thr_10ppt),
    tenppt_val
  )
}
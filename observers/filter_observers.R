#### The creation of all filter objects follows roughly the same pattern:
# 1) Clear any warning message for that filter
# 2) Attempt to store the filter object in a reactive value
# 3) If failed, display a warning message and return the old value of the filter (NULL if no filter was created yet)
# 4) Do (1-3) for the second object if present

#'@details Applies filters inplace to omicsData objects. 
apply_filt <- function(){
  # stats results are no longer valid, remove them
  objects$imdanova_res <- NULL
  
  tryCatch(
    {
      # make temp objects and clear summaries
      tmp <- objects$omicsData
      tmp2 <- objects$omicsData_2
      revals$filter_summary <- revals$filter_summary_2 <- NULL
      # molecule filter
      if (!is.null(objects$filters$molfilt)) {
        tmp <- applyFilt(objects$filters$molfilt, tmp, min_num = input$mol_min_num)
        if (!is.null(objects$filters$molfilt_2)) {
          tmp2 <- applyFilt(objects$filters$molfilt_2, tmp2, min_num = input$mol_min_num)
        }
      }
      
      # total count filter
      if (!is.null(objects$filters$tcfilt)) {
        tmp <- applyFilt(objects$filters$tcfilt, tmp, min_count = input$min_num_trans)
      }
      
      # proteomics filter
      if (inherits(tmp, "pepData")) {
        if (isTRUE(!is.null(objects$filters$profilt))) {
          tmp <- applyFilt(objects$filters$profilt, tmp, min_num_peps = input$min_num_peps, redundancy = input$degen_peps)
        }
      }
      # cv filter
      if (!is.null(objects$filters$cvfilt)) {
        tmp <- applyFilt(objects$filters$cvfilt, tmp, cv_threshold = input$cv_threshold)
        if (!is.null(objects$filters$cvfilt_2) & !is.null(tmp2)) {
          tmp2 <- applyFilt(objects$filters$cvfilt_2, tmp2, cv_threshold = input$cv_threshold)
        }
      }

      # if(e$message == "None of the samples will be removed with the current thresholds.")
      # imd filter
      if (!is.null(objects$filters$imdanovafilt)) {
        tmp <- applyFilt(objects$filters$imdanovafilt, 
                         tmp, 
                         min_nonmiss_anova = input$min_nonmiss_anova, 
                         min_nonmiss_gtest = input$min_nonmiss_gtest
                         )
        if (!is.null(objects$filters$imdanovafilt_2) & 
            !is.null(tmp2)) {
          tmp2 <- applyFilt(objects$filters$imdanovafilt_2, 
                            tmp2, 
                            min_nonmiss_anova = input$min_nonmiss_anova, 
                            min_nonmiss_gtest = input$min_nonmiss_gtest)
        }
      }
      
      #### SAMPLE/CUSTOM FILTERS ####
      
      ## RNA filter - 1 dataset only
      # library size
      if (!is.null(objects$filters$rnafilt_libsize)) {
        tmp <- applyFilt(objects$filters$rnafilt_libsize, tmp, size_library = input$rnafilt_min_lib_size)
      }
      # min-nonzero
      if (!is.null(objects$filters$rnafilt_min_nonzero)) {
        tmp <- applyFilt(objects$filters$rnafilt_min_nonzero, tmp, min_nonzero = input$rnafilt_min_nonzero)
      }
      
      # rMd filter
      if (!is.null(objects$filters$rmdfilt)) {
        tmp <- applyFilt(objects$filters$rmdfilt, tmp, pvalue_threshold = input$pvalue_threshold)
        if (!is.null(objects$filters$rmdfilt_2) & !is.null(tmp2)) {
          tmp2 <- applyFilt(objects$filters$rmdfilt_2, tmp2, pvalue_threshold = input$pvalue_threshold)
        }
      }
      
      # custom filter
      if (!is.null(objects$filters$customfilt)) {
        # get the intersect in case these samples/biomolecules were removed by previous sample filters e.g. rMd/molecule
        f_data_remove <-
          intersect(objects$filters$customfilt$f_data_remove, tmp$f_data[, get_fdata_cname(tmp)]) 
        if(length(f_data_remove) == 0) f_data_remove <- NULL
        
        e_data_remove <- intersect(
          objects$filters$customfilt$e_data_remove,
          tmp$e_data[, get_edata_cname(tmp)]
        ) 
        if(length(e_data_remove) == 0) e_data_remove <- NULL
        
        e_meta_remove <- intersect(
          objects$filters$customfilt$e_meta_remove,
          tmp$e_meta[, get_emeta_cname(tmp)]
        )
        if(length(e_meta_remove) == 0) e_meta_remove <- NULL
        
        cond <- any(sapply(list(f_data_remove, e_data_remove, e_meta_remove), length) > 0)
        
        if (cond)
          tmp <-
          applyFilt(
            custom_filter(
              tmp,
              f_data_remove = f_data_remove,
              e_data_remove = e_data_remove,
              e_meta_remove = e_meta_remove
            ),
            tmp
          )
      } # custom filter second object ...
      if (!is.null(objects$filters$customfilt_2) & !is.null(tmp2)) {
        f_data_remove <-
          intersect(objects$filters$customfilt_2$f_data_remove, tmp$f_data[, get_fdata_cname(tmp2)]) 
        if(length(f_data_remove) == 0) f_data_remove <- NULL
        
        e_data_remove <- intersect(
          objects$filters$customfilt_2$e_data_remove,
          tmp2$e_data[, get_edata_cname(tmp2)]
        ) 
        if(length(e_data_remove) == 0) e_data_remove <- NULL
        
        e_meta_remove <- intersect(
          objects$filters$customfilt_2$e_meta_remove,
          tmp2$e_meta[, get_emeta_cname(tmp2)]
        )
        if(length(e_meta_remove) == 0) e_meta_remove <- NULL
        
        cond <- any(sapply(list(f_data_remove, e_data_remove, e_meta_remove), length) > 0)
        
        if (cond)
          tmp2 <-
          applyFilt(
            custom_filter(
              tmp2,
              f_data_remove = f_data_remove,
              e_data_remove = e_data_remove,
              e_meta_remove = e_meta_remove
            ),
            tmp2
          )
      }

      # __SHINYTEST__
      if(isTRUE(getOption("shiny.testmode"))) {
        .tmp = tmp
        .tmp2 = tmp2
        .omicsData.prefilt = objects$omicsData
        .omicsData.prefilt.2 = objects$omicsData_2
        
        exportTestValues(
          filters = objects$filters,
          omicsData_filt = .tmp,
          omicsData_filt_2 = .tmp2,
          omicsData = .omicsData.prefilt,
          omicsData_2 = .omicsData.prefilt.2
        )
      }
      
      # align samples for both datasets - TODO:  Check that samples are the same
      if (two_lipids() || two_metab()) {
        samps_remaining_1 <- tmp$f_data[,get_fdata_cname(objects$omicsData)]
        samps_remaining_2 <- tmp2$f_data[,get_fdata_cname(objects$omicsData_2)]
        
        fdata_filt <- f_data() %>% 
          filter(
            across(get_fdata_cname(objects$omicsData), ~.x %in% samps_remaining_1),
            across(get_fdata_cname(objects$omicsData_2), ~ .x %in% samps_remaining_2)
          )
        
        samps_remaining_ix_1 <- fdata_filt[,get_fdata_cname(objects$omicsData)]
        samps_remaining_ix_2 <- fdata_filt[,get_fdata_cname(objects$omicsData_2)]
        
        if (!dplyr::setequal(samps_remaining_ix_1, samps_remaining_1)) {
          tmp <- applyFilt(
            custom_filter(tmp, f_data_keep = samps_remaining_ix_1),
            tmp
          )  
        }
        
        if(!dplyr::setequal(samps_remaining_ix_2, samps_remaining_2)) {
          tmp2 <- applyFilt(
            custom_filter(tmp2, f_data_keep = samps_remaining_ix_2),
            tmp2
          )  
        }
        
      }
      
      # catch overfiltering and bugs
      if(prod(dim(tmp$e_data)) == 0) {
        stop("All observations were filtered from your omicsData object, this may be due to aggressive settings, or trying to filter the same sample twice.")  
      }
      if(!is.null(tmp2)) {
        if(prod(dim(tmp2$e_data)) == 0) {
          stop("All observations were filtered from your second omicsData object, this may be due to aggressive settings, or trying to filter the same sample twice.")  
        }
      }
      
      # store saved objects and remove temp objects, tmp2 will be null if there is only one dataset
      objects$omicsData <- tmp
      objects$omicsData_2 <- tmp2
      
      revals$filter_summary <- summary(tmp)
      if (!is.null(tmp2)) revals$filter_summary_2 <- summary(tmp2)
      rm(tmp, tmp2)
      res <- NULL
    },
    error = function(e) {
      res <<- paste0("Something went wrong applying your filters:  <br> System error:  ", e)
    }
  )
}

##### Unselect all Filters
observeEvent(input$reset_filters, {
  objects$filters <- NULL
})

#'@details Toggle icons/switches.  Specifically, show the checkmark the suggests
#'that a filter has been added, and disable the inputs for that particular 
#'filter so that they cannot change it before they attempt to apply all the
#'filters.
observe({
  # check if molfilt was created
  input$reset_filters
  molfilt_exists <- !is.null(objects$filters$molfilt) & (!is.null(objects$filters$molfilt_2) | is.null(objects$omicsData_2))
  
  # toggle('remove_molfilt', condition = !is.null(objects$filters$molfilt), anim = TRUE)
  toggle("molfilt_exists", condition = molfilt_exists, anim = TRUE)
  toggleState("mol_min_num", condition = !molfilt_exists)
  toggleCssClass("mol_min_num", "grey_text", condition = molfilt_exists)
  
  ## CV Filter
  cvfilt_exists <- !is.null(objects$filters$cvfilt) & (!is.null(objects$filters$cvfilt_2) | is.null(objects$omicsData_2))

  toggle("cvfilt_exists", condition = !is.null(objects$filters$cvfilt) & (!is.null(objects$filters$cvfilt_2) | is.null(objects$omicsData_2)), anim = TRUE)
  toggleState("cv_threshold", condition = !cvfilt_exists)
  toggleCssClass("cv_threshold", "grey_text", condition = cvfilt_exists)
  

  ## Proteomics Filter
  toggle("profilt_exists", condition = !is.null(objects$filters$profilt), anim = TRUE)
  toggleState("min_num_peps", condition = is.null(objects$filters$profilt))
  toggleCssClass("min_num_peps", "grey_text", condition = !is.null(objects$filters$profilt))
  toggleState("degen_peps", condition = is.null(objects$filters$profilt))
  toggleCssClass("degen_peps", condition = !is.null(objects$filters$profilt))
  
  ## imd-ANOVA Filter
  imdanovafilt_exists <- !is.null(objects$filters$imdanovafilt) & 
    (!is.null(objects$filters$imdanovafilt_2) | is.null(objects$omicsData_2))

  toggle("imdanovafilt_exists", condition = imdanovafilt_exists, anim = TRUE)
  toggleState("min_nonmiss_anova", condition = !imdanovafilt_exists)
  toggleCssClass("min_nonmiss_anova", "grey_text", condition = imdanovafilt_exists)
  toggleState("min_nonmiss_gtest", condition = !imdanovafilt_exists)
  toggleCssClass("min_nonmiss_gtest", "grey_text", condition = imdanovafilt_exists)
  
  ## Total count filter ...
  tcfilt_exists <- !is.null(objects$filters$tcfilt)
  toggle("tcfilt_exists", condition = tcfilt_exists, anim = TRUE)
  toggleState("min_num_trans", condition = !tcfilt_exists)
  toggleCssClass("min_num_trans", "grey_text", condition = tcfilt_exists)
  
  ## RNA filter ...
  # ... library size ...
  rnafilt_libsize_exists <- !is.null(objects$filters$rnafilt_libsize)
  toggle("rnafilt_libsize_exists", condition = rnafilt_libsize_exists, anim = TRUE)
  toggleState("rnafilt_min_lib_size", condition = !rnafilt_libsize_exists)
  # ... and minimum nonzero ...
  rnafilt_min_nonzero_exists <- !is.null(objects$filters$rnafilt_min_nonzero)
  toggle("rnafilt_min_nonzero_exists", condition = rnafilt_min_nonzero_exists, anim = TRUE)
  toggleState("rnafilt_min_nonzero", condition = !rnafilt_min_nonzero_exists)
  
  ## rMd-Filter
  rmdfilt_exists <- !is.null(objects$filters$rmdfilt) & (!is.null(objects$filters$rmdfilt_2) | is.null(objects$omicsData_2))
  
  toggle("rmdfilt_exists", condition = rmdfilt_exists, anim = TRUE)
  toggleState("pvalue_threshold", condition = !rmdfilt_exists)
  toggleCssClass("pvalue_threshold", "grey_text", condition = rmdfilt_exists)
  toggleState("rmd_metrics", condition = !rmdfilt_exists)
  toggleCssClass(class = "grey_disabled", condition = rmdfilt_exists, selector = "button[data-id='rmd_metrics']")
  
  ## Custom Filter
  customfilt_exists <- !is.null(objects$filters$customfilt) | !is.null(objects$filters$customfilt_2)
  
  toggle("customfilt_exists", condition = customfilt_exists, anim = TRUE)
  toggleState("remove_or_keep", condition = !customfilt_exists)
  
  customfilt_pickers__ <- c(
    "fdata_customfilt_choices",
    "fdata_customfilt_choices_2",
    "edata_customfilt_remove_mols_1",
    "edata_customfilt_remove_mols_2",
    "emeta_customfilt_which_col_1",
    "emeta_customfilt_which_col_2",
    "emeta_customfilt_which_values_1",
    "emeta_customfilt_which_values_2"
  )
  
  for (picker_id in customfilt_pickers__) {
    toggleState(picker_id, condition = !customfilt_exists)
    toggleCssClass(class = "grey_disabled",
                   condition = customfilt_exists,
                   selector = sprintf("button[data-id='%s']", picker_id))
  }
})

# create molfilt objects
observeEvent(input$add_molfilt, {
  molfilt_exists <- !is.null(objects$filters$molfilt) & (!is.null(objects$filters$molfilt_2) | is.null(objects$omicsData_2))

  if (!molfilt_exists) {
    objects$filters$molfilt <- tryCatch(
      {
        revals$warnings_filter$molecule_filter_1 <<- NULL
        molecule_filter(objects$omicsData)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your molecule filter object <br> System error:  ", e)
        revals$warnings_filter$molecule_filter1 <<- messageBox(type = "error", msg)
        NULL
      }
    )
    if (!is.null(objects$omicsData_2)) {
      objects$filters$molfilt_2 <- tryCatch(
        {
          revals$warnings_filter$molecule_filter2 <<- NULL
          molecule_filter(objects$omicsData_2)
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second molecule filter object <br> System error:  ", e)
          revals$warnings_filter$molecule_filter2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$molecule_filter_1 <- revals$warnings_filter$molecule_filter2 <- objects$filters$molfilt <- objects$filters$molfilt_2 <- NULL
  }
})

# create cvfilt objects
observeEvent(input$add_cvfilt, {
  cvfilt_exists <- !is.null(objects$filters$cvfilt) & (!is.null(objects$filters$cvfilt_2) | is.null(objects$omicsData_2))

  if (!cvfilt_exists) {
    objects$filters$cvfilt <- tryCatch(
      {
        revals$warnings_filter$cv_filter1 <<- NULL
        tmp_cvfilt <- cv_filter(objects$omicsData)
        test_filt_conditions <- summary(
          tmp_cvfilt,
          cv_threshold = input$cv_threshold
        )
        tmp_cvfilt
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your CV filter object <br> System error:  ", e)
        revals$warnings_filter$cv_filter1 <<- messageBox(type = "error", msg)
        NULL
      }
    )
    if (!is.null(objects$omicsData_2)) {
      objects$filters$cvfilt_2 <- tryCatch(
        {
          revals$warnings_filter$cv_filter2 <<- NULL
          tmp_cvfilt <- cv_filter(objects$omicsData_2)
          test_filt_conditions <- summary(
            tmp_cvfilt,
            cv_threshold = input$cv_threshold
          )
          cv_filter(objects$omicsData_2)
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second CV filter object <br> System error:  ", e)
          revals$warnings_filter$cv_filter2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$cv_filter1 <- revals$warnings_filter$cv_filter2 <- objects$filters$cvfilt <- objects$filters$cvfilt_2 <- NULL
  }
})

# create profilt object
# this should never be an option if we're in lipid land
observeEvent(input$add_profilt, {
  if (is.null(objects$filters$profilt)) {
    objects$filters$profilt <- tryCatch(
      {
        revals$warnings_filter$profilt <<- NULL
        proteomics_filter(objects$omicsData)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your proteomics filter object <br> System error:  ", e)
        revals$warnings_filter$profilt <<- messageBox(type = "error", msg)
        objects$filters$profilt
      }
    )
  }
  else {
    revals$warnings_filter$profilt <- objects$filters$profilt <- NULL
  }

  # toggle('remove_profilt', condition = !is.null(objects$filters$profilt), anim = TRUE)
  toggle("profilt_exists", condition = !is.null(objects$filters$profilt), anim = TRUE)
  toggleState("min_num_peps", condition = is.null(objects$filters$profilt))
  toggleCssClass("min_num_peps", "grey_text", condition = !is.null(objects$filters$profilt))
  toggleState("degen_peps", condition = is.null(objects$filters$profilt))
  toggleCssClass("degen_peps", condition = !is.null(objects$filters$profilt))
})

#'@details hide/show filters based on if we are processing the correct data type
#'for example, the proteomics filt is only for peptide level data.
observeEvent(objects$omicsData, {
  toggle("profilt_ftab_UI", condition = inherits(objects$omicsData, "pepData"))
  
  # seqData has very specific types of filters that can/cannot be applied
  toggle("tcfilt_ftab_UI", condition = inherits(objects$uploaded_omicsData, "seqData"))
  toggle("rnafilt_libsize_ftab_UI", condition = inherits(objects$uploaded_omicsData, "seqData"))
  toggle("rnafilt_min_nonzero_ftab_UI", condition = inherits(objects$uploaded_omicsData, "seqData"))
  
  toggle("cvfilt_ftab_UI", condition = !inherits(objects$omicsData, "seqData"))
  toggle("imdanovafilt_ftab_UI", condition = !inherits(objects$omicsData, "seqData"))
  toggle("rmdfilt_ftab_UI", condition = !inherits(objects$omicsData, "seqData"))
})

# create imdanovafilt object
observeEvent(input$add_imdanovafilt, {
  imdanovafilt_exists <- !is.null(objects$filters$imdanovafilt) & (!is.null(objects$filters$imdanovafilt_2) | is.null(objects$omicsData_2))

  if (!imdanovafilt_exists) {
    objects$filters$imdanovafilt <- tryCatch(
      {
        revals$warnings_filter$imdanovafilt1 <<- NULL
        imdanova_filter(objects$omicsData)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your imd-ANOVA filter object <br> System error:  ", e)
        revals$warnings_filter$imdanovafilt1 <<- messageBox(type = "error", msg)
        NULL
      }
    )

    if (!is.null(objects$omicsData_2)) {
      objects$filters$imdanovafilt_2 <- tryCatch(
        {
          revals$warnings_filter$imdanovafilt2 <<- NULL
          imdanova_filter(objects$omicsData_2)
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second imd-ANOVA filter object <br> System error:  ", e)
          revals$warnings_filter$imdanovafilt2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$imdanovafilt1 <- revals$warnings_filter$imdanovafilt2 <- objects$filters$imdanovafilt <- objects$filters$imdanovafilt_2 <- NULL
  }

  imdanovafilt_exists <- !is.null(objects$filters$imdanovafilt) & 
    (!is.null(objects$filters$imdanovafilt_2) | is.null(objects$omicsData_2))
})

#'@details Create total count filter
observeEvent(input$add_tcfilt, {
  tcfilt_exists <- !is.null(objects$filters$tcfilt)
  
  if (!tcfilt_exists) {
    objects$filters$tcfilt <- tryCatch(
      {
        revals$warnings_filter$tcfilt <<- NULL
        total_count_filter(objects$omicsData)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your total count filter object <br> System error:  ", e)
        revals$warnings_filter$tcfilt <<- messageBox(type = "error", msg)
        NULL
      }
    )
  }
  else {
    revals$warnings_filter$tcfilt <- objects$filters$tcfilt <- NULL
  }
})

# create rmdfilt object
observeEvent(input$add_rmdfilt, {
  rmdfilt_exists <- !is.null(objects$filters$rmdfilt) & 
    (!is.null(objects$filters$rmdfilt_2) | is.null(objects$omicsData_2))
  
  if (!rmdfilt_exists) {
    
    objects$filters$rmdfilt <- tryCatch(
      {
        revals$warnings_filter$rmdfilt1 <<- NULL
        rmd_filter(objects$omicsData, metrics = input$rmd_metrics)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your rMd filter object <br> System error:  ", e)
        revals$warnings_filter$rmdfilt1 <<- messageBox(type = "error", msg)
        NULL
      }
    )
    if (!is.null(objects$omicsData_2)) {
      objects$filters$rmdfilt_2 <- tryCatch(
        {
          revals$warnings_filter$rmdfilt2 <<- NULL
          rmd_filter(objects$omicsData_2, metrics = input$rmd_metrics)
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second rMd filter object <br> System error:  ", e)
          revals$warnings_filter$rmdfilt2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$rmdfilt1 <- revals$warnings_filter$rmdfilt2 <- objects$filters$rmdfilt <- objects$filters$rmdfilt_2 <- NULL
  }
  
 
})

#'@details Create RNA library size filter
observeEvent(input$add_rnafilt_libsize, {
  rnafilt_exists <- !is.null(objects$filters$rnafilt_libsize)

  if (!rnafilt_exists) {
    objects$filters$rnafilt_libsize <- tryCatch(
      {
        revals$warnings_filter$rnafilt_libsize <<- NULL
        RNA_filter(objects$omicsData)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your RNA filter object <br> System error:  ", e)
        revals$warnings_filter$rnafilt_libsize <<- messageBox(type = "error", msg)
        NULL
      }
    )
  }
  else {
    revals$warnings_filter$rnafilt <- objects$filters$rnafilt_libsize <- NULL
  }
})

#'@details Create RNA minimum nonzero filter
observeEvent(input$add_rnafilt_min_nonzero, {
  rnafilt_exists <- !is.null(objects$filters$rnafilt_min_nonzero)
  
  if (!rnafilt_exists) {
    objects$filters$rnafilt_min_nonzero <- tryCatch(
      {
        revals$warnings_filter$rnafilt_min_nonzero <<- NULL
        RNA_filter(objects$omicsData)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your RNA filter object <br> System error:  ", e)
        revals$warnings_filter$rnafilt_min_nonzero <<- messageBox(type = "error", msg)
        NULL
      }
    )
  }
  else {
    revals$warnings_filter$rnafilt <- objects$filters$rnafilt_min_nonzero <- NULL
  }
})

# create customfilt object
observeEvent(input$add_customfilt, {
  customfilt_exists <- !is.null(objects$filters$customfilt) | !is.null(objects$filters$customfilt_2)
  
  revals$warnings_filter$customfilt <- revals$warnings_filter$customfilt_2 <- NULL
  
  if (!customfilt_exists) {
    objects$filters$customfilt <- tryCatch(
      {
        # removed samples either user specified samples, or in the case of 'Keep', the complement of user specified samples
        samples_rmv <- if (input$remove_or_keep == "Remove"){
          input$fdata_customfilt_choices
        } else setdiff(sample_names(), input$fdata_customfilt_choices)
        
        if (any(
          length(samples_rmv) > 0,
          length(input$edata_customfilt_remove_mols_1) > 0,
          length(e_meta_remove_rv()) > 0
        )) {
          custom_filter(
            objects$omicsData, 
            f_data_remove = samples_rmv,
            e_data_remove = input$edata_customfilt_remove_mols_1,
            e_meta_remove = e_meta_remove_rv()
          ) 
        } else NULL
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your custom sample filter object <br> System error:  ", e)
        revals$warnings_filter$customfilt1 <<- messageBox(type = "error", msg)
        NULL
      }
    )
    if (!is.null(objects$omicsData_2)) {
      objects$filters$customfilt_2 <- tryCatch(
        {
          samples_rmv <- if (input$remove_or_keep == "Remove"){
            input$fdata_customfilt_choices_2
          } else setdiff(sample_names_2(), input$fdata_customfilt_choices_2)
          
          if (any(
            length(samples_rmv) > 0,
            length(input$edata_customfilt_remove_mols_2) > 0,
            length(e_meta_remove_rv_2()) > 0
          )) {
            custom_filter(
              objects$omicsData_2, 
              f_data_remove = samples_rmv,
              e_data_remove = input$edata_customfilt_remove_mols_2,
              e_meta_remove = e_meta_remove_rv_2()
            ) 
          } else NULL
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second custom sample filter object <br> System error:  ", e)
          revals$warnings_filter$customfilt2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$customfilt1 <- revals$warnings_filter$customfilt2 <- objects$filters$customfilt <- objects$filters$customfilt_2 <- NULL
  }

  # customfilt_exists <- !is.null(objects$filters$customfilt) | !is.null(objects$filters$customfilt_2)
  # 
  # toggle("customfilt_exists", condition = customfilt_exists, anim = TRUE)
  # toggleState("remove_or_keep", condition = !customfilt_exists)
  # toggleState("fdata_customfilt_choices", condition = !customfilt_exists)
  # toggleState("fdata_customfilt_choices_2", condition = !customfilt_exists)
  # toggleCssClass(class = "grey_disabled", condition = customfilt_exists, selector = "button[data-id='fdata_customfilt_choices']")
  # toggleCssClass(class = "grey_disabled", condition = customfilt_exists, selector = "button[data-id='fdata_customfilt_choices_2']")
})


###############################
#### Filter plot observers ####
###############################

# total count filter plot
observeEvent(input$plot_tcfilt, {
  req(input$min_num_trans > 0)
  
  min_num_trans <- if(isTruthy(input$min_num_trans)) input$min_num_trans else NULL

  plots$filter_mainplot <- tryCatch(
    {
      revals$warnings_filter$tcfilt_plot <<- NULL
      
      p <- plot(
        filter_obj = total_count_filter(objects$omicsData),
        min_count = min_num_trans,
        interactive = T
      )
      
      p
    },
    error = function(e) {
      msg <- paste0("Something went wrong plotting your total count filter plot <br> System error: ", e)
      revals$warnings_filter$rnafilt_plot <<- messageBox(type = "error", msg)
      NULL
    }
  )
  
}, ignoreInit = T)

# rnafilter library size plot
observeEvent(input$plot_rnafilt_libsize, {
  req(input$plot_rnafilt_libsize > 0)
  revals$filter_vis <- "rnafilt_libsize"
  
  size_library <- if(isTruthy(input$rnafilt_min_lib_size)) input$rnafilt_min_lib_size else NULL
  
  plots$filter_mainplot <- tryCatch(
    {
      revals$warnings_filter$rnafilt_plot <<- NULL
      
      p <- plot(
        filter_obj = RNA_filter(objects$omicsData),
        plot_type = "library",
        size_library = size_library,
        interactive = T
      )
      
      p
    },
    error = function(e) {
      msg <- paste0("Something went wrong plotting your rna filter plot <br> System error: ", e)
      revals$warnings_filter$rnafilt_plot <<- messageBox(type = "error", msg)
      NULL
    }
  )
  
}, ignoreInit = T)

# rnafilter minimum nonzero plot
observeEvent(input$plot_rnafilt_min_nonzero, {
  req(input$plot_rnafilt_min_nonzero > 0)
  revals$filter_vis <- "rnafilt_min_nonzero"
  
  min_nonzero <- if(isTruthy(input$rnafilt_min_nonzero)) input$rnafilt_min_nonzero else NULL
  
  plots$filter_mainplot <- tryCatch(
    {
      revals$warnings_filter$rnafilt_plot <<- NULL
      
      p <- plot(
        filter_obj = RNA_filter(objects$omicsData),
        plot_type = "biomolecule",
        min_nonzero = min_nonzero,
        interactive = T
      )
      
      p
    },
    error = function(e) {
      msg <- paste0("Something went wrong plotting your rna filter plot <br> System error: ", e)
      revals$warnings_filter$rnafilt_plot <<- messageBox(type = "error", msg)
      NULL
    }
  )
  
}, ignoreInit = T)

# rmdfilter plot
observeEvent(c(input$plot_rmdfilt, input$rmd_metrics, input$pvalue_threshold, input$rmd_sample, input$rmdfilt_plot_type, input$plot_update_rmdfilt),
  {
    req(input$plot_rmdfilt > 0)
    revals$filter_vis <- "rmdfilt"
    # store selected sample ID's or NULL if we are plotting all samples
    sampleID1 <- if (length(input$rmd_sample) > 0 & (input$rmdfilt_plot_type %in% c("subset", "outliers"))) input$rmd_sample else NULL

    revals$warnings_filter$rmdfilt_plot <- revals$warnings_filter$rmdfilt_plot_2 <- NULL

    # store plot object in reactive variable
    plots$filter_mainplot <- tryCatch(
      {
        p <- plot(rmd_filter(objects$omicsData, 
                             metrics = input$rmd_metrics),
                  pvalue_threshold = input$pvalue_threshold, 
                  order_by = if (is.null(input$filter_rmd_order_by) || input$filter_rmd_order_by == NULLSELECT_) NULL else input$filter_rmd_order_by,
                  sampleID = sampleID1, bw_theme = TRUE, interactive = T,
                  use_VizSampNames = TRUE
                  )
        
        p
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your rmd filter plot <br> System error: ", e)
        revals$warnings_filter$rmdfilt_plot <<- messageBox(type = "error", msg)
        NULL
      }
    )

    if (!is.null(objects$omicsData_2)) {
      sampleID2 <- if (length(input$rmd_sample_2) > 0 & (input$rmdfilt_plot_type %in% c("subset", "outliers"))) input$rmd_sample_2 else NULL

      plots$filter_mainplot_2 <- tryCatch(
        {
          p <- plot(rmd_filter(objects$omicsData_2, metrics = input$rmd_metrics),
            pvalue_threshold = input$pvalue_threshold, sampleID = sampleID2, 
            bw_theme = TRUE, interactive = T
          )
          
          p
        },
        error = function(e) {
          msg <- paste0("Something went wrong making your second rmd filter plot <br> System error: ", e)
          revals$warnings_filter$rmdfilt_plot_2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  },
  ignoreInit = T
)

# proteomics filter plot
observeEvent(c(input$plot_profilt, input$min_num_peps, input$degen_peps),
  {
    req(input$plot_profilt > 0)
    revals$warnings_filter$profilt_plot <- NULL
    revals$filter_vis <- "profilt"

    plots$filter_mainplot <- tryCatch(
      {
        plot(proteomics_filter(objects$omicsData), 
             min_num_peps = as.numeric(input$min_num_peps), 
             bw_theme = TRUE, interactive = T
             )
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your proteomics filter plot <br> System error: ", e)
        revals$warnings_filter$profilt_plot <<- messageBox(type = "error", msg)
        NULL
      }
    )
  },
  ignoreInit = T
)

# molecule filter plot
observeEvent(c(input$plot_molfilt, input$mol_min_num),
  {
    req(input$plot_molfilt > 0)
    revals$warnings_filter$molfilt_plot <- revals$warnings_filter$molfilt_plot_2 <- NULL
    revals$filter_vis <- "molfilt"

    plots$filter_mainplot <- tryCatch(
      {
        plot(molecule_filter(objects$omicsData), 
             min_num = input$mol_min_num, bw_theme = TRUE, interactive = T
             )
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your molecule filter plot <br> System error: ", e)
        revals$warnings_filter$molfilt_plot <<- messageBox(type = "error", msg)
        NULL
      }
    )

    if (!is.null(objects$omicsData_2)) {
      plots$filter_mainplot_2 <- tryCatch(
        {
          plot(molecule_filter(objects$omicsData_2), 
               min_num = input$mol_min_num, 
               bw_theme = TRUE, interactive = T
               )
        },
        error = function(e) {
          msg <- paste0("Something went wrong making your second molecule filter plot <br> System error: ", e)
          revals$warnings_filter$molfilt_plot_2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  },
  ignoreInit = TRUE
)

# cv filter plot
observeEvent(c(input$plot_cvfilt, input$cv_threshold),
  {
    req(input$plot_cvfilt > 0 && !is.na(input$cv_threshold))
    revals$warnings_filter$cvfilt_plot <- revals$warnings_filter$cvfilt_plot_2 <- NULL
    revals$filter_vis <- "cvfilt"
    
    plots$filter_mainplot <- tryCatch(
      {
        p <- plot(cv_filter(objects$omicsData),
             cv_threshold = input$cv_threshold,
             bw_theme = TRUE)
        title_info <- paste(str_extract_all(as.character(p$labels$title)[3], "[0-9]+")[[1]], collapse = ".")
        p$labels$title <- paste0("Coefficient of Variation (CV): CV Threshold = ",
                                 title_info)
        ggplotly(p)
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your CV filter plot <br> System error: ", e)
        revals$warnings_filter$cvfilt_plot <<- messageBox(type = "error", msg)
        NULL
      }
    )

    if (!is.null(objects$omicsData_2)) {
      plots$filter_mainplot_2 <- tryCatch(
        {
          p <- plot(cv_filter(objects$omicsData_2),
                    cv_threshold = input$cv_threshold,
                    bw_theme = TRUE)
          title_info <- paste(str_extract_all(as.character(p$labels$title)[3], "[0-9]+")[[1]], collapse = ".")
          p$labels$title <- paste0("Coefficient of Variation (CV): CV Threshold = ",
                                   title_info)
          ggplotly(p)
        },
        error = function(e) {
          msg <- paste0("Something went wrong making your second CV filter plot <br> System error: ", e)
          revals$warnings_filter$cvfilt_plot_2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  },
  ignoreInit = TRUE
)

# imd anova filter plot
observeEvent(c(input$plot_imdanovafilt, input$min_nonmiss_anova, input$min_nonmiss_gtest),
  {
    req(input$plot_imdanovafilt > 0)
    revals$warnings_filter$imdanova_plot <- revals$warnings_filter$imdanova_plot_2 <- NULL
    revals$filter_vis <- "imdanovafilt"

    plots$filter_mainplot <- tryCatch(
      {
        plot(imdanova_filter(objects$omicsData), 
             min_nonmiss_anova = input$min_nonmiss_anova, 
             min_nonmiss_gtest = input$min_nonmiss_gtest, 
             bw_theme = TRUE, interactive = T
             )
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your iMd-ANOVA filter plot <br> System error: ", e)
        revals$warnings_filter$imdanova_plot <<- messageBox(type = "error", msg)
        NULL
      }
    )

    if (!is.null(objects$omicsData_2)) {
      plots$filter_mainplot_2 <- tryCatch(
        {
          plot(imdanova_filter(objects$omicsData_2), 
               min_nonmiss_anova = input$min_nonmiss_anova, 
               min_nonmiss_gtest = input$min_nonmiss_gtest, 
               bw_theme = TRUE, interactive = T
               )
        },
        error = function(e) {
          msg <- paste0("Something went wrong making your second iMd-ANOVA filter plot <br> System error: ", e)
          revals$warnings_filter$imdanova_plot_2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  },
  ignoreInit = TRUE
)

####

##########################
# apply plot styling to...
##########################

# ...first plot...
observeEvent(input$filter_apply_style_plot_1, {
  
  if (inherits(plots$filter_mainplot, "list")) {
    plots$filter_mainplot[[1]] <- add_plot_styling(input, 
                                                   "filter",
                                                   plots$filter_mainplot[[1]])
  }
  else {
    plots$filter_mainplot <- add_plot_styling(input, "filter", plots$filter_mainplot)
  }
})

# ...second plot
observeEvent(input$filter_apply_style_plot_2, {

  if (inherits(plots$filter_mainplot, "list")) {
    plots$filter_mainplot[[2]] <- add_plot_styling(input, 
                                                   "filter",
                                                   plots$filter_mainplot[[2]])
  }
  else {
    plots$filter_mainplot_2 <- add_plot_styling(input, 
                                                "filter",
                                                plots$filter_mainplot_2)
  }
})

#####

# filter review modal
observeEvent(input$review_filters, {
  showModal(
    modalDialog(
      tagList(
        uiOutput("filter_review"),
        uiOutput("execute_apply_filters_UI"),
        hr(),
        uiOutput("warnings_filter_modal")
      ),
      footer = NULL,
      size = "l"
    )
  )
})

#################
# Apply Filters #
#################
observeEvent(input$apply_filters, {
  req(!is.null(objects$omicsData))
  
  n_orig_filters <- length(attributes(objects$omicsData)$filters)
  n_orig_filters_2 <- length(attributes(objects$omicsData_2)$filters)
  
  res <- apply_filt()
  
  # two logicals that are TRUE is something went wrong, used to determine if success modal appears
  error_msg <- is.character(res)
  if (!is.null(objects$omicsData_2)) {
    empty_object <- nrow(objects$omicsData$e_data) == 0 | nrow(objects$omicsData_2$e_data) == 0
  }
  else {
    empty_object <- nrow(objects$omicsData$e_data) == 0
  }

  # clear old error messages
  revals$warnings_filter$badfilter <- revals$warnings_filter$empty_object <- NULL

  # store new error messages if something went wrong, else show the success modal
  if (error_msg) {
    revals$warnings_filter$badfilter <- sprintf("<p style = 'color:red'>%s</p>", res)
  }
  else if (empty_object) {
    revals$warnings_filter$empty_object <- sprintf("<p style = 'color:red'>%s</p>", "These filters removed all molecules in one or more datasets, please revise")
  }
  else {
    revals$warnings_filter$badfilter <- NULL

    # collapse the filter panels for space to view tables
    updateCollapse(session, "filter_collapse", close = c("data_filters", "sample_filters"))

    # text summarizing the effect of filters...

    cond_text1 <- if (is.null(objects$omicsData_2)) "" else sprintf("%s:  ", omic_1_name())
    cond_text2 <- if (is.null(objects$omicsData_2)) "" else sprintf("%s:  ", omic_2_name())

    filters1 <- FILTER_NAMES %>% # filter_names is a global variable defined in global.R
      filter(attribute %in% map_chr(attributes(objects$omicsData)$filters, 1)) %>%
      pluck("text") %>%
      paste(collapse = ", ")

    # ... second object
    if (!is.null(objects$omicsData_2)) {
      filters2 <- FILTER_NAMES %>%
        filter(attribute %in% map_chr(attributes(objects$omicsData_2)$filters, 1)) %>%
        pluck("text") %>%
        paste(collapse = ", ")
      filters2_div <- tagList(HTML(paste0(cond_text2, filters2)), hr())
    } else {
      filters2 <- filters2_div <- NULL
    }

    if(inherits(objects$omicsData, "nmrData")){
      buttons <- div(
        actionButton("filter_dismiss", "Stay on this tab", width = "75%"),
        br(),
        actionButton("goto_norm", "Continue to Normalization tab (optional)", 
                     style = "margin-top:5px;width:75%"),
        br(),
        actionButton("goto_stats_filter", "Continue to Statistics tab", 
                     style = "margin-top:5px;width:75%")
      )
    } else if(inherits(objects$omicsData, "seqData")){
      buttons <- div(
        actionButton("filter_dismiss", "Stay on this tab", width = "75%"),
        br(),
        actionButton("goto_stats_filter", "Continue to Statistics tab", 
                     style = "margin-top:5px;width:75%")
      )
    } else {
      buttons <- div(
        actionButton("filter_dismiss", "Stay on this tab", width = "75%"),
        br(),
        actionButton("goto_norm", "Continue to Normalization tab", style = "margin-top:5px;width:75%")
      )
    }
    
    applied_filters <- attributes(objects$omicsData)$filters[n_orig_filters:length(attributes(objects$omicsData)$filters)]
    applied_filters_2 <- attributes(objects$omicsData_2)$filters[n_orig_filters_2:length(attributes(objects$omicsData_2)$filters)]
    
    non_applied_filters <- FILTER_NAMES %>% # 
      filter(
        attribute %in% map_chr(objects$filters, ~class(.x)[1]),
        !(attribute %in% map_chr(applied_filters, 1))
      ) %>%
      pluck("text")

    non_applied_filters_2 <- if(two_lipids() || two_metab()) {
      FILTER_NAMES %>% # 
        filter(
          attribute %in% map_chr(objects$filters, ~class(.x)[1]),
          !(attribute %in% map_chr(applied_filters_2, 1))
        ) %>%
        pluck("text")
    } else NULL

    nofilt_div <- if (length(non_applied_filters) > 0) {
      div(sprintf("%s%s", cond_text1, paste(non_applied_filters, collapse = ", ")))
    } else NULL
    
    nofilt_div2 <- if (length(non_applied_filters_2) > 0) {
      div(sprintf("%s%s", cond_text2, paste(non_applied_filters_2, collapse = ", ")))
    } else NULL
    
    nofilt_header = if(is.null(nofilt_div) & is.null(nofilt_div2)) {
      NULL
    } else tags$h4(style= "color:#1A5276", "The following filters had no effect:")
    
    nofilt_hr = if(is.null(nofilt_header)) NULL else hr()

    showModal(
      modalDialog(
        title = "Filters Applied",
        fluidRow(
          column(10,
            align = "center", offset = 1,
            HTML(sprintf('<h4 style= "color:#1A5276">The following filters were applied to your data:</h4>')),
            HTML(paste0(cond_text1, filters1)),
            hr(),
            filters2_div,
            nofilt_header,
            nofilt_div,
            nofilt_div2,
            nofilt_hr,
            buttons
          )
        ),
        footer = NULL
      )
    )
  }
})

#'@details enable the apply button if applying filters would cause a reset of
#'stats or normalization
observeEvent(input$allow_reapply_filters, {
  shinyjs::enable("apply_filters")
}, ignoreInit = T)

### Filter Disabling 
#' Filters are created using `add_filter_UI`, which wraps all elements in a div
#' having css class `tooltip-wrapper` and id `sprintf("add_%s_ttip_control", filter_name)`
#' We can use togglestate_add_tooltip, supposedly, to enable/disable based on some condition.

#' @details Disable the rmd_filter if the number of metrics chosen is too large relative to the number of samples.  This throws a warning in pmartR, but can throw a non-informative error lower down in the robust pca code we are using.
observeEvent(input$rmd_metrics, {
  req(grepl("^filter_tab$", input$top_page, perl = TRUE), !is.null(objects$omicsData))
  
  # this is the same logic that throws a warning in pmartR, we use it to completely disallow rmd_filter.
  nsamps_too_low <- dim(objects$omicsData$f_data)[1] < 2 * length(input$rmd_metrics)
  
  togglestate_add_tooltip(session, "add_rmdfilt_ttip_control",
    condition = !nsamps_too_low,
    tooltip_text = ttext_[["RMD_TOO_MANY_METRICS"]],
    position = "right"
  )

})

#' @details Imd-ANOVA filter, disable if we don't have at least 3 samples per group
#' ... disable functionality (min/max values on G-test/ANOVA)
observeEvent(c(input[["min_nonmiss_gtest"]], input[["min_nonmiss_anova"]], objects$omicsData), {
  req(grepl("^filter_tab$", input$top_page, perl = TRUE),
      num_groups() > 1,
      !inherits(objects$omicsData, "seqData"))
  # check that gtest and anova minimum values are valid
  # empty numericinput (NA) is ok as long as one of them is valid
  tmpfilt <- tryCatch(
    {
      revals$warnings_filter$imdanovafilt_min_num <<- NULL
      imdanova_filter(objects$omicsData)
    },
    error = function(e) {
      msg = sprintf("An imd-anova filter cannot be created:  %s", e)
      revals$warnings_filter$imdanovafilt_min_num <<- messageBox(type = "error", msg)
      return(msg)
    }
  )

  if (!inherits(tmpfilt, 'imdanovaFilt')) {
    return(NULL)
  }

  gsizes <- unique(attributes(tmpfilt)$group_sizes$n_group)
  minsize <- min(gsizes)
  # get the next largest group, we assume there is at least one non-singleton group
  if (minsize == 1) {
    minsize <- sort(gsizes)[2]
  }

  # gtest must be less than minimum group size...
  gtest_ok1 <- ifelse(is.na(input$min_nonmiss_gtest), TRUE,
    input$min_nonmiss_gtest <= minsize
  )
  # ...and >= 3
  gtest_ok2 <- ifelse(is.na(input$min_nonmiss_gtest), TRUE,
    input$min_nonmiss_gtest >= 3
  )

  # anova must be less than minimum group size...
  anova_ok1 <- ifelse(is.na(input$min_nonmiss_anova), TRUE,
    input$min_nonmiss_anova <= minsize
  )
  # ... and >= 2
  anova_ok2 <- ifelse(is.na(input$min_nonmiss_anova), TRUE,
    input$min_nonmiss_anova >= 2
  )

  gtest_ok <- gtest_ok1 & gtest_ok2
  anova_ok <- anova_ok1 & anova_ok2

  # have to specify at least one min_nonmiss value
  both_na <- is.na(input$min_nonmiss_gtest) & is.na(input$min_nonmiss_anova)
  
  # initial failure check for input parameters
  fail_check <- any(!(gtest_ok & anova_ok), both_na)
  
  # In flow control because the other inputs need to be valid for this to run.
  if(!fail_check) {
    # It filters out at least one observation
    min_nonmiss_gtest <- if (is.na(input$min_nonmiss_gtest)) NULL else input$min_nonmiss_gtest
    min_nonmiss_anova <- if (is.na(input$min_nonmiss_anova)) NULL else input$min_nonmiss_anova
    
    imd_summ = summary(tmpfilt, min_nonmiss_anova, min_nonmiss_gtest)
    none_filtered = is.null(imd_summ$num_filtered) | imd_summ$num_filtered == 0
  } else{
    # Let it fail for the other resons
    none_filtered = FALSE
  }
  
  fail_check <- any(fail_check, none_filtered)
  
  tooltip_text <- case_when(
    !(gtest_ok & anova_ok) ~ sprintf(ttext_[["IMDANOVA_VALUES_OUT_OF_RANGE"]], minsize),
    both_na ~ ttext_[["IMDANOVA_BOTH_NA"]],
    none_filtered ~ ttext_[["IMDANOVA_NONE_FILTERED"]],
    TRUE ~ ""
  )

  togglestate_add_tooltip(session, "add_imdanovafilt_ttip_control",
    condition = !fail_check,
    tooltip_text = tooltip_text,
    position = "right"
  )
})


###

#'@details display the checkbox on the collapsepanel if any filters have been
#'successfully added.
observeEvent(reactiveValuesToList(objects), {
  # req(!is.null(objects$filters))
  cond_molfilts <- any(c("molfilt", "cvfilt", "imdanovafilt", "profilt", "tcfilt") %in% names(objects$filters))
  cond_sampfilts <- any(c("rmdfilt", "rnafilt_libsize", "rnafilt_min_nonzero") %in% names(objects$filters))
  cond_customfilt <- any(c("customfilt") %in% names(objects$filters))
  
  toggleElement("ok_data_filters", condition = cond_molfilts, anim = TRUE)
  toggleElement("ok_sample_filters", condition = cond_sampfilts, anim = TRUE)
  toggleElement("ok_custom_filter", condition = cond_customfilt, anim = TRUE)
})

observeEvent(input$goto_norm, {
  updateTabsetPanel(session, "top_page", selected = "normalization_tab")
  removeModal()
})

observeEvent(input$goto_stats_filter, {
  updateTabsetPanel(session, "top_page", selected = "statistics_tab")
  removeModal()
})

observeEvent(input$filter_dismiss, {
  removeModal()
})

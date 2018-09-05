short_summary_df <- function(object) {
  short_sum <- list()
  FAKE <- FALSE
  if (object@Options$optim.method == "none") {
    FAKE <- TRUE
  }
  if (FAKE) {
    short_sum$iter <-
      sprintf("lavaan (%s) -- DRY RUN with 0 iterations\n", 
              utils::packageDescription("lavaan", fields = "Version"))
  } else if (object@optim$iterations > 0) {
    if (object@optim$converged) {
      short_sum$iter <-
        sprintf("lavaan (%s) converged normally after %3i iterations\n", 
                utils::packageDescription("lavaan", fields = "Version"), 
                object@optim$iterations)
    } else {
      short_sum$iter <-
        c(
          sprintf("** WARNING ** lavaan (%s) did NOT converge after %i iterations\n", 
                  utils::packageDescription("lavaan", fields = "Version"), 
                  object@optim$iterations),
          "** WARNING ** Estimates below are most likely unreliable\n"
        )
      
    }
  } else {
    short_sum$iter <-
      c(
        sprintf("** WARNING ** lavaan (%s) model has NOT been fitted\n", 
                utils::packageDescription("lavaan", fields = "Version")),
        "** WARNING ** Estimates below are simply the starting values\n"
      )
  }
  listwise <- FALSE
  
  for (g in 1:object@Data@ngroups) {
    if (object@Data@nobs[[1L]] != object@Data@norig[[1L]]) {
      listwise <- TRUE
      break
    }
  }
  
  short_sum$n_groups <-
    data.frame(
      name_group = NA,
      used = NA,
      total = NA
    )
  
  if (object@Data@ngroups == 1L) {
    # Only define for one group
    # Define short_sum$n_groups because it's common
    # for bot this if statement and the one below
    
    # Previously, the group line had: object@Data@group.label[[1L]]
    # but this raised an index out of range error because when there's
    # only one group there's no group label so I just add a group 1
    # manually
    short_sum$n_groups[1, 1] <- "Group 1"
    short_sum$n_groups[1, 2] <- object@Data@nobs[[1L]]
    short_sum$n_groups[1, 3] <- ifelse(listwise,
                                       object@Data@norig[[1L]],
                                       NA_integer_)
  } else {
    for (g in 1:object@Data@ngroups) {
      short_sum$n_groups[g, 1] <- object@Data@group.label[[g]]
      short_sum$n_groups[g, 2] <- object@Data@nobs[[g]]
      short_sum$n_groups[g, 3] <- ifelse(listwise,
                                         object@Data@norig[[g]],
                                         NA_integer_)
    }
  }
  
  if (object@SampleStats@missing.flag) {
    short_sum$missing_patterns <-
      data.frame(
        name_group = NA,
        npatterns = NA
      )
    
    if (object@Data@ngroups == 1L) {
      short_sum$missing_patterns[1, 1] <- "Group 1"
      short_sum$missing_patterns[1, 2] <- object@Data@Mp[[1L]]$npatterns
    } else {
      for (g in 1:object@Data@ngroups) {
        short_sum$missing_patterns[g, 1] <- object@Data@group.label[[g]]
        short_sum$missing_patterns[g, 2] <- object@Data@Mp[[g]]$npatterns
      }
    }
  }
  
  # long tests that I saved before the ifelse statement
  new_test <- object@Options$test %in% c("satorra.bentler", "yuan.bentler", 
                                         "mean.var.adjusted", "scaled.shifted")
  length_object <- length(object@test) > 1L
  
  if (new_test && length_object) {
    scaled <- TRUE
    if (object@Options$test == "scaled.shifted")  shifted <- TRUE else shifted <- FALSE
  } else {
    scaled <- FALSE
    shifted <- FALSE
  }
  
  short_sum$estimator_details <-
    data.frame(
      estimator = NA,
      ml = NA,
      ml_scaled = NA
    )
  
  if (object@Options$test != "none" && object@Options$estimator != "MML") {
    
    short_sum$estimator_details[1, 1] <- "Minimum Function Test Statistic"
    short_sum$estimator_details[1, 2] <- object@test[[1]]$stat
    
    if (scaled) {
      short_sum$estimator_details[1, 3] <- object@test[[2]]$stat
    }
    
    short_sum$estimator_details[2, 1] <- "Degrees of freedom"
    short_sum$estimator_details[2, 2] <- object@test[[1]]$df
    
    if (scaled) {
      if (round(object@test[[2]]$df) == object@test[[2]]$df) {
        short_sum$estimator_details[2, 3] <- object@test[[2]]$df
      } else {
        short_sum$estimator_details[2, 3] <- round(object@test[[2]]$df, 3)
      }
    }
    
    # Define the p values
    if (is.na(object@test[[1]]$df)) {
      
      short_sum$estimator_details[3, 1] <- "P-value"
      short_sum$estimator_details[3, 2] <- round(object@test[[1]]$pvalue, 3)
      if (scaled) {
        short_sum$estimator_details[3, 3] <- round(object@test[[2]]$pvalue, 3)
      }
      
    } else if (object@test[[1]]$df > 0) {
      
      # There might be different p values, so here we test
      # which one it is, to later name the p value differently
      chisq <- object@test[[1]]$refdistr == "chisq"
      
      unknown <-
        length(object@test) == 1L &&
        object@test[[1]]$refdistr == "unknown"
      
      if (chisq) {
        short_sum$estimator_details[3, 1] <- "P-value (Chi-square)"
      } else if (unknown) {
        short_sum$estimator_details[3, 1] <- "P-value (Unknown)"
      } else {
        short_sum$estimator_details[3, 1] <- "P-value"
      }
      
      short_sum$estimator_details[3, 2] <- round(object@test[[1]]$pvalue, 3)
      if (scaled) {
        short_sum$estimator_details[3, 3] <- round(object@test[[2]]$pvalue, 3)
      }
      
    } else {
      if (object@optim$fx > 0) {
        short_sum$estimator_details[3, 1] <- "Minimum Function Value"
        short_sum$estimator_details[3, 2] <- object@optim$fx
      }
    }
    
    if (object@Options$test == "bollen.stine") {
      short_sum$estimator_details[4, 1] <- "P-value (Bollen-Stine Bootstrap)"
      short_sum$estimator_details[4, 2] <- round(object@test[[2]]$pvalue, 3)
    }
    if (scaled) {
      short_sum$estimator_details[5, 3] <- round(object@test[[2]]$scaling.factor, 3)
      
      if (object@Options$test == "yuan.bentler") {
        if (object@Options$mimic == "Mplus") {
          correction <- "for the Yuan-Bentler correction (Mplus variant)\n"
        } else {
          correction <- "for the Yuan-Bentler correction\n"
        }
      } else if (object@Options$test == "satorra.bentler") {
        if (object@Options$mimic == "Mplus" && object@Options$estimator == "ML") {
          correction <- "for the Satorra-Bentler correction (Mplus variant)\n"
        } else if (object@Options$mimic == "Mplus" && object@Options$estimator == "DWLS") {
          correction <- "for the Satorra-Bentler correction (WLSM)\n"
        } else if (object@Options$mimic == "Mplus" && object@Options$estimator ==  "ULS") {
          correction <- "for the Satorra-Bentler correction (ULSM)\n"
        } else {
          correction <- "for the Satorra-Bentler correction\n"
        }
      } else if (object@Options$test == "mean.var.adjusted") {
        if (object@Options$mimic == "Mplus" && object@Options$estimator == "ML") {
          correction <- "for the mean and variance adjusted correction (MLMV)\n"
        } else if (object@Options$mimic == "Mplus" && object@Options$estimator == "DWLS") {
          correction <- "for the mean and variance adjusted correction (WLSMV)\n"
        } else if (object@Options$mimic == "Mplus" && object@Options$estimator == "ULS") {
          correction <- "for the mean and variance adjusted correction (ULSMV)\n"
        } else {
          correction <- "for the mean and variance adjusted correction\n"
        }
      }
      
      short_sum$estimator_details[5, 1] <-
        paste("Scaling correction factor", correction)
    }
    if (shifted) {
      short_sum$shifted_params$df <-
        data.frame(
          name_group = NA,
          shift_parameter = NA
        )
      
      if (object@Data@ngroups == 1L) {
        short_sum$shifted_params[1, 1] <- "Group 1"
        short_sum$shifted_params[1, 2] <- object@test[[2]]$shift.parameter
      } else {
        for (g in 1:object@Data@ngroups) {
          short_sum$shifted_params[g, 1] <- object@Data@group.label[[g]]
          short_sum$shifted_params[g, 2] <- object@test[[2]]$shift.parameter[g]
        }
      }
      if (object@Options$mimic == "Mplus" && object@Options$estimator == "DWLS") {
        short_sum$shifted_params$notes <-
          "for simple second-order correction (WLSMV)"
      } else {
        short_sum$shifted_params$notes <-
          "for simple second-order correction (Mplus variant)"
      }
    }
    if (object@Data@ngroups > 1L) {
      short_sum$group_chisq <-
        data.frame(
          name_group = NA,
          chisq = NA,
          chisq_scaled = NA
        )
      
      for (g in 1:object@Data@ngroups) {
        short_sum$group_chisq[g, 1] <- object@Data@group.label[[g]]
        short_sum$group_chisq[g, 2] <- round(object@test[[1]]$stat.group[g], 3)
        
        if (scaled) {
          short_sum$group_chisq[g, 3] <- object@test[[2]]$stat.group[g]
        }
      }
    }
  }
  if (object@Options$estimator == "MML") {
    short_sum$fit_measures <-
      lavaan:::fitMeasures(object, c("logl", "npar", "aic", "bic",  "bic2"))
  }
  short_sum
}

# Fit comes from Anna's script where she runs the model and calls it
# fit. fitMeasures extracts all relevant fit measures and then the function
# below prints them really nicely.
#x <- fitMeasures(fit, fit.measures = "default")

fit_measures_details <- function(x) {
  fit_measure <- list()
  
  names.x <- names(x)
  scaled <- "chisq.scaled" %in% names.x
  
  # This function create the df you'll use and loops over each row
  # replacing each row with the fit_name x fit_value combination.
  # If there are also scaled values, the a loop will populate
  # the third row whichi is for the scaled values.
  looper <- function(fit_names, fit_values, fit_values_scaled, should_scaled = scaled) {
    
    the_df <-
      data.frame(
        fit = NA,
        value = NA,
        value_scaled = NA
      )
    
    for (i in seq_along(fit_names)) {
      the_df[i, 1] <- fit_names[i]
      the_df[i, 2] <- fit_values[i]
      
      if (should_scaled) {
        the_df[i, 3] <- fit_values_scaled[i]
      }
    }
    
    the_df
  }
  
  if ("C_F" %in% names.x) {
    
    fit_names <- c(
      "Observed response patterns (1st group)",
      "Total response patterns (1st group)",
      "Empty response patterns (1st group)",
      "C_F Test Statistic",
      "C_F Degrees of freedom",
      "C_F P-value",
      "C_M Test Statistic",
      "C_M Degrees of freedom",
      "C_M P-value"
    )
    
    fit_values <- c(
      x["rpat.observed"],
      x["rpat.total"],
      x["rpat.empty"],
      x["C_F"],
      x["C_F.df"],
      x["C_F.p.value"],
      x["C_M"],
      x["C_M.df"],
      x["C_M.p.value"]
    )
    
    fit_measure$response_patterns <- looper(fit_names,fit_values)
    
    
  }
  if ("C_p" %in% names.x) {
    
    fit_names <- c(
      "C_P Test Statistic",
      "Degrees of freedom",
      "Bonferroni corrected P-value"
    )
    
    fit_values <- c(
      x["C_p"],
      x["C_p.df"],
      x["C_p.p.value"]
    )
    
    fit_measure$pairwise_summary_statistics <- looper(fit_names,fit_values)
  }
  if ("baseline.chisq" %in% names.x) {
    
    fit_names <- c(
      "Minimum Function Test Statistic",
      "Degrees of freedom",
      "P-value"
    )
    
    fit_values <- c(
      x["baseline.chisq"],
      x["baseline.df"],
      x["baseline.pvalue"]
    )
    
    fit_scale_values <- c(
      x["baseline.chisq.scaled"],
      x["baseline.df.scaled"],
      x["baseline.pvalue.scaled"]
    )
    
    fit_scale_values <- vapply(fit_scale_values,
                               round, 3, FUN.VALUE = numeric(1))
    
    fit_measure$baseline_chisq <- looper(fit_names,
                                         fit_values,
                                         fit_scale_values,
                                         should_scaled = scaled)
  }
  if (any(c("cfi", "tli", "nnfi", "rfi", "nfi", "ifi", "rni", "pnfi") %in% names.x)) {
    
    # Create empty df to append indicators which are true
    fit_measure$usermodel_vs_baselinemodel <-
      data.frame(fit = character(),
                 value = numeric(),
                 value_scaled = numeric())
    
    # I create a wrapper function that rbinds the new indicator row
    # to the existing fit_measure df because some of the if statements below
    # need to be appended, that is, some might be true and appended while
    # other might be not!
    baseline_fun <- function(x, f, s, t, the_df = fit_measure$usermodel_vs_baselinemodel) {
      rbind(the_df, looper(f, x[s], x[t]))
    }
    
    #    cat("\nUser model versus baseline model:\n\n")
    if ("cfi" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Comparative Fit Index (CFI)",
                     "cfi",
                     "cfi.scaled")
    }
    
    if ("tli" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Tucker-Lewis Index (TLI)",
                     "tli",
                     "tli.scaled")
    }
    if ("cfi.robust" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Robust Comparative Fit Index (CFI)",
                     "",
                     "cfi.robust")
    }
    if ("tli.robust" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Robust Tucker-Lewis Index (TLI)",
                     "",
                     "tli.robust")
    }
    if ("nnfi" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Bentler-Bonett Non-normed Fit Index (NNFI)",
                     "nnfi",
                     "nnfi.robust")
    }
    if ("nfi" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Bentler-Bonett Normed Fit Index (NFI)",
                     "nfi",
                     "nfi.scaled")
    }
    if ("nfi" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Parsimony Normed Fit Index (PNFI)",
                     "pnfi",
                     "pnfi.scaled")
    }
    if ("rfi" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Bollen's Relative Fit Index (RFI)",
                     "rfi",
                     "rfi.scaled")
    }
    if ("ifi" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Bollen's Incremental Fit Index (IFI)",
                     "ifi",
                     "ifi.scaled")
    }
    if ("rni" %in% names.x) {
      fit_measure$usermodel_vs_baselinemodel <-
        baseline_fun(x,
                     "Relative Noncentrality Index (RNI)",
                     "rni",
                     "rni.robust")
    }
  }
  if ("logl" %in% names.x) {
    # Create empty df to append indicators which are true
    fit_measure$loglike_informationcriteria <-
      data.frame(fit = character(),
                 value = numeric(),
                 value_scaled = numeric())
    
    
    baseline_fun <- function(x, f, s, t, the_df = fit_measure$loglike_informationcriteria) {
      rbind(the_df, looper(f, x[s], x[t]))
    }
    
    #    cat("\nLoglikelihood and Information Criteria:\n\n")
    
    fit_measure$loglike_informationcriteria <-
      baseline_fun(x,
                   "Loglikelihood user model (H0)",
                   "logl",
                   "logl")
    
    if (!is.na(x["scaling.factor.h0"])) {
      fit_measure$loglike_informationcriteria <-
        baseline_fun(x,
                     "Scaling correction factor",
                     "",
                     "scaling.factor.h0")
    }
    if ("unrestricted.logl" %in% names.x) {
      
      fit_measure$loglike_informationcriteria <-
        baseline_fun(x,
                     "Loglikelihood unrestricted model (H1)",
                     "unrestricted.logl",
                     "unrestricted.logl")
      
      if (!is.na(x["scaling.factor.h1"])) {
        
        fit_measure$loglike_informationcriteria <-
          baseline_fun(x,
                       "Scaling correction factor for the MLR correction",
                       "",
                       "scaling.factor.h1")
      }
    }
    
    
    fit_measure$loglike_informationcriteria <-
      baseline_fun(x,
                   "Number of free parameters",
                   "npar",
                   "npar")
    
    fit_measure$loglike_informationcriteria <-
      baseline_fun(x,
                   "Akaike (AIC)",
                   "aic",
                   "aic")
    
    fit_measure$loglike_informationcriteria <-
      baseline_fun(x,
                   "Bayesian (BIC)",
                   "bic",
                   "bic")
    
    if (!is.na(x["bic2"])) {
      
      fit_measure$loglike_informationcriteria <-
        baseline_fun(x,
                     "Sample-size adjusted Bayesian (BIC)",
                     "bic2",
                     "bic2")
    }
  }
  if ("rmsea" %in% names.x) {
    
    # Create empty df to append indicators which are true
    fit_measure$rootmse_approx <-
      data.frame(fit = character(),
                 value = numeric(),
                 value_scaled = numeric())
    
    
    baseline_fun <- function(x, f, s, t, the_df = fit_measure$rootmse_approx) {
      rbind(the_df, looper(f, x[s], x[t]))
    }
    
    #    cat("\nRoot Mean Square Error of Approximation:\n\n")
    
    fit_measure$rootmse_approx <-
      baseline_fun(x,
                   "RMSEA",
                   "rmsea",
                   "rmsea.scaled")
    
    if ("rmsea.ci.lower" %in% names.x) {
      
      fit_measure$rootmse_approx <-
        baseline_fun(x,
                     "90 Percent Confidence Interval on RMSEA (lower)",
                     "rmsea.ci.lower",
                     "rmsea.ci.lower.scaled")
      
      fit_measure$rootmse_approx <-
        baseline_fun(x,
                     "90 Percent Confidence Interval on RMSEA (upper)",
                     "rmsea.ci.upper",
                     "rmsea.ci.upper.scaled")
    }
    if ("rmsea.pvalue" %in% names.x) {
      
      fit_measure$rootmse_approx <-
        baseline_fun(x,
                     "P-value RMSEA <= 0.05",
                     "rmsea.pvalue",
                     "rmsea.pvalue.scaled")
    }
    if ("rmsea.robust" %in% names.x) {
      
      fit_measure$rootmse_approx <-
        baseline_fun(x,
                     "Robust RMSEA",
                     "",
                     "rmsea.robust")
    }
    if ("rmsea.ci.lower.robust" %in% names.x) {
      
      
      fit_measure$rootmse_approx <-
        baseline_fun(x,
                     "90 Percent Confidence Interval on RMSEA (Lower ROBUST)",
                     "",
                     "rmsea.ci.lower.robust")
      fit_measure$rootmse_approx <-
        baseline_fun(x,
                     "90 Percent Confidence Interval on RMSEA (Upper ROBUST)",
                     "",
                     "rmsea.ci.upper.robust")
    }
  }
  if (any(c("rmr", "srmr") %in% names.x)) {
    
    # Create empty df to append indicators which are true
    fit_measure$standardize_rmsqresidual <-
      data.frame(fit = character(),
                 value = numeric(),
                 value_scaled = numeric())
    
    
    baseline_fun <- function(x, f, s, t, the_df = fit_measure$standardize_rmsqresidual) {
      rbind(the_df, looper(f, x[s], x[t]))
    }
    
    
    #    cat("\nStandardized Root Mean Square Residual:\n\n")
    if ("rmr" %in% names.x) {
      
      fit_measure$standardize_rmsqresidual <-
        baseline_fun(x,
                     "RMR",
                     "rmr",
                     "rmr")
    }
    if ("rmr_nomean" %in% names.x) {
      
      fit_measure$standardize_rmsqresidual <-
        baseline_fun(x,
                     "RMR (No Mean)",
                     "rmr_nomean",
                     "rmr_nomean")
    }
    if ("srmr" %in% names.x) {
      
      fit_measure$standardize_rmsqresidual <-
        baseline_fun(x,
                     "SRMR",
                     "srmr",
                     "srmr")
    }
    if ("srmr_nomean" %in% names.x) {
      
      fit_measure$standardize_rmsqresidual <-
        baseline_fun(x,
                     "SRMR (No Mean)",
                     "srmr_nomean",
                     "srmr_nomean")
    }
  }
  if ("wrmr" %in% names.x) {
    # Create empty df to append indicators which are true
    fit_measure$weighted_rmsqresidual <-
      data.frame(fit = character(),
                 value = numeric(),
                 value_scaled = numeric())
    
    
    baseline_fun <- function(x, f, s, t, the_df = fit_measure$weighted_rmsqresidual) {
      rbind(the_df, looper(f, x[s], x[t]))
    }
    
    #    cat("\nWeighted Root Mean Square Residual:\n\n")
    if ("wrmr" %in% names.x) {
      
      fit_measure$weighted_rmsqresidual <-
        baseline_fun(x,
                     "WRMR",
                     "wrmr",
                     "wrmr")
    }
  }
  if (any(c("cn_05", "cn_01", "gfi", "agfi", "pgfi", "mfi") %in% names.x)) {
    
    # Create empty df to append indicators which are true
    fit_measure$other_fit_indices <-
      data.frame(fit = character(),
                 value = numeric(),
                 value_scaled = numeric())
    
    
    baseline_fun <- function(x, f, s, t, the_df = fit_measure$other_fit_indices) {
      rbind(the_df, looper(f, x[s], x[t]))
    }
    
    
    #    cat("\nOther Fit Indices:\n\n")
    if ("cn_05" %in% names.x) {
      fit_measure$other_fit_indices <-
        baseline_fun(x,
                     "Hoelter Critical N (CN) alpha=0.05",
                     "cn_05",
                     "cn_05")
    }
    if ("cn_01" %in% names.x) {
      fit_measure$other_fit_indices <-
        baseline_fun(x,
                     "Hoelter Critical N (CN) alpha=0.01",
                     "cn_01",
                     "cn_01")
    }
    
    if ("gfi" %in% names.x) {
      
      fit_measure$other_fit_indices <-
        baseline_fun(x,
                     "Goodness of Fit Index (GFI)",
                     "gfi",
                     "gfi")
    }
    
    if ("agfi" %in% names.x) {
      
      fit_measure$other_fit_indices <-
        baseline_fun(x,
                     "Adjusted Goodness of Fit Index (AGFI)",
                     "agfi",
                     "agfi")
    }
    if ("pgfi" %in% names.x) {
      
      fit_measure$other_fit_indices <-
        baseline_fun(x,
                     "Parsimony Goodness of Fit Index (PGFI)",
                     "pgfi",
                     "pgfi")
    }
    if ("mfi" %in% names.x) {
      
      fit_measure$other_fit_indices <-
        baseline_fun(x,
                     "McDonald Fit Index (MFI)",
                     "mfi",
                     "mfi")
    }
    if ("ecvi" %in% names.x) {
      
      fit_measure$other_fit_indices <-
        baseline_fun(x,
                     "Expected Cross-Validation Index (ECVI)",
                     "ecvi",
                     "ecvi")
    }
  }
  fit_measure
}

parameter_estimates <-
  function (object, se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
            level = 0.95, boot.ci.type = "perc", standardized = FALSE,
            fmi = FALSE, remove.system.eq = TRUE, remove.eq = TRUE,
            remove.ineq = TRUE, remove.def = FALSE, rsquare = FALSE,
            add.attributes = FALSE) {
    if ("lavaan.fsr" %in% class(object)) {
      return(object$PE)
    }
    
    if (class(object) != "lavaan") {
      if (missing(se) || !se) {
        se <- FALSE
        zstat <- FALSE
        pvalue <- FALSE
      }
    }
    
    if (fmi) {
      if (inherits(object, "lavaanList")) {
        warning("lavaan WARNING: fmi not available for object of class \"lavaanList\"")
        fmi <- FALSE
      }
      if (object@Options$se != "standard") {
        warning("lavaan WARNING: fmi only available if se = \"standard\"")
        fmi <- FALSE
      }
      if (object@Options$estimator != "ML") {
        warning("lavaan WARNING: fmi only available if estimator = \"ML\"")
        fmi <- FALSE
      }
      if (!object@SampleStats@missing.flag) {
        warning("lavaan WARNING: fmi only available if missing = \"(fi)ml\"")
        fmi <- FALSE
      }
      if (!object@optim$converged) {
        warning("lavaan WARNING: fmi not available; model did not converge")
        fmi <- FALSE
      }
    }
    if (object@Options$estimator == "Bayes") {
      zstat <- pvalue <- FALSE
    }
    PARTABLE <- as.data.frame(object@ParTable, stringsAsFactors = FALSE)
    LIST <- PARTABLE[, c("lhs", "op", "rhs")]
    if (!is.null(PARTABLE$user)) {
      LIST$user <- PARTABLE$user
    }
    if (!is.null(PARTABLE$block)) {
      LIST$block <- PARTABLE$block
    } else {
      LIST$block <- rep(1L, length(LIST$lhs))
    }
    if (!is.null(PARTABLE$level)) {
      LIST$level <- PARTABLE$level
    } else {
      LIST$level <- rep(1L, length(LIST$lhs))
    }
    if (!is.null(PARTABLE$group)) {
      LIST$group <- PARTABLE$group
    } else {
      LIST$group <- rep(1L, length(LIST$lhs))
    }
    if (!is.null(PARTABLE$label)) {
      LIST$label <- PARTABLE$label
    } else {
      LIST$label <- rep("", length(LIST$lhs))
    }
    if (!is.null(PARTABLE$exo)) {
      LIST$exo <- PARTABLE$exo
    } else {
      LIST$exo <- rep(0L, length(LIST$lhs))
    }
    if (inherits(object, "lavaanList")) {
      LIST$est <- NULL
    } else if (!is.null(PARTABLE$est)) {
      LIST$est <- PARTABLE$est
    } else {
      LIST$est <- lavaan:::lav_model_get_parameters(object@Model, type = "user",
                                                    extra = TRUE)
    }
    if (se && object@Options$se != "none") {
      LIST$se <- lavaan:::lav_object_inspect_se(object)
      tmp.se <- ifelse(LIST$se == 0, NA, LIST$se)
      if (zstat) {
        LIST$z <- LIST$est/tmp.se
        if (pvalue) {
          LIST$pvalue <- 2 * (1 - stats::pnorm(abs(LIST$z)))
        }
      }
    }
    BOOT <- lavaan:::lav_object_inspect_boot(object)
    bootstrap.successful <- NROW(BOOT)
    if (se && object@Options$se != "none" && ci) {
      a <- (1 - level)/2
      a <- c(a, 1 - a)
      if (object@Options$se != "bootstrap") {
        fac <- stats::qnorm(a)
        ci <- LIST$est + LIST$se %o% fac
      }
      else if (object@Options$se == "bootstrap") {
        norm.inter <- function(t, alpha) {
          t <- t[is.finite(t)]
          R <- length(t)
          rk <- (R + 1) * alpha
          if (!all(rk > 1 & rk < R))
            warning("extreme order statistics used as endpoints")
          k <- trunc(rk)
          inds <- seq_along(k)
          out <- inds
          kvs <- k[k > 0 & k < R]
          tstar <- sort(t, partial = sort(union(c(1, R),
                                                c(kvs, kvs + 1))))
          ints <- (k == rk)
          if (any(ints))
            out[inds[ints]] <- tstar[k[inds[ints]]]
          out[k == 0] <- tstar[1L]
          out[k == R] <- tstar[R]
          not <- function(v) xor(rep(TRUE, length(v)),
                                 v)
          temp <- inds[not(ints) & k != 0 & k != R]
          temp1 <- stats::qnorm(alpha[temp])
          temp2 <- stats::qnorm(k[temp]/(R + 1))
          temp3 <- stats::qnorm((k[temp] + 1)/(R + 1))
          tk <- tstar[k[temp]]
          tk1 <- tstar[k[temp] + 1L]
          out[temp] <- tk + (temp1 - temp2)/(temp3 - temp2) *
            (tk1 - tk)
          cbind(round(rk, 2), out)
        }
        stopifnot(!is.null(BOOT))
        stopifnot(boot.ci.type %in% c("norm", "basic", "perc",
                                      "bca.simple"))
        if (boot.ci.type == "norm") {
          fac <- stats::qnorm(a)
          boot.x <- colMeans(BOOT)
          boot.est <- lavaan:::lav_model_get_parameters(object@Model,
                                                        GLIST = lavaan:::lav_model_x2GLIST(object@Model, boot.x),
                                                        type = "user", extra = TRUE)
          bias.est <- (boot.est - LIST$est)
          ci <- (LIST$est - bias.est) + LIST$se %o% fac
        } else if (boot.ci.type == "basic") {
          ci <- cbind(LIST$est, LIST$est)
          alpha <- (1 + c(level, -level))/2
          qq <- apply(BOOT, 2, norm.inter, alpha)
          free.idx <- which(object@ParTable$free & !duplicated(object@ParTable$free))
          ci[free.idx, ] <- 2 * ci[free.idx, ] - t(qq[c(3,
                                                        4), ])
          def.idx <- which(object@ParTable$op == ":=")
          if (length(def.idx) > 0L) {
            BOOT.def <- apply(BOOT, 1, object@Model@def.function)
            if (length(def.idx) == 1L) {
              BOOT.def <- as.matrix(BOOT.def)
            }
            else {
              BOOT.def <- t(BOOT.def)
            }
            qq <- apply(BOOT.def, 2, norm.inter, alpha)
            ci[def.idx, ] <- 2 * ci[def.idx, ] - t(qq[c(3,
                                                        4), ])
          }
        } else if (boot.ci.type == "perc") {
          ci <- cbind(LIST$est, LIST$est)
          alpha <- (1 + c(-level, level))/2
          qq <- apply(BOOT, 2, norm.inter, alpha)
          free.idx <- which(object@ParTable$free & !duplicated(object@ParTable$free))
          ci[free.idx, ] <- t(qq[c(3, 4), ])
          def.idx <- which(object@ParTable$op == ":=")
          if (length(def.idx) > 0L) {
            BOOT.def <- apply(BOOT, 1, object@Model@def.function)
            if (length(def.idx) == 1L) {
              BOOT.def <- as.matrix(BOOT.def)
            }
            else {
              BOOT.def <- t(BOOT.def)
            }
            qq <- apply(BOOT.def, 2, norm.inter, alpha)
            def.idx <- which(object@ParTable$op == ":=")
            ci[def.idx, ] <- t(qq[c(3, 4), ])
          }
        } else if (boot.ci.type == "bca.simple") {
          alpha <- (1 + c(-level, level))/2
          zalpha <- stats::qnorm(alpha)
          ci <- cbind(LIST$est, LIST$est)
          free.idx <- which(object@ParTable$free & !duplicated(object@ParTable$free))
          x <- LIST$est[free.idx]
          for (i in 1:length(free.idx)) {
            t <- BOOT[, i]
            t <- t[is.finite(t)]
            t0 <- x[i]
            w <- stats::qnorm(sum(t < t0)/length(t))
            a <- 0
            adj.alpha <- stats::pnorm(w + (w + zalpha)/(1 - a *
                                                   (w + zalpha)))
            qq <- norm.inter(t, adj.alpha)
            ci[free.idx[i], ] <- qq[, 2]
          }
          def.idx <- which(object@ParTable$op == ":=")
          if (length(def.idx) > 0L) {
            x.def <- object@Model@def.function(x)
            BOOT.def <- apply(BOOT, 1, object@Model@def.function)
            if (length(def.idx) == 1L) {
              BOOT.def <- as.matrix(BOOT.def)
            }
            else {
              BOOT.def <- t(BOOT.def)
            }
            for (i in 1:length(def.idx)) {
              t <- BOOT.def[, i]
              t <- t[is.finite(t)]
              t0 <- x.def[i]
              w <- stats::qnorm(sum(t < t0)/length(t))
              a <- 0
              adj.alpha <- stats::pnorm(w + (w + zalpha)/(1 -
                                                     a * (w + zalpha)))
              qq <- norm.inter(t, adj.alpha)
              ci[def.idx[i], ] <- qq[, 2]
            }
          }
        }
      }
      LIST$ci.lower <- ci[, 1]
      LIST$ci.upper <- ci[, 2]
    }
    if (standardized) {
      LIST$std.lv <- lavaan:::lav_standardize_lv(object)
      LIST$std.all <- lavaan:::lav_standardize_all(object, est.std = LIST$est.std)
      LIST$std.nox <- lavaan:::lav_standardize_all_nox(object, est.std = LIST$est.std)
    }
    if (rsquare) {
      r2 <- lavaan:::lavTech(object, "rsquare", add.labels = TRUE)
      NAMES <- unlist(lapply(r2, names))
      nel <- length(NAMES)
      R2 <- data.frame(lhs = NAMES, op = rep("r2", nel), rhs = NAMES,
                       block = rep(1:length(r2), sapply(r2, length)), est = unlist(r2),
                       stringsAsFactors = FALSE)
      # I changed the `lavaan:::lavaan:::lav_partable_merge`
      # because it wasn't working. I replaced it by 
      # lavaan merge. Check the final part of the function
      # to see the changes.
      LIST <- lavaan_merge(pt1 = LIST, pt2 = R2, warn = FALSE)
    }
    if (fmi) {
      SE.orig <- LIST$se
      lavmodel <- object@Model
      implied <- object@implied
      COV <- if (lavmodel@conditional.x)
        implied$res.cov
      else implied$cov
      MEAN <- if (lavmodel@conditional.x)
        implied$res.int
      else implied$mean
      for (g in 1:object@Data@ngroups) rownames(COV[[g]]) <- object@Data@ov.names[[g]]
      if (object@Options$estimator == "ML" && object@Options$likelihood ==
          "normal") {
        for (g in 1:object@Data@ngroups) {
          N <- object@Data@nobs[[g]]
          COV[[g]] <- (N + 1)/N * COV[[g]]
        }
      }
      step2 <- lavaan::lavaan(slotOptions = object@Options, slotParTable = object@ParTable,
                              sample.cov = COV, sample.mean = MEAN, sample.nobs = object@Data@nobs)
      SE2 <- lavaan:::lav_object_inspect_se(step2)
      SE.step2 <- ifelse(SE2 == 0, as.numeric(NA), SE2)
      if (rsquare) {
        r2.idx <- which(LIST$op == "r2")
        if (length(r2.idx) > 0L) {
          SE.step2 <- c(SE.step2, rep(as.numeric(NA),
                                      length(r2.idx)))
        }
      }
      LIST$fmi <- 1 - (SE.step2 * SE.step2/(SE.orig * SE.orig))
    }
    if (object@Data@nlevels == 1L)
      LIST$level <- NULL
    if (object@Data@ngroups == 1L)
      LIST$group <- NULL
    if (object@Data@nlevels == 1L && object@Data@ngroups == 1L) {
      LIST$block <- NULL
    }
    if (sum(nchar(object@ParTable$label)) == 0L)
      LIST$label <- NULL
    if (remove.eq) {
      eq.idx <- which(LIST$op == "==" & LIST$user == 1L)
      if (length(eq.idx) > 0L) {
        LIST <- LIST[-eq.idx, ]
      }
    }
    if (remove.system.eq) {
      eq.idx <- which(LIST$op == "==" & LIST$user != 1L)
      if (length(eq.idx) > 0L) {
        LIST <- LIST[-eq.idx, ]
      }
    }
    if (remove.ineq) {
      ineq.idx <- which(LIST$op == "<" || LIST$op == ">")
      if (length(ineq.idx) > 0L) {
        LIST <- LIST[-ineq.idx, ]
      }
    }
    if (remove.def) {
      def.idx <- which(LIST$op == ":=")
      if (length(def.idx) > 0L) {
        LIST <- LIST[-def.idx, ]
      }
    }
    LIST$user <- NULL
    if (add.attributes) {
      class(LIST) <- c("lavaan.data.frame", "data.frame")
      attr(LIST, "information") <- object@Options$information
      attr(LIST, "se") <- object@Options$se
      attr(LIST, "group.label") <- object@Data@group.label
      attr(LIST, "level.label") <- object@Data@level.label
      attr(LIST, "bootstrap") <- object@Options$bootstrap
      attr(LIST, "bootstrap.successful") <- bootstrap.successful
      attr(LIST, "missing") <- object@Options$missing
      attr(LIST, "observed.information") <- object@Options$observed.information
      attr(LIST, "h1.information") <- object@Options$h1.information
    } else {
      LIST$exo <- NULL
      class(LIST) <- c("lavaan.data.frame", "data.frame")
    }
    LIST
  }

lavaan_merge <- function (pt1 = NULL, pt2 = NULL, remove.duplicated = FALSE, 
                          fromLast = FALSE, warn = TRUE) {
  pt1 <- as.data.frame(pt1, stringsAsFactors = FALSE)
  pt2 <- as.data.frame(pt2, stringsAsFactors = FALSE)
  stopifnot(!is.null(pt1$lhs), !is.null(pt1$op), !is.null(pt1$rhs), 
            !is.null(pt2$lhs), !is.null(pt2$op), !is.null(pt2$rhs))
  if (is.null(pt1$block) && is.null(pt2$block)) {
    pt1$block <- rep(1L, length(pt1$lhs))
    pt2$block <- rep(1L, length(pt2$lhs))
    TMP <- rbind(pt1[, c("lhs", "op", "rhs", "block")],
                 pt2[,c("lhs", "op", "rhs", "block")])
  } else {
    if (is.null(pt1$block) && !is.null(pt2$block)) {
      pt1$block <- rep(1L, length(pt1$lhs))
    } else if (is.null(pt2$block) && !is.null(pt1$block)) {
      pt2$block <- rep(1L, length(pt2$lhs))
    }
    TMP <- rbind(pt1[, c("lhs", "op", "rhs", "block")],
                 pt2[, c("lhs", "op", "rhs", "block")])
  }
  if (is.null(pt1$group) && !is.null(pt2$group)) {
    pt1$group <- rep(0L, length(pt1$lhs))
  } else if (is.null(pt2$group) && !is.null(pt1$group)) {
    pt2$group <- rep(0L, length(pt2$lhs))
  }
  if (is.null(pt1$level) && !is.null(pt2$level)) {
    pt1$level <- rep(0L, length(pt1$lhs))
  } else if (is.null(pt2$level) && !is.null(pt1$level)) {
    pt2$level <- rep(0L, length(pt2$lhs))
  }
  if (is.null(pt1$user) && !is.null(pt2$user)) {
    pt1$user <- rep(0L, length(pt1$lhs))
  } else if (is.null(pt2$user) && !is.null(pt1$user)) {
    pt2$user <- rep(0L, length(pt2$lhs))
  }
  if (is.null(pt1$free) && !is.null(pt2$free)) {
    pt1$free <- rep(0L, length(pt1$lhs))
  } else if (is.null(pt2$free) && !is.null(pt1$free)) {
    pt2$free <- rep(0L, length(pt2$lhs))
  }
  if (is.null(pt1$ustart) && !is.null(pt2$ustart)) {
    pt1$ustart <- rep(0, length(pt1$lhs))
  } else if (is.null(pt2$ustart) && !is.null(pt1$ustart)) {
    pt2$ustart <- rep(0, length(pt2$lhs))
  }
  if (is.null(pt1$exo) && !is.null(pt2$exo)) {
    pt1$exo <- rep(0L, length(pt1$lhs))
  } else if (is.null(pt2$exo) && !is.null(pt1$exo)) {
    pt2$exo <- rep(0L, length(pt2$lhs))
  }
  if (is.null(pt1$label) && !is.null(pt2$label)) {
    pt1$label <- rep("", length(pt1$lhs))
  } else if (is.null(pt2$label) && !is.null(pt1$label)) {
    pt2$label <- rep("", length(pt2$lhs))
  }
  if (is.null(pt1$plabel) && !is.null(pt2$plabel)) {
    pt1$plabel <- rep("", length(pt1$lhs))
  } else if (is.null(pt2$plabel) && !is.null(pt1$plabel)) {
    pt2$plabel <- rep("", length(pt2$lhs))
  }
  if (is.null(pt1$start) && !is.null(pt2$start)) {
    pt1$start <- rep(as.numeric(NA), length(pt1$lhs))
  } else if (is.null(pt2$start) && !is.null(pt1$start)) {
    pt2$start <- rep(as.numeric(NA), length(pt2$lhs))
  }
  if (is.null(pt1$est) && !is.null(pt2$est)) {
    pt1$est <- rep(0, length(pt1$lhs))
  } else if (is.null(pt2$est) && !is.null(pt1$est)) {
    pt2$est <- rep(0, length(pt2$lhs))
  }
  if (remove.duplicated) {
    idx <- which(duplicated(TMP, fromLast = fromLast))
    if (length(idx)) {
      if (warn) {
        warning("lavaan WARNING: duplicated parameters are ignored:\n", 
                paste(apply(pt1[idx, c("lhs", "op", "rhs")], 
                            1, paste, collapse = " "), collapse = "\n"))
      }
      if (fromLast) {
        pt1 <- pt1[-idx, ]
      }
      else {
        idx <- idx - nrow(pt1)
        pt2 <- pt2[-idx, ]
      }
    }
  } else if (!is.null(pt1$start) && !is.null(pt2$start)) {
    for (i in 1:length(pt1$lhs)) {
      idx <- which(pt2$lhs == pt1$lhs[i] & pt2$op == pt1$op[i] & 
                     pt2$rhs == pt1$rhs[i] & pt2$block == pt1$block[i])
      pt2$start[idx] <- pt1$start[i]
    }
  }
  
  if (is.null(pt1$id) && !is.null(pt2$id)) {
    nid <- max(pt2$id)
    pt1$id <- (nid + 1L):(nid + nrow(pt1))
  } else if (is.null(pt2$id) && !is.null(pt1$id)) {
    nid <- max(pt1$id)
    pt2$id <- (nid + 1L):(nid + nrow(pt2))
  }
  
  # We need to merge pt1 and the r2 dataset (pt2)
  # but the traditional way was merging on all variables
  # including some empty variables and `est` (which is the actual
  # r2).
  
  # rename to r2
  pt2$r2 <- pt2$est
  pt2$est <- NULL
  # selected only variables for merging
  pt2 <- pt2[, c("lhs", "rhs", "block", "r2")]
  # merge
  NEW <- base::merge(pt1, pt2, all = TRUE, sort = FALSE)
  NEW
}


### Utils for group_qqscore

# Just vectorize the previous so that it runs through all
# possible latent variables
vec_subsetting_lat <- function(x, y) {
  as.data.frame(lapply(x, subsetting_latent, group_data = y))
}

# If a single latent variable df is supplied
# with standardized coefficients and the
# country data with original variables, it returns the quality
# of sumscore of both latent variables
subsetting_latent <- function(latent_df, group_data) {
  lambda <- latent_df$std.all
  
  var_data <-
    vapply(latent_df$rhs,
           function(x) stats::var(group_data[x]), FUN.VALUE = numeric(1))
  
  variance_cs <- stats::var(rowSums(group_data[latent_df$rhs]))
  
  q_sscore(lambda, var_data, variance_cs)
}

# Extracts the standardized coefficients
# from each group
country_latent <- function(df) {
  cnt_stdest <- df[df$op == "=~", c("lhs", "rhs", "std.all")]
  cnt_sep_latent <- split(cnt_stdest, cnt_stdest$lhs)
  cnt_sep_latent
}

# Turns a matrix given by lavaan to a
# dataframe w/ column names
todf <- function(dat, some_names) {
  dt <- as.data.frame(dat)
  names(dt) <- some_names
  dt
}

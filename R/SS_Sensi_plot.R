#' Create relative sensitivity plots as described in Cope and Gertseva (2020)
#'
#' Uses output from [SSsummarize()] to make a figure showing
#' sensitivity of various quantities of interest.
#'
#' @param model.summaries Output from [SSsummarize()]
#' summarizing results of models to be included
#' @template dir
#' @param current.year Year to report output
#' @param mod.names List the names of the sensitivity runs
#' @param Sensi.RE.out  Saved file of relative changes
#' @param CI Confidence interval box based on the reference model
#' @param TRP.in Target relative abundance value
#' @param LRP.in Limit relative abundance value
#' @param sensi_xlab X-axis label
#' @param ylims.in Y-axis label
#' @param plot.figs Which plots to make/save?
#' @param sensi.type.breaks vertical breaks that can separate out types
#' of sensitivities
#' @param anno.x Horizontal positioning of the sensitivity types labels
#' @param anno.y Vertical positioning of the sensitivity types labels
#' @param anno.lab Sensitivity types labels
#' @param spawn.lab Label for spawning output or spawning biomass. By default
#' it will be set to "SO" if any model has spawning output in numbers and "SB"
#' if all models have spawning output in biomass. Subscripts will be added
#' for 0 or current year.
#' @param yield.lab Label for yield reference point.
#' By default it will be set to something like "Yield(SPR=0.3)" where the SPR
#' value is the SPR target. If the models have different SPR targets, it will
#' be set to "Yield(tgt SPR)".
#' @param F.lab Label for F reference point.
#' By default it will be set to something like "F(SPR=0.3)" where the SPR
#' value is the SPR target. If the models have different SPR targets, it will
#' be set to "F(tgt SPR)".
#'
#' @author Jason Cope
#' @export
#' @seealso [SSsummarize()]
#' @references Cope, J. and Gertseva, V. 2020. A new way to visualize
#' and report structural and data uncertainty in stock assessments.
#' Can. J. Fish. Aquat. Sci. 77:1275-1280.
#' https://doi.org/10.1139/cjfas-2020-0082

#' @examples
#' \dontrun{
#' # Set directory and extract ouput from models
#' # Model 1 needs to be the Reference model, with sensitivity runs following
#' # from run 2 on.
#'
#' # Note: models are available in Jason Cope's github repository:
#' # https://github.com/shcaba/Stock-Assessment-Sensitivity-Plots/
#' dir <-
#'   "C:/Users/.../GitHub/Stock-Assessment-Sensitivity-Plots/Sensitivity_runs/"
#' models.dirs <- paste0("Cab_SCS_MS_", 1:19)
#' zz <- SSgetoutput(dirvec = file.path(dir, models.dirs))
#'
#' # Use the summarize function in r4ss to get model summaries
#' model.summaries <- SSsummarize(zz)
#'
#' # Define the names of each model. This will be used to label runs in the
#' # table and in the figures.
#' mod.names <- c(
#'   "Reference",
#'   "M: Fix to 2009",
#'   "M: Fix to prior",
#'   "M: Fix to Hamel",
#'   "M: Fix to VBGF",
#'   "M: Fix to OR",
#'   "VBGF 2009",
#'   "VBGF Grebel",
#'   "OR maturity",
#'   "Est. h",
#'   "All rec devs",
#'   "No rec devs",
#'   "High bias adj.",
#'   "Harmonic mean",
#'   "Dirichlet",
#'   "Wts = 1",
#'   "No blocks",
#'   "First blocks in 2000",
#'   "Alt rec catches"
#' )
#'
#' # Run the sensitivity plot function
#' SS_Sensi_plot(
#'   model.summaries = model.summaries,
#'   dir = dir,
#'   current.year = 2019,
#'   mod.names = mod.names, # List the names of the sensitivity runs
#'   likelihood.out = c(1, 1, 0),
#'   Sensi.RE.out = "Sensi_RE_out.DMP", # Saved file of relative errors
#'   CI = 0.95, # Confidence interval box based on the reference model
#'   TRP.in = 0.4, # Target relative abundance value
#'   LRP.in = 0.25, # Limit relative abundance value
#'   sensi_xlab = "Sensitivity scenarios", # X-axis label
#'   ylims.in = c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1), # Y-axis label
#'   plot.figs = c(1, 1, 1, 1, 1, 1), # Which plots to make/save?
#'   sensi.type.breaks = c(6.5, 9.5, 13.5, 16.5), # vertical breaks
#'   anno.x = c(3.75, 8, 11.5, 15, 18), # positioning of types labels
#'   anno.y = c(1, 1, 1, 1, 1), # positioning of types labels
#'   anno.lab = c(
#'     "Natural mortality", "VBGF/Mat.", "Recruitment", "Data Wts.",
#'     "Other"
#'   ) # Sensitivity types labels
#' )
#' }
#'
SS_Sensi_plot <- function(model.summaries,
                          dir = "",
                          current.year,
                          mod.names,
                          # likelihood.out = c(1, 1, 1), # note which likelihoods are in the model
                          Sensi.RE.out = "Sensi_RE_out.DMP",
                          CI = 0.95,
                          TRP.in = 0.4,
                          LRP.in = 0.25,
                          sensi_xlab = "Sensitivity scenarios",
                          ylims.in = c(-1, 2, -1, 2, -1, 2, -1, 2, -1, 2, -1, 2),
                          plot.figs = c(1, 1, 1, 1, 1, 1),
                          sensi.type.breaks = NA,
                          anno.x = NA,
                          anno.y = NA,
                          anno.lab = NA,
                          spawn.lab = NA,
                          yield.lab = NA,
                          F.lab = NA) {
  # internal function
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
  }

  # num.likes<-sum(likelihood.out)*2+2
  num.likes <- length(unique(model.summaries[["likelihoods_by_fleet"]][["Label"]])) # determine how many likelihoods components
  nummods <- as.data.frame(matrix(NA, ncol(model.summaries[["likelihoods_by_fleet"]]) - 2, length(unique(model.summaries[["likelihoods_by_fleet"]][["model"]])) + 2))
  nummods[, 1] <- colnames(model.summaries[["likelihoods_by_fleet"]])[-c(1, 2)]
  colnames(nummods) <- c("Type", mod.names, "Label")
  survey.lambda <- survey.like <- Lt.lambda <- Lt.like <- Age.lambda <- Age.like <- nummods

  if (missing(mod.names)) {
    mod.names <- paste("model ", 1:model.summaries[["n"]])
  }
  if (any(unique(model.summaries[["likelihoods_by_fleet"]][["Label"]]) == "Surv_lambda")) {
    survey.lb <- model.summaries[["likelihoods_by_fleet"]][model.summaries[["likelihoods_by_fleet"]][["Label"]] == "Surv_lambda", ]
    survey.lambda[, survey.lb[["model"]] + 1] <- t(survey.lb[, 3:ncol(survey.lb)])
    survey.lambda[["Label"]] <- "Surv_lambda"

    survey.lk <- model.summaries[["likelihoods_by_fleet"]][model.summaries[["likelihoods_by_fleet"]][["Label"]] == "Surv_like", ]
    survey.like[, survey.lk[["model"]] + 1] <- t(survey.lk[, 3:ncol(survey.lk)])
    survey.like[["Label"]] <- "Survey_likelihood"

    #    syrvlambda_index <- c(1:num.likes)[subset(model.summaries[["likelihoods_by_fleet"]], model == 1)$Label == "Surv_lambda"]
    #    survey.lambda <- data.frame(rownames(t(model.summaries[["likelihoods_by_fleet"]]))[-1:-2], t(model.summaries[["likelihoods_by_fleet"]][seq(3, dim(model.summaries[["likelihoods_by_fleet"]])[1], num.likes), ][-1:-2]), "Survey_lambda")
    #    syrvlike_index <- c(1:num.likes)[subset(model.summaries[["likelihoods_by_fleet"]], model == 1)$Label == "Surv_like"]
    #    survey.like <- data.frame(rownames(t(model.summaries[["likelihoods_by_fleet"]]))[-1:-2], t(model.summaries[["likelihoods_by_fleet"]][seq(syrvlike_index, dim(model.summaries[["likelihoods_by_fleet"]])[1], num.likes), ][-1:-2]), "Survey_likelihood")
  } else {
    survey.lambda <- survey.like <- data.frame(t(rep(NA, model.summaries[["n"]] + 2)))
  }
  if (any(unique(model.summaries[["likelihoods_by_fleet"]][["Label"]]) == "Length_lambda")) {
    Lt.lb <- model.summaries[["likelihoods_by_fleet"]][model.summaries[["likelihoods_by_fleet"]][["Label"]] == "Length_lambda", ]
    Lt.lambda[, Lt.lb[["model"]] + 1] <- t(Lt.lb[, 3:ncol(Lt.lb)])
    Lt.lambda[["Label"]] <- "Length_lambda"

    Lt.lk <- model.summaries[["likelihoods_by_fleet"]][model.summaries[["likelihoods_by_fleet"]][["Label"]] == "Length_like", ]
    Lt.like[, Lt.lk[["model"]] + 1] <- t(Lt.lk[, 3:ncol(Lt.lk)])
    Lt.like[["Label"]] <- "Length_likelihood"
  } else {
    Lt.lambda <- Lt.like <- data.frame(t(rep(NA, model.summaries[["n"]] + 2)))
  }
  if (any(unique(model.summaries[["likelihoods_by_fleet"]][["Label"]]) == "Age_lambda")) {
    Age.lb <- model.summaries[["likelihoods_by_fleet"]][model.summaries[["likelihoods_by_fleet"]][["Label"]] == "Age_lambda", ]
    Age.lambda[, Age.lb[["model"]] + 1] <- t(Age.lb[, 3:ncol(Age.lb)])
    Age.lambda[["Label"]] <- "Age_lambda"

    Age.lk <- model.summaries[["likelihoods_by_fleet"]][model.summaries[["likelihoods_by_fleet"]][["Label"]] == "Age_like", ]
    Age.like[, Age.lk[["model"]] + 1] <- t(Age.lk[, 3:ncol(Age.lk)])
    Age.like[["Label"]] <- "Age_likelihood"
  } else {
    Age.lambda <- Age.like <- data.frame(t(rep(NA, model.summaries[["n"]] + 2)))
  }

  parms <- model.summaries[["pars"]]
  # rownames(parms)<-parms[["Label"]]
  parms <- data.frame(parms[["Label"]], parms[, 1:(dim(parms)[2] - 3)], "Parameters")
  if (any(model.summaries[["nsexes"]] == 1)) {
    dev.quants <- rbind(
      model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] == "SSB_Virgin", 1:(dim(model.summaries[["quants"]])[2] - 2)] / 2,
      (model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] == paste0("SSB_", current.year), 1:(dim(model.summaries[["quants"]])[2] - 2)]) / 2,
      model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] == paste0("Bratio_", current.year), 1:(dim(model.summaries[["quants"]])[2] - 2)],
      model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] == "Dead_Catch_SPR", 1:(dim(model.summaries[["quants"]])[2] - 2)] / 2,
      model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] %in% c("Fstd_SPR", "annF_SPR"), 1:(dim(model.summaries[["quants"]])[2] - 2)]
    )
    # Extract SDs for use in the ggplots
    dev.quants.SD <- c(
      model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] == "SSB_Virgin", 1] / 2,
      (model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] == paste0("SSB_", current.year), 1]) / 2,
      model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] == paste0("Bratio_", current.year), 1],
      model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] == "Dead_Catch_SPR", 1] / 2,
      model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] %in% c("Fstd_SPR", "annF_SPR"), 1]
    )
  }
  if (any(model.summaries[["nsexes"]] == 2)) {
    dev.quants <- rbind(
      model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] == "SSB_Virgin", 1:(dim(model.summaries[["quants"]])[2] - 2)],
      model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] == paste0("SSB_", current.year), 1:(dim(model.summaries[["quants"]])[2] - 2)],
      model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] == paste0("Bratio_", current.year), 1:(dim(model.summaries[["quants"]])[2] - 2)],
      model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] == "Dead_Catch_SPR", 1:(dim(model.summaries[["quants"]])[2] - 2)],
      model.summaries[["quants"]][model.summaries[["quants"]][["Label"]] %in% c("Fstd_SPR", "annF_SPR"), 1:(dim(model.summaries[["quants"]])[2] - 2)]
    )
    # Extract SDs for use in the ggplots
    dev.quants.SD <- c(
      model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] == "SSB_Virgin", 1],
      (model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] == paste0("SSB_", current.year), 1]),
      model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] == paste0("Bratio_", current.year), 1],
      model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] == "Dead_Catch_SPR", 1],
      model.summaries[["quantsSD"]][model.summaries[["quantsSD"]][["Label"]] %in% c("Fstd_SPR", "annF_SPR"), 1]
    )
  }

  dev.quants.labs <- data.frame(c("SB0", paste0("SSB_", current.year), paste0("Bratio_", current.year), "MSY_SPR", "F_SPR"), dev.quants, "Derived quantities")
  AICs <- 2 * model.summaries[["npars"]] + (2 * as.numeric(model.summaries[["likelihoods"]][1, 1:model.summaries[["n"]]]))
  deltaAICs <- AICs - AICs[1]
  AIC.out <- data.frame(cbind(c("AIC", "deltaAIC"), rbind.data.frame(AICs, deltaAICs), c("AIC")))
  colnames(AIC.out) <- colnames(survey.lambda) <- colnames(survey.like) <- colnames(Lt.lambda) <- colnames(Lt.like) <- colnames(Age.lambda) <- colnames(Age.like) <- colnames(parms) <- colnames(dev.quants.labs) <- c("Type", mod.names, "Label")
  Like.parm.quants <- rbind(AIC.out, survey.like, survey.lambda, Lt.like, Lt.lambda, Age.like, Age.lambda, parms, dev.quants.labs)
  Like.parm.quants.table.data <- flextable::as_grouped_data(Like.parm.quants, groups = c("Label"))
  # as_flextable(Like.parm.quants.table.data)
  write.csv(
    Like.parm.quants.table.data,
    file.path(dir, "Likes_parms_devquants_table.csv")
  )

  # Calcualte Relative changes
  dev.quants.mat <- as.matrix(dev.quants)
  colnames(dev.quants.mat) <- 1:dim(dev.quants.mat)[2]
  rownames(dev.quants.mat) <- c("SB0", paste0("SSB_", current.year), paste0("Bratio_", current.year), "MSY_SPR", "F_SPR")
  # RE<-reshape2::melt((as.matrix(dev.quants)-as.matrix(dev.quants)[,1])/as.matrix(dev.quants)[,1])
  RE <- reshape2::melt((dev.quants.mat - dev.quants.mat[, 1]) / dev.quants.mat[, 1])[-1:-5, ]
  logRE <- reshape2::melt(log(dev.quants.mat / dev.quants.mat[, 1]))[-1:-5, ]
  # Get values for plots
  Dev.quants.temp <- as.data.frame(cbind(rownames(dev.quants.mat), dev.quants.mat[, -1]))
  colnames(Dev.quants.temp) <- c("Metric", mod.names[-1])
  Dev.quants.ggplot <- data.frame(reshape2::melt(Dev.quants.temp, id.vars = c("Metric")), RE[, 2:3], logRE[, 2:3])
  colnames(Dev.quants.ggplot) <- c("Metric", "Model_name", "Value", "Model_num_plot", "RE", "Model_num_plot_log", "logRE")
  Dev.quants.ggplot[["Metric"]] <- factor(Dev.quants.ggplot[["Metric"]], levels = unique(Dev.quants.ggplot[["Metric"]]))
  save(Dev.quants.ggplot, file = file.path(dir, Sensi.RE.out))

  # Calculate RE values for reference model boxes
  CI_DQs_RE <- ((dev.quants[, 1] + dev.quants.SD * qnorm(CI)) - dev.quants[, 1]) / dev.quants[, 1]
  TRP <- (TRP.in - dev.quants[3, 1]) / dev.quants[3, 1]
  LRP <- (LRP.in - dev.quants[3, 1]) / dev.quants[3, 1]

  logCI_DQs_RE <- log((dev.quants[, 1] + dev.quants.SD * qnorm(CI)) / dev.quants[, 1])
  logTRP <- log(TRP.in / dev.quants[3, 1])
  logLRP <- log(LRP.in / dev.quants[3, 1])

  # Plot Relative changes
  four.colors <- gg_color_hue(5)
  lty.in <- 2
  if (any(is.na(sensi.type.breaks))) {
    lty.in <- 0
    sensi.type.breaks <- c(1, 1)
  }
  if (any(is.na(anno.x))) {
    anno.x <- c(1, 1)
  }
  if (any(is.na(anno.y))) {
    anno.y <- c(1, 1)
  }

  if (any(is.na(anno.lab))) {
    anno.lab <- c("", "")
  }
  if (is.na(spawn.lab)) {
    spawn.lab <- ifelse(all(model.summaries[["SpawnOutputUnits"]] == "biomass"),
      "SB", "SO"
    )
  }
  # add subscripts to spawning label (e.g. SB0)
  spawn.lab.0 <- as.expression(bquote(.(spawn.lab)[0]))
  spawn.lab.curr <- as.expression(bquote(.(spawn.lab)[.(current.year)]))
  spawn.lab.ratio <- as.expression(bquote(frac(.(spawn.lab)[.(current.year)], .(spawn.lab)[.(0)])))

  if (is.na(yield.lab)) {
    sprtarg <- model.summaries[["sprtargs"]][1]
    yield.lab <- ifelse(test = all(model.summaries[["sprtargs"]] == sprtarg),
      yes = paste0("Yield(SPR=", sprtarg, ")"),
      no = "Yield(tgt SPR)"
    )
  }
  if (is.na(F.lab)) {
    sprtarg <- model.summaries[["sprtargs"]][1]
    F.lab <- ifelse(test = all(model.summaries[["sprtargs"]] == sprtarg),
      yes = paste0("F(SPR=", sprtarg, ")"),
      no = "F(tgt SPR)"
    )
  }

  # Begin plots
  pt.dodge <- 0.3
  if (plot.figs[1] == 1) {
    # RE plot
    ggplot(Dev.quants.ggplot, aes(.data[["Model_num_plot"]], RE)) +
      geom_point(aes(shape = .data[["Metric"]], color = .data[["Metric"]]), position = position_dodge(pt.dodge)) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[1], ymax = CI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[2], ymax = CI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[3], ymax = CI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[4], ymax = CI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[5], ymax = CI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
      geom_hline(yintercept = c(TRP, LRP, 0), lty = c(2, 2, 1), color = c("darkgreen", "darkred", "gray")) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[1:2])+
      coord_cartesian(ylim = ylims.in[1:2]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.text.align = 0,
        panel.grid.minor = element_blank()
      ) +
      scale_shape_manual(
        values = c(15:18, 12),
        name = "",
        labels = c(
          spawn.lab.0,
          spawn.lab.curr,
          spawn.lab.ratio,
          yield.lab,
          F.lab
        )
      ) +
      scale_color_manual(
        values = four.colors[1:5],
        name = "",
        labels = c(
          spawn.lab.0,
          spawn.lab.curr,
          spawn.lab.ratio,
          yield.lab,
          F.lab
        )
      ) +
      labs(x = sensi_xlab, y = "Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      annotate("text", x = c((model.summaries[["n"]] + 2), (model.summaries[["n"]] + 2)), y = c(TRP + 0.03, LRP - 0.03), label = c("TRP", "LRP"), linewidth = c(3, 3), color = c("darkgreen", "darkred")) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_REplot_all.png"))

    # log plot
    ggplot(Dev.quants.ggplot, aes(.data[["Model_num_plot"]], logRE)) +
      geom_point(aes(shape = .data[["Metric"]], color = .data[["Metric"]]), position = position_dodge(pt.dodge)) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[1], ymax = logCI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[2], ymax = logCI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[3], ymax = logCI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[4], ymax = logCI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[5], ymax = logCI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
      geom_hline(yintercept = c(logTRP, logLRP, 0), lty = c(2, 2, 1), color = c("darkgreen", "darkred", "gray")) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[1:2])+
      coord_cartesian(ylim = ylims.in[1:2]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.text.align = 0, panel.grid.minor = element_blank()) +
      scale_shape_manual(
        values = c(15:18, 12),
        name = "",
        labels = c(
          spawn.lab.0,
          spawn.lab.curr,
          spawn.lab.ratio,
          yield.lab,
          F.lab
        )
      ) +
      scale_color_manual(
        values = four.colors[1:5],
        name = "",
        labels = c(
          spawn.lab.0,
          spawn.lab.curr,
          spawn.lab.ratio,
          yield.lab,
          F.lab
        )
      ) +
      labs(x = sensi_xlab, y = "Log relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      annotate("text", x = c((model.summaries[["n"]] + 2), (model.summaries[["n"]] + 2)), y = c(logTRP + 0.03, logLRP - 0.03), label = c("TRP", "LRP"), linewidth = c(3, 3), color = c("darkgreen", "darkred")) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_logREplot_all.png"))
  }

  if (plot.figs[1] == 1) {
    # RE plots
    Dev.quants.ggplot.SBs <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[1] |
        Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[2],
    ]
    p1 <- ggplot(Dev.quants.ggplot.SBs, aes(.data[["Model_num_plot"]], RE)) +
      geom_point(aes(shape = .data[["Metric"]], color = .data[["Metric"]]), position = position_dodge(pt.dodge)) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[1], ymax = CI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[2], ymax = CI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]])) +
      coord_cartesian(ylim = ylims.in[1:2]) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_shape_manual(
        values = c(16, 17),
        name = "",
        labels = c(
          spawn.lab.0,
          spawn.lab.curr
        )
      ) +
      scale_color_manual(
        values = four.colors[1:2],
        name = "",
        labels = c(
          spawn.lab.0,
          spawn.lab.curr
        )
      ) +
      theme(legend.text.align = 0) +
      labs(x = " ", y = " ") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_hline(yintercept = 0, lwd = 0.5, color = "gray") +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    Dev.quants.ggplot.Dep <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[3],
    ]
    p2 <- ggplot(Dev.quants.ggplot.Dep, aes(.data[["Model_num_plot"]], RE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[3], ymax = CI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]])) +
      # scale_y_continuous(limits=ylims.in[7:8])+
      coord_cartesian(ylim = ylims.in[7:8]) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(legend.text.align = 0) +
      labs(x = " ", y = "Relative change") +
      scale_colour_manual(
        values = four.colors[3],
        name = "",
        labels = spawn.lab.ratio
      ) +
      annotate("text", x = c((model.summaries[["n"]] + 1), (model.summaries[["n"]] + 1)), y = c(TRP + 0.1, LRP - 0.1), label = c("TRP", "LRP"), linewidth = c(3, 3), color = c("darkgreen", "darkred")) +
      geom_hline(yintercept = c(TRP, LRP, 0), lty = c(3, 3, 1), lwd = c(0.5, 0.5, 0.5), color = c("darkgreen", "darkred", "gray")) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)

    Dev.quants.ggplot.MSY_FMSY <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[4] |
        Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[5],
    ]

    p3 <- ggplot(Dev.quants.ggplot.MSY_FMSY, aes(.data[["Model_num_plot"]], RE, group = .data[["Metric"]])) +
      geom_point(aes(shape = .data[["Metric"]], color = .data[["Metric"]]), position = position_dodge(pt.dodge)) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[4], ymax = CI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[5], ymax = CI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[9:10])+
      coord_cartesian(ylim = ylims.in[9:10]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.text.align = 0,
        panel.grid.minor = element_blank()
      ) +
      #          legend.text=element_text(size=rel(1)))+
      scale_shape_manual(
        values = c(16, 17),
        name = "",
        labels = c(yield.lab, F.lab)
      ) +
      scale_color_manual(
        values = four.colors[4:5],
        name = "",
        labels = c(yield.lab, F.lab)
      ) +
      labs(x = sensi_xlab, y = "") +
      guides(fill = "none") +
      # annotate("text",x=anno.x,y=anno.y,label=anno.lab)+
      geom_hline(yintercept = 0, lwd = 0.5, color = "gray") +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)

    # p4<-grid.arrange(p1,p2,p3,heights=c(5,5,8))
    p4 <- ggpubr::ggarrange(p1, p2, p3, nrow = 3, ncol = 1, align = "v", heights = c(5, 5, 8))
    ggsave(file.path(dir, "Sensi_REplot_SB_Dep_F_MSY.png"), p4)

    # Log plots
    Dev.quants.ggplot.SBs <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[1] |
        Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[2],
    ]

    p1 <- ggplot(Dev.quants.ggplot.SBs, aes(.data[["Model_num_plot"]], logRE)) +
      geom_point(aes(shape = .data[["Metric"]], color = .data[["Metric"]]), position = position_dodge(pt.dodge)) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[1], ymax = logCI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[2], ymax = logCI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]])) +
      # scale_y_continuous(limits=ylims.in[1:2])+
      coord_cartesian(ylim = ylims.in[1:2]) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_shape_manual(
        values = c(16, 17),
        name = "",
        labels = c(
          spawn.lab.0,
          spawn.lab.curr
        )
      ) +
      scale_color_manual(
        values = four.colors[1:2],
        name = "",
        labels = c(
          spawn.lab.0,
          spawn.lab.curr
        )
      ) +
      theme(legend.text.align = 0) +
      labs(x = " ", y = " ") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_hline(yintercept = 0, lwd = 0.5, color = "gray") +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)

    Dev.quants.ggplot.Dep <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[3],
    ]
    p2 <- ggplot(Dev.quants.ggplot.Dep, aes(.data[["Model_num_plot"]], logRE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[3], ymax = logCI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]])) +
      # scale_y_continuous(limits=ylims.in[7:8])+
      coord_cartesian(ylim = ylims.in[7:8]) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(legend.text.align = 0) +
      labs(x = " ", y = "Log relative change") +
      scale_colour_manual(
        values = four.colors[3],
        name = "",
        labels = spawn.lab.ratio
      ) +
      annotate("text", x = c((model.summaries[["n"]] + 1), (model.summaries[["n"]] + 1)), y = c(logTRP + 0.08, logLRP - 0.08), label = c("TRP", "LRP"), linewidth = c(3, 3), color = c("darkgreen", "darkred")) +
      geom_hline(yintercept = c(logTRP, logLRP, 0), lty = c(3, 3, 1), lwd = c(0.5, 0.5, 0.5), color = c("darkgreen", "darkred", "gray")) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)

    Dev.quants.ggplot.MSY_FMSY <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[4] |
        Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[5],
    ]

    p3 <- ggplot(Dev.quants.ggplot.MSY_FMSY, aes(.data[["Model_num_plot"]], logRE, group = .data[["Metric"]])) +
      geom_point(aes(shape = .data[["Metric"]], color = .data[["Metric"]]), position = position_dodge(pt.dodge)) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[4], ymax = logCI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[5], ymax = logCI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[9:10])+
      coord_cartesian(ylim = ylims.in[9:10]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.text.align = 0,
        panel.grid.minor = element_blank()
      ) +
      #          legend.text=element_text(size=7.5))+
      scale_shape_manual(
        values = c(16, 17),
        name = "",
        labels = c(yield.lab, F.lab)
      ) +
      scale_color_manual(
        values = four.colors[4:5],
        name = "",
        labels = c(yield.lab, F.lab)
      ) +
      labs(x = sensi_xlab, y = "") +
      guides(fill = "none") +
      # annotate("text",x=anno.x,y=anno.y,label=anno.lab)+
      geom_hline(yintercept = 0, lwd = 0.5, color = "gray") +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)

    p4 <- ggpubr::ggarrange(p1, p2, p3, nrow = 3, ncol = 1, align = "v", heights = c(5, 5, 8))
    # p4<-grid.arrange(p1,p2,p3,heights=c(5,5,8))
    ggsave(file.path(dir, "Sensi_logREplot_SB_Dep_F_MSY.png"), p4)
  }

  if (plot.figs[2] == 1) {
    # RE plot
    Dev.quants.ggplot.SB0 <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[1],
    ]
    ggplot(Dev.quants.ggplot.SB0, aes(.data[["Model_num_plot"]], RE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[1], ymax = CI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
      geom_hline(yintercept = 0, lty = 1, color = "gray") +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot.SB0[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[3:4])+
      coord_cartesian(ylim = ylims.in[3:4]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.text.align = 0,
        panel.grid.minor = element_blank()
      ) +
      scale_colour_manual(
        values = four.colors[1],
        name = "",
        labels = spawn.lab.0
      ) +
      labs(x = sensi_xlab, y = "Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_REplot_SO_0.png"))

    # Log plot
    Dev.quants.ggplot.SB0 <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[1],
    ]
    ggplot(Dev.quants.ggplot.SB0, aes(.data[["Model_num_plot"]], logRE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[1], ymax = logCI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
      geom_hline(yintercept = 0, lty = 1, color = "gray") +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot.SB0[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[3:4])+
      coord_cartesian(ylim = ylims.in[3:4]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.text.align = 0,
        panel.grid.minor = element_blank()
      ) +
      scale_colour_manual(
        values = four.colors[1],
        name = "",
        labels = spawn.lab.0
      ) +
      labs(x = sensi_xlab, y = "Log Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_logREplot_SO_0.png"))
  }

  if (plot.figs[3] == 1) {
    # RE plots
    Dev.quants.ggplot.SBt <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[2],
    ]
    ggplot(Dev.quants.ggplot.SBt, aes(.data[["Model_num_plot"]], RE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[2], ymax = CI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
      geom_hline(yintercept = 0, lty = 1, color = "gray") +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), minor_breaks = NULL, labels = unique(Dev.quants.ggplot.SBt[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[5:6])+
      coord_cartesian(ylim = ylims.in[5:6]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        # panel.grid.minor = element_blank(),
        legend.text.align = 0
      ) +
      scale_colour_manual(
        values = four.colors[2],
        name = "",
        labels = spawn.lab.curr
      ) +
      labs(x = sensi_xlab, y = "Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_REplot_SOcurrent.png"))

    # Log plots
    Dev.quants.ggplot.SBt <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[2],
    ]
    ggplot(Dev.quants.ggplot.SBt, aes(.data[["Model_num_plot"]], logRE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[2], ymax = logCI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
      geom_hline(yintercept = 0, lty = 1, color = "gray") +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), minor_breaks = NULL, labels = unique(Dev.quants.ggplot.SBt[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[5:6])+
      coord_cartesian(ylim = ylims.in[5:6]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        # panel.grid.minor = element_blank(),
        legend.text.align = 0
      ) +
      scale_colour_manual(
        values = four.colors[2],
        name = "",
        labels = spawn.lab.curr
      ) +
      labs(x = sensi_xlab, y = "Log Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_logREplot_SOcurrent.png"))
  }

  if (plot.figs[4] == 1) {
    # RE plots
    Dev.quants.ggplot.Dep <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[3],
    ]
    ggplot(Dev.quants.ggplot.Dep, aes(.data[["Model_num_plot"]], RE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[3], ymax = CI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot.Dep[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[7:8])+
      coord_cartesian(ylim = ylims.in[7:8]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.text.align = 0,
        panel.grid.minor = element_blank()
      ) +
      labs(x = " ", y = "Relative change") +
      scale_colour_manual(
        values = four.colors[3],
        name = "",
        labels = spawn.lab.ratio
      ) +
      annotate("text", x = c((model.summaries[["n"]] + 2), (model.summaries[["n"]] + 2)), y = c(TRP + 0.03, LRP - 0.03), label = c("TRP", "LRP"), linewidth = c(3, 3), color = c("darkgreen", "darkred")) +
      labs(x = sensi_xlab, y = "Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_hline(yintercept = c(TRP, LRP, 0), lty = c(3, 3, 1), lwd = c(0.5, 0.5, 0.5), color = c("darkgreen", "darkred", "gray")) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_REplot_status.png"))

    # Log plots
    Dev.quants.ggplot.Dep <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[3],
    ]
    ggplot(Dev.quants.ggplot.Dep, aes(.data[["Model_num_plot"]], logRE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[3], ymax = logCI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot.Dep[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[7:8])+
      coord_cartesian(ylim = ylims.in[7:8]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.text.align = 0,
        panel.grid.minor = element_blank()
      ) +
      labs(x = " ", y = "Relative change") +
      scale_colour_manual(
        values = four.colors[3],
        name = "",
        labels = spawn.lab.ratio
      ) +
      annotate("text", x = c((model.summaries[["n"]] + 2), (model.summaries[["n"]] + 2)), y = c(logTRP + 0.03, logLRP - 0.03), label = c("TRP", "LRP"), linewidth = c(3, 3), color = c("darkgreen", "darkred")) +
      labs(x = sensi_xlab, y = "Log Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_hline(yintercept = c(logTRP, logLRP, 0), lty = c(3, 3, 1), lwd = c(0.5, 0.5, 0.5), color = c("darkgreen", "darkred", "gray")) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_logREplot_status.png"))
  }

  if (plot.figs[5] == 1) {
    # RE plots
    Dev.quants.ggplot.MSY <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[4],
    ]
    ggplot(Dev.quants.ggplot.MSY, aes(.data[["Model_num_plot"]], RE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[4], ymax = CI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
      geom_hline(yintercept = 0, lty = 1, color = "gray") +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot.MSY[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[9:10])+
      coord_cartesian(ylim = ylims.in[9:10]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank()
      ) +
      scale_color_manual(
        values = four.colors[4],
        name = "",
        labels = yield.lab
      ) +
      labs(x = sensi_xlab, y = "Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_REplot_MSY.png"))
    # Log plots
    Dev.quants.ggplot.MSY <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[4],
    ]
    ggplot(Dev.quants.ggplot.MSY, aes(.data[["Model_num_plot"]], logRE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[4], ymax = logCI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
      geom_hline(yintercept = 0, lty = 1, color = "gray") +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot.MSY[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[9:10])+
      coord_cartesian(ylim = ylims.in[9:10]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank()
      ) +
      scale_color_manual(
        values = four.colors[4],
        name = "",
        labels = yield.lab
      ) +
      labs(x = sensi_xlab, y = "Log Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_logREplot_MSY.png"))
  }

  if (plot.figs[6] == 1) {
    # RE plots
    Dev.quants.ggplot.FMSY <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[5],
    ]
    ggplot(Dev.quants.ggplot.FMSY, aes(.data[["Model_num_plot"]], RE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -CI_DQs_RE[5], ymax = CI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
      geom_hline(yintercept = 0, lty = 1, color = "gray") +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot.FMSY[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[11:12])+
      coord_cartesian(ylim = ylims.in[11:12]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank()
      ) +
      scale_color_manual(
        values = four.colors[5],
        name = "",
        labels = F.lab
      ) +
      labs(x = sensi_xlab, y = "Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_REplot_FMSY.png"))

    # RE plots
    Dev.quants.ggplot.FMSY <- Dev.quants.ggplot[
      Dev.quants.ggplot[["Metric"]] == unique(Dev.quants.ggplot[["Metric"]])[5],
    ]
    ggplot(Dev.quants.ggplot.FMSY, aes(.data[["Model_num_plot"]], logRE)) +
      geom_point(aes(color = .data[["Metric"]])) +
      geom_rect(aes(xmin = 1, xmax = model.summaries[["n"]] + 1, ymin = -logCI_DQs_RE[5], ymax = logCI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
      geom_hline(yintercept = 0, lty = 1, color = "gray") +
      scale_x_continuous(breaks = 2:(model.summaries[["n"]]), labels = unique(Dev.quants.ggplot.FMSY[["Model_name"]])) +
      # scale_y_continuous(limits=ylims.in[11:12])+
      coord_cartesian(ylim = ylims.in[11:12]) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank()
      ) +
      scale_color_manual(
        values = four.colors[5],
        name = "",
        labels = F.lab
      ) +
      labs(x = sensi_xlab, y = "Log Relative change") +
      annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
      geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
    ggsave(file.path(dir, "Sensi_logREplot_FMSY.png"))
  }
}

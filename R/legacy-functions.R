

# Function to accept a list of packages, install them if not installed, and load all packages in the list
install_packages <- function(pkg) { 
  # Install package if it is not already
  if (!(pkg %in% installed.packages()[, "Package"])){ 
    
    install.packages(pkg, repos='http://cran.us.r-project.org')
  }
  
  library(pkg, character.only = TRUE)
  
} # end installPackages()


# Function to generate plots for  ratio variable
# Example usage:
# plot_ratio_variable(survey, "tpstress", "Total Perceived Stress", 1)
plot_ratio_variable <- function(data, variable_name, descriptive_name, figure_number) {
  # Ensure the variable exists in the dataset
  if (!variable_name %in% colnames(data)) {
    stop(paste("Variable", variable_name, "not found in the dataset."))
  }
  
  # Extract the variable
  variable <- data[[variable_name]]
  
  # Create histogram
  gs <- ggplot(data, aes_string(x = variable_name)) +
    geom_histogram(binwidth = 2, colour = "black", aes(y = ..density.., fill = ..count..)) +
    scale_fill_gradient("Count", low = "#DCDCDC", high = "#7C7C7C") +
    stat_function(fun = dnorm, color = "red", 
                  args = list(mean = mean(variable, na.rm = TRUE), 
                              sd = sd(variable, na.rm = TRUE))) +
    labs(x = descriptive_name, y = "Density", 
         title = paste("Histogram")) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
  
  # Create QQ plot
  qq_plot <- ggplot(data.frame(variable = variable), aes(sample = variable)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste("QQ Plot"),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
  
  # Combine the plots in a grid with additional space at the bottom
  grid.arrange(gs, qq_plot, ncol = 2, 
               bottom = textGrob(paste("Figure", figure_number, 
                                       ":", descriptive_name, "Plots"), 
                                 gp = gpar(fontsize = 12)), 
               heights = unit(c(5, 1), "null"))  # Adjust spacing after the plots
}


# Function to generate statistics needed to assess normality of a ratio variable
# Example usage:
# analyze_ratio_variable(survey, "tpstress")
analyze_ratio_variable <- function(data, variable_name) {
  # Ensure the variable exists in the dataset
  if (!variable_name %in% colnames(data)) {
    stop(paste("Variable", variable_name, "not found in the dataset."))
  }
  
  # Extract the variable
  variable <- data[[variable_name]]
  
  # Generate summary statistics
  stats <- pastecs::stat.desc(variable, basic = FALSE)
  
  
  # Calculate standardized skewness and kurtosis
  skew <- semTools::skew(variable)
  kurt <- semTools::kurtosis(variable)
  skewness_standardized <- skew[1] / skew[2]
  kurtosis_standardized <- kurt[1] / kurt[2]
  
  # Round skewness and kurtosis
  skewness_rounded <- round(abs(skewness_standardized), 2)
  kurtosis_rounded <- round(abs(kurtosis_standardized), 2)
  
  # Determine skewness and kurtosis judgment
  skew_judgment <- ifelse(skewness_rounded < 2, "acceptable", "unacceptable")
  kurtosis_judgment <- ifelse(kurtosis_rounded < 2, "acceptable", "unacceptable")
  
  # Calculate percentage of standardized scores outside acceptable limits
  z_scores <- abs(scale(variable))
  perc_gt_1_96 <- FSA::perc(as.numeric(z_scores), 1.96, "gt")
  perc_gt_3_29 <- FSA::perc(as.numeric(z_scores), 3.29, "gt")
  
  # Judgment on normality
  normality_judgment <- ifelse(perc_gt_3_29 > 95, "not normal", "can be considered normal")
  
  # Central tendency and dispersion
  if (normality_judgment == "not normal") {
    central_tendency <- paste("Mdn:", round(median(variable, na.rm = TRUE), 2), 
                              "IQR:", round(IQR(variable, na.rm = TRUE), 2))
  } else {
    central_tendency <- paste("M:", round(mean(variable, na.rm = TRUE), 2), 
                              "SD:", round(sd(variable, na.rm = TRUE), 2))
  }
  
  # Return a list with the relevant values
  return(list(
    skewness_rounded = skewness_rounded,
    skew_judgment = skew_judgment,
    kurtosis_rounded = kurtosis_rounded,
    kurtosis_judgment = kurtosis_judgment,
    perc_within_3_29 = round(perc_gt_3_29,2),
    normality_judgment = normality_judgment,
    central_tendency = central_tendency
  ))
}

#Function to generate diagnostic plots
regression_diagnostic_plots <- function(model, sdata, predictors, figure_number) {
  
  # Extract residuals
  residuals <- resid(model)
  
  # Standardize the residuals
  std_residuals <- residuals / sd(residuals)
  
  # 1. Density plot of residuals
  density_plot <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
    geom_density(
      fill = "skyblue",
      alpha = 0.6,
      color = "darkblue",
      linewidth = 1
    ) +
    geom_vline(
      aes(xintercept = 0),
      color = "red",
      linetype = "dashed",
      linewidth = 1
    ) +
    stat_function(
      fun = dnorm,
      args = list(mean = mean(residuals), sd = sd(residuals)),
      color = "darkred",
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = paste("Density Plot of Residuals"),
      x = "Residuals",
      y = "Density"
    ) +
    theme_minimal(base_size = 10)
  
  
  # 2. QQ plot of residuals
  qq_plot <- ggplot(data.frame(sample = residuals), aes(sample = std_residuals)) +
    stat_qq(color = "blue") +
    stat_qq_line(color = "red") +
    labs(
      title = paste( ": QQ Plot"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal(base_size = 10)
  
  
  # 3. Cook's Distance plot
  cooksd <- cooks.distance(model)
  cooks_plot <- ggplot(data.frame(index = seq_along(cooksd), cooks = cooksd), aes(x = index, y = cooks)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 4 * mean(cooksd, na.rm = TRUE), color = "blue", linetype = "dashed") +
    labs(
      title = paste("Cook's Distance"),
      x = "Observation Index",
      y = "Cook's Distance"
    ) +
    theme_minimal(base_size = 10)
  
  
  # Combine ggplots into a grid with patchwork
  combined_ggplots <- (density_plot | qq_plot) / cooks_plot +
    plot_annotation(
      caption = paste(
        "Figure",
        figure_number,
        ": Residual Diagnostics and Influential Observations Plots"
      )
    ) &
    theme(plot.caption = element_text(hjust = 0.5, size = 12))
  
  # Return results and display plots
  print(combined_ggplots)
  
  # 4. Homocedasticity 
  # Plot 1: Residuals vs Fitted Values for Homoscedasticity
  plot1<-plot(model, 1, main = "Residuals vs Fitted Values")
  
  # Create a list of plots to hold Residuals v Predictors plots
  plots <- list()
  # Loop through each predictor to create residuals vs predictor plot
  for (predictor in predictors) {
    p <- ggplot(sdata, aes(x = sdata[[predictor]], y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "red") +
      labs(
        x = predictor,
        y = "Residuals",
        title = paste("Residuals vs", predictor)
      ) +
      theme(
        axis.text.x = element_text(angle = 45,size=3)  # Rotate x-axis labels
        
      )
    
    plots[[predictor]] <- p  # Store the plot in the list
  }
  
  
  # Create the caption grob
  caption <- textGrob(
    paste("Figure", figure_number + 1, ": Homocedasticity Plots (Residuals v Fitted and Residuals vs Predictors Plots"),
    gp = gpar(
      fontsize = 12,
      fontface = "bold",
      col = "black"
    )
  )
  
  # Combine all the plots (including plot1) into a grid layout
  grid.arrange(arrangeGrob(
    plot1,
    grobs = plots,
    nrow = ceiling(length(predictors) / 2) + 1,
    # Increase row count to fit plot1
    ncol = 2
  ),
  bottom = caption  # Place the caption at the bottom)
  )
  
  
}


#Function to generate statistics needed for linear regression diagnostics
regression_diagnostic_stats <- function(model, data) {
  
  # Extract residuals
  residuals <- resid(model)
  
  # Standardize the residuals
  std_residuals <- residuals / sd(residuals)
  
  # Identify potential outliers in the dataset based on the studentized residuals
  outlierp <- car::outlierTest(model)  # Bonferroni p-value for most extreme observations
  
  # Caculate Cooks distance
  cooksd <- cooks.distance(model)
  # VIF Calculation
  vifmodel <- car::vif(model)
  # Round the VIF values to 2 decimal places
  vifmodel<- round(vifmodel, 2)
  
  
  
  tolerance <- 1 / vifmodel
  tolerance <- round(tolerance, 2)
  
  return(list(
    VIF = vifmodel,
    Tolerance = tolerance,
    Influential_Count = sum(cooksd > 1, na.rm = TRUE),
    max_std_residual = round(max(std_residuals),2),
    min_std_residual = round(min(std_residuals),2), 
    outlierp = outlierp
  ))
}


# Example usage
#results <- regression_diagnostics_stats(model1, sdata, predictors)


# Function to execute t-tests
conduct_t_test <- function(ratiovar, nomvar, data) {
  # Extract the ratio and group variables
  rvar <- data[[ratiovar]]
  grpvar <- data[[nomvar]]
  
  # Get descriptive statistics by group - output as a matrix
  descriptives <- psych::describeBy(rvar, grpvar, mat = TRUE)  # Fixed variable reference
  print(descriptives)
  
  # Conduct Levene's test for homogeneity of variance
  levene <- car::leveneTest(rvar ~ as.factor(grpvar), data = data)
  p_value <- levene[["Pr(>F)"]][1]  # Access p-value from Levene's test result
  
  # Run the t-test based on the homogeneity of variance assumption
  if (p_value > 0.05) {  # Use 0.05 for the common significance level
    res <- stats::t.test(rvar ~ as.factor(grpvar), var.equal = TRUE, data = data)
  } else {
    res <- stats::t.test(rvar ~ as.factor(grpvar), var.equal = FALSE, data = data)
  }
  
  # Cohen's d using effectsize package
  effectcohen <- effectsize::t_to_d(t = res$statistic, df_error = res$parameter)
  
  # Calculate eta squared
  effecteta <- round((res$statistic^2) / ((res$statistic^2) + res$parameter), 3)
  
  # Return a list of results
  return(list(
    descriptives = descriptives,
    t = res$statistic,
    tdf = res$parameter,
    tpvalue = res$p.value,
    cohen_d = effectcohen,
    eta_squared = effecteta
  ))
}


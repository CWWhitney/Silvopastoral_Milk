library(decisionSupport)
library(tidyverse)
library(patchwork)




# change plot_distribution function #### 
plot_distributions <- function (mcSimulation_object, vars, method = "smooth_simple_overlay", 
                                bins = 150, binwidth = NULL, old_names = NULL, new_names = NULL, 
                                colors = NULL, outlier_shape = ".", x_axis_name = "Outcome distribution", 
                                y_axis_name = NULL, base_size = 11, ...) 
{
  assertthat::assert_that(class(mcSimulation_object)[[1]] == 
                            "mcSimulation", msg = "mcSimulation_object is not class 'mcSimulation', please provide a valid object. This does not appear to have been generated with the 'mcSimulation' function.")
  data <- data.frame(mcSimulation_object$y, mcSimulation_object$x)
  data <- dplyr::select(data, tidyselect::all_of(vars))
  if (is.null(new_names) | is.null(old_names)) {
    new_names <- names(data)
    old_names <- names(data)
  }
  data <- dplyr::rename_at(data, dplyr::vars(tidyselect::all_of(old_names)), 
                           ~new_names)
  standard_plot_data <- tidyr::pivot_longer(data, tidyselect::all_of(new_names))
  if (is.null(colors)) {
    colors <- c("#009999", "#0000FF", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  }
  if (!is.null(y_axis_name)) 
    y_axis_name <- y_axis_name
  else if (method == "hist_simple_overlay") 
    y_axis_name <- "Number of points in bin"
  else if (method == "boxplot") 
    y_axis_name <- "Decision option"
  else y_axis_name <- "Density estimate"
  standard_plot <- ggplot2::ggplot(standard_plot_data, ggplot2::aes(x = value, 
                                                                    group = name, fill = name)) + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                                                                                                              labels = scales::comma) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                                                                                                                                                                    labels = scales::comma) + ggplot2::scale_fill_manual(values = colors) + 
    ggplot2::labs(x = x_axis_name, y = y_axis_name, fill = "Decision\noption") + 
    ggplot2::theme_bw(base_size = base_size)
  if (method == "smooth_simple_overlay") {
    return(standard_plot + ggplot2::geom_density(color = NA, 
                                                 alpha = 0.5) + ggplot2::theme(...))
  }
  if (method == "hist_simple_overlay") {
    return(standard_plot + ggplot2::geom_histogram(color = NA, 
                                                   alpha = 0.5, bins = bins, position = "identity", 
                                                   binwidth = binwidth) + ggplot2::theme(...))
  }
  if (method == "boxplot") {
    return(ggplot2::ggplot(standard_plot_data, ggplot2::aes(x = value, 
                                                            y = stats::reorder(name, value, FUN = stats::median), 
                                                            fill = name)) + ggplot2::geom_boxplot(outlier.alpha = 0.3, width = 0.6) + # added width
             ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                         labels = scales::comma) + ggplot2::scale_fill_manual(values = colors) + 
             ggplot2::labs(x = x_axis_name, y = y_axis_name, 
                           fill = "Decision\noption") + ggplot2::theme_bw(base_size = base_size) + 
             ggplot2::theme(legend.position = "none") + ggplot2::theme(...))
  }
  if (method == "boxplot_density") {
    data <- data.frame(percentage = c(0.1, 0.15, 0.5, 4), 
                       options_difference = c(100, 11229249, 58838895, 
                                              507997898))
    regression <- stats::lm(percentage ~ options_difference, 
                            data = data)
    options_difference <- max(standard_plot_data$value) - 
      min(standard_plot_data$value)
    boxploth_width_correction <- stats::coefficients(regression)[[1]] + 
      (stats::coefficients(regression)[[2]] * options_difference)
    return(ggplot2::ggplot(standard_plot_data, ggplot2::aes(x = value, 
                                                            fill = name)) + ggplot2::geom_density(alpha = 0.5, 
                                                                                                  color = NA) + ggstance::geom_boxploth(ggplot2::aes(x = value, 
                                                                                                                                                     y = 0), width = max(stats::density(standard_plot_data$value)$y * 
                                                                                                                                                                           boxploth_width_correction), varwidth = TRUE, alpha = 0.5, 
                                                                                                                                        size = 0.3, outlier.shape = outlier_shape) + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                                                                                                                                                                                                 labels = scales::comma) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                                                                                                                                                                                                                                                       labels = scales::comma) + ggplot2::scale_fill_manual(values = colors) + 
             ggplot2::labs(x = x_axis_name, y = y_axis_name) + 
             ggplot2::facet_wrap(. ~ name, scales = "free_y") + 
             ggplot2::theme_bw(base_size = base_size) + ggplot2::theme(strip.background = ggplot2::element_blank(), 
                                                                       legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, 
                                                                                                                                     hjust = 1, vjust = 1), ...))
  }
}


# Load simulation results ####
dairy_mc_simulation_10k_Y_10ha <- readRDS("data/.simulation_results/dairy_mc_simulation_10k_004Y_10ha")

dairy_mc_simulation_10k_N_10ha <- readRDS("data/.simulation_results/dairy_mc_simulation_10k_004N_10ha")


dairy_mc_simulation_10k_Y_XXha <- readRDS("data/.simulation_results/dairy_mc_simulation_10k_004Y_XXha")

dairy_mc_simulation_10k_N_XXha <- readRDS("data/.simulation_results/dairy_mc_simulation_10k_004N_XXha")



# EVPI results from: for_Calculating_EVPI_in_seperat_session.R & for_Calculating_EVPI_in_seperat_session_2.R ####
evpi_10k_Y_10ha <- readRDS("data/evpi_results/evpi_10k_004Y_10ha")

evpi_10k_N_10ha <- readRDS("data/evpi_results/evpi_10k_004N_10ha")


# Create Plots ####

## Plot histogram with boxplot ####

### Plot histogram with boxplot with ecological aspects ####
compare_NPV_Y_10ha <-  plot_distributions(mcSimulation_object = dairy_mc_simulation_10k_Y_10ha, 
                                          vars = c("NPV_no_AF", "NPV_AF"),
                                          method = "boxplot",
                                          x_axis_name = "Kapitalwert (EUR)",
                                          y_axis_name = "",
                                          base_size = 11
) +
  labs(title = "Verteilung der Kapitalwerte der Produktionssysteme\n(mit ökologischen Aspekten)"
       ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 0)
        )+
  plot_distributions(mcSimulation_object = dairy_mc_simulation_10k_Y_10ha, 
                     vars = c("NPV_AF" , "NPV_no_AF"),
                     method = "hist_simple_overlay", 
                     bins = 200,
                     binwidth = NULL,
                     x_axis_name = "Kapitalwert (EUR)",
                     y_axis_name = "Häufigkeit",
                     base_size = 11
                     )+
  guides(fill = guide_legend(title = "Entscheidungs\nOptionen")
         ) +
  scale_fill_manual(labels=c("Weidehaltung\nmit Agroforst\n(Pappel Kurzumtrieb)", "Weidehaltung"),
                    values=c("#009999", "#0000FF")
                    )+
  theme( legend.justification = c(0.3, 0.7),
         legend.location = "plot"
         )+
  plot_layout(nrow = 2, heights = c(1, 6), axes = "collect_x", axis_titles = "collect_x")

compare_NPV_Y_10ha


ggsave(filename = "images/compare_NPV_003Y_10ha.png", 
       plot = compare_NPV_Y_10ha, 
       width = 9, 
       height = 5
       )




### Plot histogram with boxplot without ecological aspects ####
compare_NPV_N_10ha <-  plot_distributions(mcSimulation_object = dairy_mc_simulation_10k_N_10ha, 
                                          vars = c("NPV_no_AF", "NPV_AF"),
                                          method = "boxplot",
                                          x_axis_name = "Kapitalwert (EUR)",
                                          y_axis_name = "",
                                          base_size = 11
) +
  labs(title = "Verteilung der Kapitalwerte der Produktionssysteme\n(ohne ökologischen Aspekten)"
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 0)
  )+
  plot_distributions(mcSimulation_object = dairy_mc_simulation_10k_N_10ha, 
                     vars = c("NPV_AF" , "NPV_no_AF"),
                     method = "hist_simple_overlay", 
                     bins = 200,
                     binwidth = NULL,
                     x_axis_name = "Kapitalwert (EUR)",
                     y_axis_name = "Häufigkeit",
                     base_size = 11
  )+
  guides(fill = guide_legend(title = "Entscheidungs\nOptionen")
  ) +
  scale_fill_manual(labels=c("Weidehaltung\nmit Agroforst\n(Pappel Kurzumtrieb)", "Weidehaltung"),
                    values=c("#009999", "#0000FF")
  )+
  theme( legend.justification = c(0.3, 0.7),
         legend.location = "plot"
  )+
  plot_layout(nrow = 2, heights = c(1, 6), axes = "collect_x", axis_titles = "collect_x")

compare_NPV_N_10ha


ggsave(filename = "images/compare_NPV_003N_10ha.png", 
       plot = compare_NPV_N_10ha, 
       width = 9, 
       height = 5
       )





## Plot combined cashflows with and without ecological Aspects ####

### Plot decision_do cashflow with and without ecological Aspects ####
decision_do_cashflow_10ha <- plot_cashflow(mcSimulation_object = dairy_mc_simulation_10k_N_10ha, 
                                           cashflow_var_name = "Cashflow_decision_do",
                                           x_axis_name = "Zeitpunkt der Betrachtung (Jahr)",
                                           y_axis_name = "Geldfluss (EUR)",
                                           legend_name = "Quantile (%)",
                                           legend_labels = c("5 bis 95", "25 bis 75", "Median"),
                                           color_25_75 = "grey40",
                                           color_5_95 = "grey70",
                                           color_median = "blue",
                                           base_size = 11,
                                           labs(title = "Geldfluss bei Agroforst Etablierung\nverglichen mit normaler Weidehaltung\n(ohne ökologischen Aspekten)")
)+
  scale_y_continuous(breaks = seq(-45000, 10000, by = 5000)
                     )+
  theme(legend.position = "none"
        )+
plot_cashflow(mcSimulation_object = dairy_mc_simulation_10k_Y_10ha, 
              cashflow_var_name = "Cashflow_decision_do",
              x_axis_name = "Zeitpunkt der Betrachtung (Jahr)",
              y_axis_name = "",
              legend_name = "Quantile (%)",
              legend_labels = c("5 bis 95", "25 bis 75", "Median"),
              color_25_75 = "grey40",
              color_5_95 = "grey70",
              color_median = "blue",
              base_size = 11,
              labs(title = "Geldfluss bei Agroforst Etablierung\nverglichen mit normaler Weidehaltung\n(mit ökologischen Aspekten)")
)+
  scale_y_continuous(breaks = seq(-45000, 10000, by = 5000)
                     )+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )

decision_do_cashflow_10ha


ggsave(filename = "images/decision_do_cashflow_003_10ha.png", 
       plot = decision_do_cashflow_10ha, 
       width = 7, 
       height = 4
       )


### Plot no_AF cashflow with and without ecological Aspects ####
no_AF_cashflow_10ha <- plot_cashflow(mcSimulation_object = dairy_mc_simulation_10k_N_10ha, 
                                           cashflow_var_name = "Cashflow_no_AF",
                                           x_axis_name = "Zeitpunkt der Betrachtung (Jahr)",
                                           y_axis_name = "Geldfluss (EUR)",
                                           legend_name = "Quantile (%)",
                                           legend_labels = c("5 bis 95", "25 bis 75", "Median"),
                                           color_25_75 = "grey40",
                                           color_5_95 = "grey70",
                                           color_median = "blue",
                                           base_size = 11,
                                           labs(title = "Geldfluss bei\nherkömmlicher Weidewirtschaft\n(ohne ökologischen Aspekten)")
)+
  scale_y_continuous(breaks = seq(-10000, 40000, by = 5000)
  )+
  theme(legend.position = "none"
  )+
  plot_cashflow(mcSimulation_object = dairy_mc_simulation_10k_Y_10ha, 
                cashflow_var_name = "Cashflow_no_AF",
                x_axis_name = "Zeitpunkt der Betrachtung (Jahr)",
                y_axis_name = "",
                legend_name = "Quantile (%)",
                legend_labels = c("5 bis 95", "25 bis 75", "Median"),
                color_25_75 = "grey40",
                color_5_95 = "grey70",
                color_median = "blue",
                base_size = 11,
                labs(title = "Geldfluss bei\nherkömmlicher Weidewirtschaft\n(mit ökologischen Aspekten)")
  )+
  scale_y_continuous(breaks = seq(-10000, 40000, by = 5000)
  )+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

no_AF_cashflow_10ha


ggsave(filename = "images/no_AF_cashflow_003_10ha.png", 
       plot = no_AF_cashflow_10ha, 
       width = 7, 
       height = 4
       )




### Plot AF cashflow with and without ecological Aspects ####
AF_cashflow_10ha <- plot_cashflow(mcSimulation_object = dairy_mc_simulation_10k_N_10ha, 
                                     cashflow_var_name = "Cashflow_AF",
                                     x_axis_name = "Zeitpunkt der Betrachtung (Jahr)",
                                     y_axis_name = "Geldfluss (EUR)",
                                     legend_name = "Quantile (%)",
                                     legend_labels = c("5 bis 95", "25 bis 75", "Median"),
                                     color_25_75 = "grey40",
                                     color_5_95 = "grey70",
                                     color_median = "blue",
                                     base_size = 11,
                                     labs(title = "Geldfluss bei\nAgroforst Etablierung\n(ohne ökologischen Aspekten)")
)+
  scale_y_continuous(breaks = seq(-35000, 30000, by = 5000)
  )+
  theme(legend.position = "none"
  )+
  plot_cashflow(mcSimulation_object = dairy_mc_simulation_10k_Y_10ha, 
                cashflow_var_name = "Cashflow_AF",
                x_axis_name = "Zeitpunkt der Betrachtung (Jahr)",
                y_axis_name = "",
                legend_name = "Quantile (%)",
                legend_labels = c("5 bis 95", "25 bis 75", "Median"),
                color_25_75 = "grey40",
                color_5_95 = "grey70",
                color_median = "blue",
                base_size = 11,
                labs(title = "Geldfluss bei\nAgroforst Etablierung\n(mit ökologischen Aspekten)")
  )+
  scale_y_continuous(breaks = seq(-35000, 30000, by = 5000)
  )+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

AF_cashflow_10ha


ggsave(filename = "images/AF_cashflow_003_10ha.png", 
       plot = AF_cashflow_10ha, 
       width = 7, 
       height = 4
       )






## Plot Projection to Latent Structures (PLS) ####

### Plot Projection to Latent Structures (PLS) with ecological aspects ####
pls_result_N_10ha <- plsr.mcSimulation(object = dairy_mc_simulation_10k_N_10ha,
                                       resultName = names(dairy_mc_simulation_10k_N_10ha$y)[3], 
                                       ncomp = 1
                                       )

pls_N_10ha <- plot_pls(plsrResults = pls_result_N_10ha, 
                       input_table = data.frame(variable = input_table$variable, 
                                                label = input_table$label
                       ), 
                       cut_off_line = 1,
                       x_axis_name = "Wichtigkeit von Einflüssen der Variablen in der Projektion",
                       y_axis_name = NULL,
                       legend_name = "Koeffizient",
                       legend_labels = c("Positive", "Negative"),
                       pos_color = "cadetblue",
                       neg_color = "firebrick",
                       base_size = 11,
                       threshold = 1
                       )+
  labs(title = "Wichtigkeit von Variablen (ohne ökologischen Aspekten)")

pls_N_10ha


ggsave(filename = "images/pls_1_003N_10ha.png", 
       plot = pls_N_10ha, 
       width = 8, 
       height = 3.5
       )


### Plot Projection to Latent Structures (PLS) with ecological aspects ####
pls_result_Y_10ha <- plsr.mcSimulation(object = dairy_mc_simulation_10k_Y_10ha,
                                       resultName = names(dairy_mc_simulation_10k_Y_10ha$y)[3], 
                                       ncomp = 1
                                       )

pls_Y_10ha <- plot_pls(plsrResults = pls_result_Y_10ha, 
                       input_table = data.frame(variable = input_table$variable, 
                                                label = input_table$label
                       ), 
                       cut_off_line = 1,
                       x_axis_name = "Wichtigkeit von Einflüssen der Variablen in der Projektion",
                       y_axis_name = NULL,
                       legend_name = "Koeffizient",
                       legend_labels = c("Positive", "Negative"),
                       pos_color = "cadetblue",
                       neg_color = "firebrick",
                       base_size = 11,
                       threshold = 1
                       )+
  labs(title = "Wichtigkeit von Variablen (mit ökologischen Aspekten)")

pls_Y_10ha


ggsave(filename = "images/pls_1_003Y_10ha.png", 
       plot = pls_Y_10ha, 
       width = 8, 
       height = 3.5
       )





### Plot Projection to Latent Structures (PLS) with ecological aspects and XX ha####
pls_result_N_XXha <- plsr.mcSimulation(object = dairy_mc_simulation_10k_N_XXha,
                                       resultName = names(dairy_mc_simulation_10k_N_XXha$y)[3], 
                                       ncomp = 1
                                       )

pls_N_XXha <- plot_pls(plsrResults = pls_result_N_XXha, 
                       input_table = data.frame(variable = input_table$variable, 
                                                label = input_table$label
                       ), 
                       cut_off_line = 1,
                       x_axis_name = "Wichtigkeit von Einflüssen der Variablen in der Projektion",
                       y_axis_name = NULL,
                       legend_name = "Koeffizient",
                       legend_labels = c("Positive", "Negative"),
                       pos_color = "cadetblue",
                       neg_color = "firebrick",
                       base_size = 11,
                       threshold = 1
)+
  labs(title = "Wichtigkeit von Variablen (ohne ökologischen Aspekten)")

pls_N_XXha


ggsave(filename = "images/pls_1_003N_XXha.png", 
       plot = pls_N_XXha, 
       width = 8, 
       height = 2
)


### Plot Projection to Latent Structures (PLS) with ecological aspects and XX ha####
pls_result_Y_XXha <- plsr.mcSimulation(object = dairy_mc_simulation_10k_Y_XXha,
                                       resultName = names(dairy_mc_simulation_10k_Y_XXha$y)[3], 
                                       ncomp = 1
                                       )

pls_Y_XXha <- plot_pls(plsrResults = pls_result_Y_XXha, 
                       input_table = data.frame(variable = input_table$variable, 
                                                label = input_table$label
                       ), 
                       cut_off_line = 1,
                       x_axis_name = "Wichtigkeit von Einflüssen der Variablen in der Projektion",
                       y_axis_name = NULL,
                       legend_name = "Koeffizient",
                       legend_labels = c("Positive", "Negative"),
                       pos_color = "cadetblue",
                       neg_color = "firebrick",
                       base_size = 11,
                       threshold = 1
)+
  labs(title = "Wichtigkeit von Variablen (mit ökologischen Aspekten)")

pls_Y_XXha


ggsave(filename = "images/pls_1_003Y_XXha.png", 
       plot = pls_Y_XXha, 
       width = 8, 
       height = 2
)




## Value of information (VoI) analysis ####

evpi_plot_Y_10ha <-  plot_evpi(evpi_10k_Y_10ha, 
                               decision_vars = "NPV_decision_do"
)

#Warnmeldung:
#There are no variables with a positive EVPI. You probably do not need a plot for that.

evpi_plot_N_10ha <-  plot_evpi(evpi_10k_N_10ha, 
                               decision_vars = "NPV_decision_do"
)

#Warnmeldung:
#There are no variables with a positive EVPI. You probably do not need a plot for that. 


# Exporting EVPI result plots ####
ggsave(filename = "images/evpi_plot_003Y_10ha.png", 
       plot = evpi_plot_Y_10ha, 
       width = 5, 
       height = 6
)

ggsave(filename = "images/evpi_plot_003N_10ha.png", 
       plot = evpi_plot_N_10ha, 
       width = 5, 
       height = 6
)




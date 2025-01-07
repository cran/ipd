## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  error = FALSE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
default_options <- options()

## ----eval = F-----------------------------------------------------------------
# #-- Install devtools if it is not already installed
# 
# install.packages("devtools")
# 
# #-- Install the ipd package from GitHub
# 
# devtools::install_github("ipd-tools/ipd")

## ----setup--------------------------------------------------------------------
#-- Load ipd Package

library(ipd)

## ----simols-------------------------------------------------------------------
#-- Generate a Dataset for Linear Regression

set.seed(123)

n <- c(10000, 500, 1000)

dat_ols <- simdat(n = n, effect = 1, sigma_Y = 4, model = "ols")

#-- Print First 6 Rows of Training, Labeled, and Unlabeled Subsets

options(digits=2)

head(dat_ols[dat_ols$set_label == "training",])

head(dat_ols[dat_ols$set_label == "labeled",])

head(dat_ols[dat_ols$set_label == "unlabeled",])

## ----plot, echo = F, error = F, warning = F, message = F, comment = NA, fig.height=4, fig.width=7----

library(tidyverse)

library(patchwork)

#-- Plot example labeled data

dat_labeled <- dat_ols[dat_ols$set_label == "labeled",]

p1 <- dat_labeled |>

  pivot_longer(Y:f) |>

  mutate(

    name = factor(name, levels = c("f", "Y"),

      labels = c("Predicted ", "Observed ")) |> fct_rev()) |>

  ggplot(aes(x = X1, y = value, group = name, color = name, fill = name)) +

    theme_bw() + coord_fixed(1/3) +
  
    geom_abline(slope = 1, intercept = 0) +

    geom_point(alpha = 0.5) +

    geom_smooth(method = "lm") +

    scale_x_continuous(limits = c(-2.5, 2.5)) +

    scale_y_continuous(limits = c(-7.5, 7.5)) +

    scale_color_manual(values = c("#AA4AC4", "#00C1D5")) +

    scale_fill_manual(values = c("#AA4AC4", "#00C1D5")) +

    labs(
      
      x = "\nCovariate", y = "Outcome\n", fill = NULL, color = NULL,
       
      title = "Outcomes vs. Covariate") +

    theme(

      legend.direction = "horizontal",

      legend.position = "inside",

      legend.position.inside = c(0.5, 0.91),

      axis.title = element_text(size = 8),

      axis.text = element_text(size = 8),
      
      title = element_text(size = 8))

p2 <- dat_labeled |> 
  
  ggplot(aes(x = f, y = Y)) +

    theme_bw() + coord_fixed(1) +
  
    geom_abline(slope = 1, intercept = 1) +

    geom_point(color = "#00C1D5", alpha = 0.5) +

    geom_smooth(method = "lm", color = "#00C1D5") +

    scale_x_continuous(limits = c(-7.5, 7.5)) +

    scale_y_continuous(limits = c(-7.5, 7.5)) +

    labs(
      
      x = "\nPredicted Outcome", y = "Observed Outcome\n",
       
      title = "Predicted vs. Observed Outcomes") +

    theme(

      axis.title = element_text(size = 8),

      axis.text = element_text(size = 8),
      
      title = element_text(size = 8))

fig1 <- (p1 + theme(plot.margin = unit(c(0,20,0, 0), "pt"))) +

  (p2 + theme(plot.margin = unit(c(0,20,0,20), "pt"))) +

  plot_annotation(tag_levels = "A")

fig1


## ----simlogistic--------------------------------------------------------------
#-- Generate a Dataset for Logistic Regression

set.seed(123)

dat_logistic <- simdat(n = n, effect = 3, sigma_Y = 1, 
                       
  model = "logistic")

#-- Print First 6 Rows of Training, Labeled, and Unlabeled Subsets

head(dat_logistic[dat_logistic$set_label == "training",])

head(dat_logistic[dat_logistic$set_label == "labeled",])

head(dat_logistic[dat_logistic$set_label == "unlabeled",])

## ----logsum, echo = F---------------------------------------------------------

dat_logistic_labeled <- dat_logistic[dat_logistic$set_label == "labeled",]

dat_logistic_labeled_summ <- dat_logistic_labeled |>
  
  group_by(Y, f) |> count() |> ungroup() |>
  
  mutate(Y = factor(Y), f = factor(f), 
         
    pct = n / sum(n) * 100,
    
    fill = if_else(Y == f, 1, 0))


## ----plot2, echo = F, fig.width=7---------------------------------------------

dat_logistic_labeled_summ |>
  
  ggplot(aes(x = f, y = Y, fill = fill)) + 
  
    geom_tile() + coord_equal() +
  
    geom_text(
      
      aes(label = paste0(n, " (", sprintf("%2.1f", pct), "%)")), vjust = 1) +
  
    scale_x_discrete(expand = c(0,0), limits = rev) +
  
    scale_y_discrete(expand = c(0,0)) +
  
    scale_fill_gradient(high = "#00C1D5", low = "#FFB500") + 
  
    theme(legend.position = "none")

## ----naive--------------------------------------------------------------------
#--- Fit the Naive Regression

lm(f ~ X1, data = dat_ols[dat_ols$set_label == "unlabeled",]) |> 
  
  summary()

## ----classic------------------------------------------------------------------
#--- Fit the Classic Regression

lm(Y ~ X1, data = dat_ols[dat_ols$set_label == "labeled",]) |> 
  
  summary()

## ----postpi_boot_ols----------------------------------------------------------
#-- Specify the Formula

formula <- Y - f ~ X1

#-- Fit the PostPI Bootstrap Correction

nboot <- 200

ipd::ipd(formula, 
         
  method = "postpi_boot", model = "ols", data = dat_ols, label = "set_label", 
  
  nboot = nboot) |> 
  
  summary()

## ----postpi_analytic_ols------------------------------------------------------
#-- Fit the PostPI Analytic Correction

ipd::ipd(formula, 
         
  method = "postpi_analytic", model = "ols", data = dat_ols, label = "set_label") |> 
  
  summary()

## ----ppi_ols------------------------------------------------------------------
#-- Fit the PPI Correction

ipd::ipd(formula, 
         
  method = "ppi", model = "ols", data = dat_ols, label = "set_label") |> 
  
  summary()

## ----ppi_plusplus-------------------------------------------------------------
#-- Fit the PPI++ Correction

ipd::ipd(formula, 
         
  method = "ppi_plusplus", model = "ols", data = dat_ols, label = "set_label") |> 
  
  summary()

## ----pspa---------------------------------------------------------------------
#-- Fit the PSPA Correction

ipd::ipd(formula, 
         
  method = "pspa", model = "ols", data = dat_ols, label = "set_label") |> 
  
  summary()

## ----naive2-------------------------------------------------------------------
#--- Fit the Naive Regression

glm(f ~ X1, family = binomial, 
    
  data = dat_logistic[dat_logistic$set_label == "unlabeled",]) |> 
  
  summary()

## ----classic2-----------------------------------------------------------------
#--- Fit the Classic Regression

glm(Y ~ X1, family = binomial,
   
  data = dat_logistic[dat_logistic$set_label == "labeled",]) |> 
  
  summary()

## ----postpi_boot2-------------------------------------------------------------
#-- Specify the Formula

formula <- Y - f ~ X1

#-- Fit the PostPI Bootstrap Correction

nboot <- 200

ipd::ipd(formula, method = "postpi_boot", model = "logistic", 
         
  data = dat_logistic, label = "set_label", nboot = nboot) |> 
  
  summary()

## ----ppi2---------------------------------------------------------------------
#-- Fit the PPI Correction

ipd::ipd(formula, method = "ppi", model = "logistic", 
         
  data = dat_logistic, label = "set_label") |> 
  
  summary()

## ----ppi_plusplus2------------------------------------------------------------
#-- Fit the PPI++ Correction

ipd::ipd(formula, method = "ppi_plusplus", model = "logistic", 
         
  data = dat_logistic, label = "set_label") |> 
  
  summary()

## ----pspa2--------------------------------------------------------------------
#-- Fit the PSPA Correction

ipd::ipd(formula, method = "pspa", model = "logistic", 
         
  data = dat_logistic, label = "set_label") |> 
  
  summary()

## ----methods------------------------------------------------------------------
#-- Fit the PostPI Bootstrap Correction

nboot <- 200

fit_postpi <- ipd::ipd(formula, 
         
  method = "postpi_boot", model = "ols", data = dat_ols, label = "set_label", 
  
  nboot = nboot)


## ----print--------------------------------------------------------------------
#-- Print the Model

print(fit_postpi)


## ----summary------------------------------------------------------------------
#-- Summarize the Model

summ_fit_postpi <- summary(fit_postpi)
  
#-- Print the Model Summary

print(summ_fit_postpi)

## ----tidy---------------------------------------------------------------------
#-- Tidy the Model Output

tidy(fit_postpi)

## ----glance-------------------------------------------------------------------
#-- Get a One-Row Summary of the Model

glance(fit_postpi)

## ----augment------------------------------------------------------------------
#-- Augment the Original Data with Fitted Values and Residuals

augmented_df <- augment(fit_postpi)

head(augmented_df)

## ----echo=FALSE---------------------------------------------------------------
options(default_options)

## ----help, eval=F-------------------------------------------------------------
# ?ipd


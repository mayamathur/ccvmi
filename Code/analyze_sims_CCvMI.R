
# PRELIMINARIES ----------------------------------------------------

#rm(list=ls())

# data-wrangling packages
library(here)
library(plotly)  # must be BEFORE dplyr or else plotly::select will take over
library(dplyr)
library(tibble)
library(ggplot2)
library(data.table)
library(tidyverse)
library(fastDummies)
library(xlsx)
# meta-analysis packages
library(metafor)
library(robumeta)
# other
library(xtable)
library(testthat)
library(Deriv)
library(mosaic)
library(hpa)
library(pracma)
library(truncnorm)
library(tmvtnorm)
library(RColorBrewer)
library(sjmisc)

# prevent masking
select = dplyr::select


# ~~ User-specified global vars -------------------------
# no sci notation
options(scipen=999)

# control which results should be redone and/or overwritten
# e.g., sim_plot_multiple_outcomes
#@ not all fns respect this setting
overwrite.res = TRUE


# ~~ Set directories -------------------------

# helper fns
code.dir = here()
setwd(code.dir)
source("analyze_sims_helper_NMAR2.R")

# sim results
data.dir = str_replace( string = here(),
                           pattern = "Code",
                           replacement = "Data" )


# analysis results
results.dir = str_replace( string = here(),
                           pattern = "Code",
                           replacement = "Results from analysis" )



overleaf.dir.figs = "/Users/mmathur/Dropbox/Apps/Overleaf/NMAR2 (sufficiency) Overleaf/figures/sims"
overleaf.dir.nums = "/Users/mmathur/Dropbox/Apps/Overleaf/NMAR2 (sufficiency) Overleaf/R_objects"



# ~~ Get agg data -------------------------

# get stitched data
setwd(data.dir)
aggo = fread("agg.csv")
file.info("agg.csv")$mtime
table(aggo$method)

# check when the dataset was last modified to make sure we're working with correct version
file.info("agg.csv")$mtime

agg = wrangle_agg_local(aggo)
table(agg$method)
table(agg$dag_name)


# ~~ Quick sanity checks on results -------------------------

if ( FALSE ) {
  # get stitched data
  setwd(data.dir)
  s = fread("stitched.csv")
  
  # check when the dataset was last modified to make sure we're working with correct version
  file.info("stitched.csv")$mtime
  
  
  # quick sanity check for results
  # aggregating over sample sizes and which vars are missing
  t = s %>% group_by(dag_name, method) %>%
    summarise( meanNA(bhat),
               meanNA(bhat_covers) ) %>%
    mutate_if( is.numeric, function(x) round(x, 2) )
  
  
  View(t)
  
  
  # large n and MI-unadj only
  t1 = s %>% group_by(dag_name, missing_vars) %>%
    filter( N == 10000 & method == "MI-unadj" ) %>%
    summarise( n(),
               meanNA(bhat),
               meanNA(bhat_covers) ) %>%
    mutate_if( is.numeric, function(x) round(x, 2) )
  
  
  View(t1)
  
}



# ~~ Make agg data -------------------------

# # if only analyzing a single set of sims (no merging):
# setwd(data.dir)
# agg = make_agg_data(s)
# 
# 
# dim(agg)  # will exceed number of scens because of multiple methods
# expect_equal( 36, nuni(agg$scen.name) )
# 
# # prettify variable names
# agg = wrangle_agg_local(agg)
# table(agg$method.pretty)
# table(agg$missing.vars.MAR)
# 
# # save data
# setwd(data.dir)
# fwrite(agg, "agg.csv")

# # one-off for paper
# update_result_csv( name = "n scens",
#                    value = nuni(agg$scen.name),
#                    .results.dir = results.dir,
#                    .overleaf.dir = overleaf.dir.nums )


# TABLES -------------------------

# ~ Check MI asymptotics  -------------------------
# should agree with my classifications as MAR vs. NMAR
# large n and MI-unadj only
# sanity check: should agree with t1 above
t1b = agg %>% group_by(dag_name, missing.vars.MAR) %>%
  filter( N == 10000 & method == "MI-unadj" ) %>%
  summarise( n = nuni(scen.name),
             meanNA(BhatBias),
             meanNA(BhatCover) ) %>%
  mutate_if( is.numeric, function(x) round(x, 2) )


View(t1b)


# ~ Stats for CCvMI (tutorial) paper  -------------------------------------------------

# MNAR, but CCA-adj is good
t = agg %>% filter(dag_name == "I(a)" & missing.vars.MAR == "A missing; MNAR") %>%
  filter(method %in% c("CC-adj", "MI-unadj", "CC-unadj")) %>%
  group_by(N, method) %>%
  summarise( n = nuni(scen.name),
             meanNA(bhat),
             meanNA(BhatBias),
             meanNA(BhatCover) ) %>%
  mutate_if( is.numeric, function(x) round(x, 2) )
View(t)

# MAR, but MI's asymptotics are slow
t = agg %>% filter(dag_name == "I(c)" & missing.vars.MAR == "Y missing; MAR") %>%
  filter(method %in% c("CC-adj", "MI-unadj", "CC-unadj")) %>%
  group_by(N, method) %>%
  summarise( n = nuni(scen.name),
             meanNA(bhat),
             meanNA(BhatBias),
             meanNA(BhatCover) ) %>%
  mutate_if( is.numeric, function(x) round(x, 2) )
View(t)



# BIG PLOTLY FIGURES -------------------------

# I think you can remove this entire section because simplified part below already runs it?
# But that code excludes certain methods

# Huge figure with all DAGs -------------------------

setwd(results.dir)
setwd("Big plotlys")
setwd("*all_dags")
temp.dir = getwd()

p = quick_5var_agg_plot(.dat = agg,
                        .Xname = "N",
                        .Yname = "BhatBias",
                        .colorVarName = "method",
                        .facetVar2Name = "missing.vars.MAR",
                        .facetVar1Name = "dag_name",
                        .ggtitle = "",
                        .y.breaks = NULL,
                        .writePlot = TRUE,
                        .results.dir = temp.dir )

p = quick_5var_agg_plot(.dat = agg,
                        .Xname = "N",
                        .Yname = "BhatCover",
                        .colorVarName = "method",
                        .facetVar2Name = "missing.vars.MAR",
                        .facetVar1Name = "dag_name",
                        .ggtitle = "",
                        .y.breaks = NULL,
                        .writePlot = TRUE,
                        .results.dir = temp.dir )

p = quick_5var_agg_plot(.dat = agg,
                        .Xname = "N",
                        .Yname = "BhatRMSE",
                        .colorVarName = "method",
                        .facetVar2Name = "missing.vars.MAR",
                        .facetVar1Name = "dag_name",
                        .ggtitle = "",
                        .y.breaks = NULL,
                        .writePlot = TRUE,
                        .results.dir = temp.dir )


p = quick_5var_agg_plot(.dat = agg,
                        .Xname = "N",
                        .Yname = "BhatWidth",
                        .colorVarName = "method",
                        .facetVar2Name = "missing.vars.pretty",
                        .facetVar1Name = "dag.name.pretty",
                        .ggtitle = "",
                        .y.breaks = NULL,
                        .writePlot = TRUE,
                        .results.dir = temp.dir )


# Separate plots for each DAG - NOT IN USE -------------------------


# for ( .dag in unique(agg$dag_name) ) {
#   
#   # test
#   #.dag = unique(agg$dag_name)[1]
#   
#   setwd(results.dir)
#   setwd("Big plotlys")
#   setwd(.dag)
#   temp.dir = getwd()
#   
#   
#   p = quick_5var_agg_plot(.dat = agg[ agg$dag_name == .dag, ],
#                           .Xname = "N",
#                           .Yname = "BhatBias",
#                           .colorVarName = "method",
#                           .facetVar1Name = "missing.vars.pretty",
#                           .ggtitle = .dag,
#                           .y.breaks = NULL,
#                           .writePlot = TRUE,
#                           .results.dir = temp.dir
#                          )
#   
#   p = quick_5var_agg_plot(.dat = agg[ agg$dag_name == .dag, ],
#                           .Xname = "N",
#                           .Yname = "BhatCover",
#                           .colorVarName = "method",
#                           .facetVar1Name = "missing.vars.pretty",
#                           .ggtitle = .dag,
#                           .y.breaks = NULL,
#                           .writePlot = TRUE,
#                           .results.dir = temp.dir
#   )
#   
#   p = quick_5var_agg_plot(.dat = agg[ agg$dag_name == .dag, ],
#                           .Xname = "N",
#                           .Yname = "BhatWidth",
#                           .colorVarName = "method",
#                           .facetVar1Name = "missing.vars.pretty",
#                           .ggtitle = .dag,
#                           .y.breaks = NULL,
#                           .writePlot = TRUE,
#                           .results.dir = temp.dir
#   )
#   
#   p = quick_5var_agg_plot(.dat = agg[ agg$dag_name == .dag, ],
#                           .Xname = "N",
#                           .Yname = "BhatRMSE",
#                           .colorVarName = "method",
#                           .facetVar1Name = "missing.vars.pretty",
#                           .ggtitle = .dag,
#                           .y.breaks = NULL,
#                           .writePlot = TRUE,
#                           .results.dir = temp.dir
#   )
#   
#   
# }
#   


# SIMPLIFIED PLOTS FOR PAPER  -------------------------

method.keepers = c("CC-adjusted", "CC-unadjusted", "MI-adjusted", "MI-unadjusted")
aggp = agg %>% filter( method.pretty %in% method.keepers ) %>% droplevels()

colors = c(
  `CC-unadjusted` = "#ff99cc",
  `CC-adjusted` = "red",
  `MI-unadjusted` = "#3399ff",
  `MI-adjusted` = "#004080")

# fewer x-axis breaks than manipulated levels for visual clarity
x.breaks = unique(agg$N)
( x.breaks = x.breaks[ !x.breaks == 5000 ] )

p = quick_5var_agg_plot_2(.dat = aggp,
                        .Xname = "N",
                        .Yname = "BhatBias",
                        .colorVarName = "method.pretty",
                        .facetVar2Name = "missing.vars.MAR",
                        .facetVar1Name = "dag.name.pretty",
                        .ggtitle = "",
                        .colors = colors,
                        .x.breaks = x.breaks,
                        .y.breaks = NULL,
                        
                        .writePlot = TRUE,
                        ..results.dir = results.dir,
                        ..overleaf.dir =  overleaf.dir.figs)


p = quick_5var_agg_plot_2(.dat = aggp,
                          .Xname = "N",
                          .Yname = "BhatCover",
                          .colorVarName = "method.pretty",
                          .facetVar2Name = "missing.vars.MAR",
                          .facetVar1Name = "dag.name.pretty",
                          .ggtitle = "",
                          .colors = colors,
                          .x.breaks = x.breaks,
                          .y.breaks = NULL,
                          
                          .writePlot = TRUE,
                          ..results.dir = results.dir,
                          ..overleaf.dir =  overleaf.dir.figs)


p = quick_5var_agg_plot_2(.dat = aggp,
                          .Xname = "N",
                          .Yname = "BhatWidth",
                          .colorVarName = "method.pretty",
                          .facetVar2Name = "missing.vars.MAR",
                          .facetVar1Name = "dag.name.pretty",
                          .ggtitle = "",
                          .colors = colors,
                          .x.breaks = x.breaks,
                          .y.breaks = NULL,
                          
                          .writePlot = TRUE,
                          ..results.dir = results.dir,
                          ..overleaf.dir =  overleaf.dir.figs)


p = quick_5var_agg_plot_2(.dat = aggp,
                          .Xname = "N",
                          .Yname = "BhatRMSE",
                          .colorVarName = "method.pretty",
                          .facetVar2Name = "missing.vars.MAR",
                          .facetVar1Name = "dag.name.pretty",
                          .ggtitle = "",
                          .colors = colors,
                          .x.breaks = x.breaks,
                          .y.breaks = NULL,
                          
                          .writePlot = TRUE,
                          ..results.dir = results.dir,
                          ..overleaf.dir =  overleaf.dir.figs)


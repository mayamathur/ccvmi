
# PLOTTING FNS -------------------------------------------------------------



# new version of the below fn that allows you to subset 
quick_5var_agg_plot_2 = function(.Xname,
                               .Yname,
                               .colorVarName,
                               .facetVar1Name = NULL,
                               .facetVar2Name = NULL,
                               
                               .dat,
                               .ggtitle = "",
                               
                               .x.breaks = NULL,
                               .y.breaks = NULL,
                               
                               .colors = NULL,
                               
                               .writePlot = FALSE,
                               ..results.dir = NULL,
                               ..overleaf.dir = NULL) {
  
  
  # # TEST
  # method.keepers = c("CC-adjusted", "CC-unadjusted", "MI-adjusted", "MI-unadjusted")
  # .dat = agg %>% filter( method.pretty %in% method.keepers ) %>% droplevels()
  # .Xname = "N"
  # .Yname = "BhatBias"
  # .colorVarName = "method.pretty"
  # .facetVar2Name = "missing.vars.MAR"
  # .facetVar1Name = "dag_name"
  # .colors = NULL
  # .ggtitle = ""
  # .writePlot = FALSE
  # .y.breaks = NULL
  # #.results.dir
  # .colors = c(
  #   `CC-unadjusted` = "#ff99cc",
  #   `CC-adjusted` = "red",
  #   `MI-unadjusted` = "#3399ff",
  #   `MI-adjusted` = "#004080")

  
  
  .dat$Y = .dat[[.Yname]]
  .dat$X = .dat[[.Xname]]
  .dat$colorVar = .dat[[.colorVarName]]
  # don't try to move these inside conditional statement below
  #  about facet_wrap b/c then .dat won't contain the facet vars
  if(!is.null(.facetVar1Name)) .dat$facetVar1 = .dat[[.facetVar1Name]]
  if(!is.null(.facetVar2Name)) .dat$facetVar2 = .dat[[.facetVar2Name]]
  
  # pick pretty label for X-axis
  xlab = .Xname
  if (.Xname == "N") xlab = "Sample size before 50% missingness (N)"
  
  # pick pretty label for Y-axis
  ylab = .Yname
  if (.Yname == "BhatBias") ylab = "Bias"
  if (.Yname == "BhatCover") ylab = "CI coverage"
  if (.Yname == "BhatWidth") ylab = "CI width"
  if (.Yname == "BhatRMSE") ylab = "RMSE"
  
  # force ordering of methods
  correct.order = c("CC-unadjusted",
                    "CC-adjusted",
                    "MI-unadjusted",
                    "MI-adjusted")
  
  
  .dat$method.pretty = factor(.dat$method.pretty, levels = rev(correct.order))
  
  
  

  # ~ Make base plot ----------
  p = ggplot( data = .dat,
              aes( x = X,
                   y = Y,
                   color = as.factor(colorVar) ) ) +
    
    #geom_point() +
    geom_line(linetype = 1,
              lwd = 1,
              # dodge lines to avoid perfect overlap
              position = position_dodge(width=0.25) ) +
    
    # use all values of
    #scale_x_log10( breaks = unique(.dp$n) )
    # use only some values
    #scale_x_log10( breaks = c(500, 1000) ) +
    
    xlab(xlab) +
    ylab(ylab) +
    #guides( color = guide_legend(title = .colorVarName) ) +
    guides( color = guide_legend(title = "Method") ) +
    ggtitle(.ggtitle) +
    # base_size controls all text sizes; default is 11
    # https://ggplot2.tidyverse.org/reference/ggtheme.html
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom")
  
  # ~ Add reference lines ----------
  if ( str_contains(x = .Yname, pattern = "Cover") ) {
    p = p + geom_hline( yintercept = 0.95,
                        lty = 1,
                        color = "gray" ) 
    
  }
  
  if ( str_contains(x = .Yname, pattern = "Bias") ) {
    p = p + geom_hline( yintercept = 0,
                        lty = 1,
                        color = "gray" ) 
    
  }
  
  
  # ~ Add facetting ----------
  # this block needs to be after adding geom_hlines so that the lines obey the facetting
  if ( !is.null(.facetVar1Name) & !is.null(.facetVar2Name) ) {
    p = p + facet_wrap(facetVar1 ~ facetVar2,
                       nrow = length( unique(.dat$facetVar1) ) ) 
  }
  
  if ( !is.null(.facetVar1Name) & is.null(.facetVar2Name) ) {
    p = p + facet_wrap("facetVar1",
                       nrow = length( unique(.dat$facetVar1) ) ) 
  }
  
  # ~ Custom colors ----------
  if ( !is.null(.colors) ) {
    p = p + scale_color_manual(values = .colors)
  }
  
  # ~ Set X-axis scaling ----------
  if ( is.null(.x.breaks) ) x.breaks = unique(.dat$X)
  p = p + scale_x_continuous(breaks = x.breaks,
                             trans = "log") 
  
  # ~ Set Y-axis breaks ----------
  # other outcomes follow rules or can just use default axis breaks
  # y.breaks are only still null if none of the above applied
  if ( is.null(.y.breaks) ) {
    # set default breaks
    if ( grepl(pattern = "Cover", .Yname) ){
      y.breaks = seq(0.4, 1, .1)
      
    } else {
      # otherwise keep the default limits from ggplot
      y.breaks = ggplot_build(p)$layout$panel_params[[1]]$y$breaks
    }
  } # end "if ( is.null(.y.breaks) )"
  
  
  # if user provided own y.breaks
  if ( !is.null(.y.breaks) ) {
    y.breaks = .y.breaks
  }
  
  # use coord_cartesian so that lines/points that go outside limits look cut off
  #  rather than completely omitted
  p = p + coord_cartesian( ylim = c( min(y.breaks), max(y.breaks) ) ) +
    scale_y_continuous( breaks = y.breaks )
  
  if ( .writePlot == TRUE ) {
    
    my_ggsave( name = paste(.Yname, "_plot.pdf", sep=""),
               .plot = p,
               .width = 10,
               .height = 14,
               .results.dir = ..results.dir,
               .overleaf.dir = ..overleaf.dir )
    
    # how to save a plotly as html
    # https://www.biostars.org/p/458325/
    pl = ggplotly(p)
    setwd(..results.dir)
    string = paste(.Yname, "plotly.html", sep="_")
    htmlwidgets::saveWidget(pl, string)
  }
  
  return(p)
  
}


# # this version is better for automatic plots not intended for the paper
# 
# # make a plot with 3 variables: x-axis, y-axis, facets, and colors
# # facet vars allowed be null
# quick_5var_agg_plot = function(.Xname,
#                                .Yname,
#                                .colorVarName,
#                                .facetVar1Name = NULL,
#                                .facetVar2Name = NULL,
#                                
#                                .dat,
#                                .ggtitle = "",
#                                
#                                .y.breaks = NULL,
# 
#                                
#                                .colors = NULL,
#                                
#                                .writePlot = FALSE,
#                                .results.dir = NULL) {
#   
#   
#   # # TEST
#   # .dat = agg
#   # .Xname = "N"
#   # .Yname = "BhatBias"
#   # .colorVarName = "method"
#   # .facetVar1Name = NULL
#   # .facetVar1Name = "missing.vars.pretty"
#   # .dat = agg
#   # .ggtitle = ""
#   # .writePlot = FALSE
#   # .y.breaks = NULL
#   # #.results.dir
#   
#   
#   .dat$Y = .dat[[.Yname]]
#   .dat$X = .dat[[.Xname]]
#   .dat$colorVar = .dat[[.colorVarName]]
#   # don't try to move these inside conditional statement below
#   #  about facet_wrap b/c then .dat won't contain the facet vars
#   if(!is.null(.facetVar1Name)) .dat$facetVar1 = .dat[[.facetVar1Name]]
#   if(!is.null(.facetVar2Name)) .dat$facetVar2 = .dat[[.facetVar2Name]]
#   
# 
#   # ~ Make base plot ----------
#   p = ggplot( data = .dat,
#               aes( x = X,
#                    y = Y,
#                    color = as.factor(colorVar) ) ) +
#     
#     geom_point() +
#     geom_line(position=position_dodge(width=0.2)) +
#     
#     # use all values of
#     #scale_x_log10( breaks = unique(.dp$n) )
#     # use only some values
#     #scale_x_log10( breaks = c(500, 1000) ) +
#     
#     xlab(.Xname) +
#     ylab(.Yname) +
#     guides( color = guide_legend(title = .colorVarName) ) +
#     ggtitle(.ggtitle) +
#     theme_bw() 
#   
#   # ~ Add reference lines ----------
#   if ( str_contains(x = .Yname, pattern = "Cover") ) {
#     p = p + geom_hline( yintercept = 0.95,
#                         lty = 2,
#                         color = "black" ) 
#     
#   }
#   
#   if ( str_contains(x = .Yname, pattern = "Bias") ) {
#     p = p + geom_hline( yintercept = 0,
#                         lty = 2,
#                         color = "black" ) 
#     
#   }
#   
# 
#   # ~ Add facetting ----------
#   # this block needs to be after adding geom_hlines so that the lines obey the facetting
#   if ( !is.null(.facetVar1Name) & !is.null(.facetVar2Name) ) {
#     p = p + facet_wrap(facetVar1 ~ facetVar2,
#                        nrow = length( unique(.dat$facetVar1) ) ) 
#   }
#   
#   if ( !is.null(.facetVar1Name) & is.null(.facetVar2Name) ) {
#     p = p + facet_wrap("facetVar1",
#                        nrow = length( unique(.dat$facetVar1) ) ) 
#   }
#   
#   # ~ Custom colors ----------
#   if ( !is.null(.colors) ) {
#     p = p + scale_color_manual(values = .colors)
#   }
#   
#   # ~ Set X-axis scaling ----------
#   p = p + scale_x_continuous(breaks = unique(.dat$X),
#                              trans = "log") 
#   
#   # ~ Set Y-axis breaks ----------
#   # other outcomes follow rules or can just use default axis breaks
#   # y.breaks are only still null if none of the above applied
#   if ( is.null(.y.breaks) ) {
#     # set default breaks
#     if ( grepl(pattern = "Cover", .Yname) ){
#       y.breaks = seq(0.4, 1, .1)
#       
#     } else {
#       # otherwise keep the default limits from ggplot
#       y.breaks = ggplot_build(p)$layout$panel_params[[1]]$y$breaks
#     }
#   } # end "if ( is.null(.y.breaks) )"
#   
#   
#   # if user provided their own y.breaks
#   if ( !is.null(.y.breaks) ) {
#     y.breaks = .y.breaks
#   }
#   
#   
#   # use coord_cartesian so that lines/points that go outside limits look cut off
#   #  rather than completely omitted
#   p = p + coord_cartesian( ylim = c( min(y.breaks), max(y.breaks) ) ) +
#     scale_y_continuous( breaks = y.breaks )
#   
#   
#   if ( .writePlot == TRUE ) {
#     # my_ggsave( name = paste(.Yname, "_plot.pdf", sep=""),
#     #            .plot = p,
#     #            .width = 10,
#     #            .height = 10,
#     #            .results.dir = .results.dir,
#     #            .overleaf.dir = overleaf.dir.figs )
#     
#     # how to save a plotly as html
#     # https://www.biostars.org/p/458325/
#     #browser()
#     pl = ggplotly(p)
#     setwd(.results.dir)
#     string = paste(.Yname, "plotly.html", sep="_")
#     htmlwidgets::saveWidget(pl, string)
#   }
#   
#   return(p)
#   
# }
# 






# ~ FNS FOR SUMMARIZING SIMULATION DATA ----------------------------------------------


# fn for aggregating so we can look at different
#  iterate-level filtering rules
# .s: the iterate-level stitched data (not yet aggregated in any way)
# averagefn: fn to use when aggregating results across scenarios
# expected.sim.reps: only used for sanity checks
make_agg_data = function( .s,
                          expected.sim.reps = NA ){
  
  # # TEST ONLY
  # .s = s
  # badCoverageCutoff = 0.85
  # expected.sim.reps = NA
  
  
  # make unique scenario variable, defined as scen.name AND method
  if ( !"unique.scen" %in% names(.s) ) .s$unique.scen = paste(.s$scen.name, .s$method)
  
  ##### Outcome and Parameter Variables #####
  # "outcome" variables used in analysis
  analysis.vars = c( 
    "bhat",
    "bhat_lo",
    "bhat_hi",
    
    ##### variables to be created in mutate below:
    
    "BhatBias",
    "BhatCover",
    "BhatWidth",
    "BhatRMSE",
    "BhatEmpSE",
    
    # diagnostics regarding point estimation and CIs
    "BhatEstFail",
    "BhatCIFail",
    
    "pR_Emp_Mean"
  )
  
  
  
  
  # variables that define the scenarios
  param.vars = c("unique.scen",  
                 "method",
                 "model", "dag_name", "N", "betaAY", "betaCY", 
                 "betaAC", "betaCR", "betaQA", "betaQY", "betaQR",
                 "adj_form_string", "unadj_form_string")
  
  
  # sanity check to make sure we've listed all param vars
  t = .s %>% group_by_at(param.vars) %>% summarise(n())
  if ( !is.na(expected.sim.reps) ) {
    if ( max(t$`n()`) > expected.sim.reps ) stop("param.vars in make_agg_data might be missing something because grouping that way indicated some scenarios had more than expected.sim.reps")
  }
  
  
  ##### Overwrite Analysis Variables As Their Within-Scenario Means #####
  # organize variables into 3 mutually exclusive sets: 
  # - param.vars: parameter variables for grouping
  # - toDrop: variables to drop completely
  # - firstOnly: variables that are static within a scenario, for which we
  #   should just take the first one (but not param variables)
  # - takeMean: variables for which we should take the mean within scenarios

  toDrop = c("rep.methods",
             "get.CIs",
             "overall.error",
             "rep.name",
          
             "job.name",
             "sim.reps.actual")
  
  firstOnly = c("scen.name",
                "unique.scen")
  
  ##### Add New Variables Calculated at Scenario Level #####
  
  # if you have 10K iterates, script breaks from here forward if running locally
  # "vector memory limits"
  s2 = .s %>%
  
    # take just first entry of non-parameter variables that are static within scenarios
    group_by_at(param.vars) %>%
    mutate_at( firstOnly, 
               function(x) x[1] ) %>%
    
    # make certain ad hoc variables that don't conform to below rules
    # this step creates variables that are repeated for every rep within 
    #  a scenario, which is intentional
    
    # make variables that are calculated within scenarios
    # some of the vars are defined at the iterate level (i.e., they still vary within scen), 
    #  while others are calculated at the scen level (i.e., they are static within scen)
    # after this step, we take the means within scens of all these vars, which is immaterial
    #   for the ones that are already static within scenario
    group_by_at(param.vars) %>%
    
    mutate( sim.reps.actual = n(),
            
            # varies within scenario
            BhatBias = bhat - betaAY,
            BhatCover = covers(truth = betaAY, lo = bhat_lo, hi = bhat_hi),
            BhatWidth = bhat_hi - bhat_lo,
            
            # static within scenario
            BhatRMSE = sqrt( meanNA( (bhat - betaAY)^2 ) ),
            BhatEstFail = mean(is.na(bhat)),
            BhatCIFail = mean(is.na(bhat_lo)),
            BhatEmpSE = sd(bhat, na.rm = TRUE),
            pR_Emp_Mean = meanNA(pR_emp)

    ) 
  
  
  # now look for which variables should have their means taken
  # this step must happen here, after we've started making s2, 
  #  so that the takeMean vars are actually in s2
  ( takeMean = names(s2)[ !names(s2) %in% c(param.vars, toDrop, firstOnly) ] )
  # sanity check: have all variables been sorted into these categories?
  expect_equal( TRUE,
                all( names(s2) %in% c(param.vars, toDrop, firstOnly, takeMean) ) )
  
  
  ##### Aggregate to Scenario Level #####
  
  # calculate scenario-level averages, but keep dataset at the rep level
  #  for now to facilitate sanity checks
  # don't try to drop vars that don't exist
  toDrop = toDrop[ toDrop %in% names(s2) ]
  
  s3 = s2 %>%
    # take averages of numeric variables
    group_by_at(param.vars) %>%
    mutate_at( takeMean,
               function(x) meanNA(x) ) %>%
    select( -all_of(toDrop) )
  
  
  
  # sanity check: name mismatches
  trouble.vars = analysis.vars[ !analysis.vars %in% names(s2) ]
  if ( length( trouble.vars ) > 0 ) {
    stop( paste("Might have name mismatches; edit analysis.vars in make_agg_data; trouble vars are: ", trouble.vars ) )
  }
  
  # sanity check: SDs of all analysis variables should be 0 within unique scenarios
  t = data.frame( s3 %>% group_by(unique.scen) %>%
                    summarise_at( analysis.vars, sd ) )
  
  t = t %>% select(-unique.scen)
  expect_equal( FALSE,
                any( !as.matrix( t[, 2:(ncol(t)) ] ) %in% c(0, NA, NaN) ) )
  # end sanity checks
  
  
  ##### Aggregate Data at Scenario Level #####
  # make aggregated data by keeping only first row for each 
  #  combination of scenario name and calib.method
  agg = s3[ !duplicated(s3$unique.scen), ]
  
  # ##### Create Variables That Are Defined At Scenario Rather Than Iterate Level #####
  # agg = agg %>% mutate( BadMhatCover = MhatCover < badCoverageCutoff,
  #                       BadShatCover = ShatCover < badCoverageCutoff )
  # 
  # return(agg %>% ungroup() )
  
  return(agg)
}


# This is meant to be called after make_agg_data
# Can be run locally even when agg is huge
# This fn is separate from make_agg_data because it needs more frequent modification
wrangle_agg_local = function(agg) {
  ##### Make New Variables At Scenario Level ##### 
  
  # label methods more intelligently for use in plots
  agg$method.pretty = NA
  agg$method.pretty[ agg$method == c("CC-adj") ] = "CC-adjusted"
  agg$method.pretty[ agg$method == c("CC-unadj") ] = "CC-unadjusted"
  agg$method.pretty[ agg$method == c("MI-adj") ] = "MI-adjusted"
  agg$method.pretty[ agg$method == c("MI-unadj") ] = "MI-unadjusted"
  agg$method.pretty[ agg$method == c("IPW-adj") ] = "IPW-adjusted"
  agg$method.pretty[ agg$method == c("IPW-unadj") ] = "IPW-unadjusted" 
  table(agg$method, agg$method.pretty)
  
  
  agg$missing.vars.pretty = NA
  agg$missing.vars.pretty[ agg$missing_vars == c("c('A', 'Y')") ] = "A and Y missing"
  agg$missing.vars.pretty[ agg$missing_vars == c("c('A')") ] = "A missing"
  agg$missing.vars.pretty[ agg$missing_vars == c("c('Y')") ] = "Y missing"
  table(agg$missing_vars, agg$missing.vars.pretty)
  
  # MAR vs. MNAR
  # depends on both DAG and missing vars
  agg$missing.vars.MAR = NA
  agg$missing.vars.MAR[ agg$dag_name == "I(a)" & agg$missing_vars == c("c('A', 'Y')") ] = "(A,Y) missing; MNAR"
  agg$missing.vars.MAR[ agg$dag_name == "I(a)" & agg$missing_vars == c("c('A')") ] = "A missing; MNAR"
  agg$missing.vars.MAR[ agg$dag_name == "I(a)" & agg$missing_vars == c("c('Y')") ] = "Y missing; MAR"

  agg$missing.vars.MAR[ agg$dag_name == "I(b)" & agg$missing_vars == c("c('A', 'Y')") ] = "(A,Y) missing; MNAR"
  agg$missing.vars.MAR[ agg$dag_name == "I(b)" & agg$missing_vars == c("c('A')") ] = "A missing; MNAR"
  agg$missing.vars.MAR[ agg$dag_name == "I(b)" & agg$missing_vars == c("c('Y')") ] = "Y missing; MAR"
  
  agg$missing.vars.MAR[ agg$dag_name == "I(c)" & agg$missing_vars == c("c('A', 'Y')") ] = "(A,Y) missing; MAR"
  agg$missing.vars.MAR[ agg$dag_name == "I(c)" & agg$missing_vars == c("c('A')") ] = "A missing; MAR"
  agg$missing.vars.MAR[ agg$dag_name == "I(c)" & agg$missing_vars == c("c('Y')") ] = "Y missing; MAR"
  
  
  agg$missing.vars.MAR[ agg$dag_name == "II(a)" & agg$missing_vars == c("c('A', 'Y')") ] = "(A,Y) missing; MNAR"
  agg$missing.vars.MAR[ agg$dag_name == "II(a)" & agg$missing_vars == c("c('A')") ] = "A missing; MAR"
  agg$missing.vars.MAR[ agg$dag_name == "II(a)" & agg$missing_vars == c("c('Y')") ] = "Y missing; MNAR"
  
  agg$missing.vars.MAR[ agg$dag_name == "II(b)" & agg$missing_vars == c("c('A', 'Y')") ] = "(A,Y) missing; MAR"
  agg$missing.vars.MAR[ agg$dag_name == "II(b)" & agg$missing_vars == c("c('A')") ] = "A missing; MAR"
  agg$missing.vars.MAR[ agg$dag_name == "II(b)" & agg$missing_vars == c("c('Y')") ] = "Y missing; MAR"
  
  agg$missing.vars.MAR[ agg$dag_name == "II(c)" & agg$missing_vars == c("c('A', 'Y')") ] = "(A,Y) missing; MAR"
  agg$missing.vars.MAR[ agg$dag_name == "II(c)" & agg$missing_vars == c("c('A')") ] = "A missing; MAR"
  agg$missing.vars.MAR[ agg$dag_name == "II(c)" & agg$missing_vars == c("c('Y')") ] = "Y missing; MAR"
  
  agg$dag.name.pretty = paste( "DAG", agg$dag_name, sep = " "  )
 
  return(agg)
}


# SMALL GENERIC HELPERS ---------------------


# one or both dirs can be NA
my_ggsave = function(name,
                     .plot = last_plot(),
                     .width,
                     .height,
                     .results.dir = results.dir,
                     .overleaf.dir = overleaf.dir) {
  
  dirs = c(.results.dir, .overleaf.dir)
  dirIsNA = sapply(dirs, is.na)
  validDirs = dirs[ !dirIsNA ]
  
  
  for ( dir in validDirs ) {
    setwd(dir)
    ggsave( name,
            plot = .plot,
            width = .width,
            height = .height,
            device = "pdf" )
  }
}


# quickly look at results when running doParallel locally
srr = function(.rep.res) {
  
  
  cat("\n")
  print( .rep.res %>%
           mutate_if(is.numeric, function(x) round(x,2)) )
  cat("\n")
  
}

# quick mean with NAs removed
meanNA = function(x){
  mean(x, na.rm = TRUE)
}


# check CI coverage
covers = function( truth, lo, hi ) {
  return( as.numeric( (lo <= truth) & (hi >= truth) ) )
}

# get names of dataframe containing a string
namesWith = function(pattern, dat){
  names(dat)[ grepl(pattern = pattern, x = names(dat) ) ]
}


# quick length(unique)
nuni = function(x) {
  length(unique(x))
}

# (re-)install package AND its dependencies
# useful for stupid rstan issues in which rstan itself it UTD but not its dependencies
# https://stackoverflow.com/questions/21010705/update-a-specific-r-package-and-its-dependencies
instPkgPlusDeps <- function(pkg, install = FALSE,
                            which = c("Depends", "Imports", "LinkingTo"),
                            inc.pkg = TRUE) {
  stopifnot(require("tools")) ## load tools
  ap <- available.packages() ## takes a minute on first use
  ## get dependencies for pkg recursively through all dependencies
  deps <- package_dependencies(pkg, db = ap, which = which, recursive = TRUE)
  ## the next line can generate warnings; I think these are harmless
  ## returns the Priority field. `NA` indicates not Base or Recommended
  pri <- sapply(deps[[1]], packageDescription, fields = "Priority")
  ## filter out Base & Recommended pkgs - we want the `NA` entries
  deps <- deps[[1]][is.na(pri)]
  ## install pkg too?
  if (inc.pkg) {
    deps = c(pkg, deps)
  }
  ## are we installing?
  if (install) {
    install.packages(deps)
  }
  deps ## return dependencies
}

# example
# instPkgPlusDeps("fields")


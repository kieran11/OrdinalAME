Untitled
================

The `R` package `margins` provides average marginal effects for any
model developed with `lm` or `glm`, among many others. However,
`margins` does not work with `brms`. Jack O’Bailey developed two
functions for `brms`
[here](https://gist.github.com/jackobailey/0982c89326c36b12d6fa6d6f182189be).
There are two functions in this text file. The first function gets the
average marginal effects for continuous independent variables. The
second function get the average marginal effects for categorical
variables. The link compares the results of the two functions with the
`margins` command from the package `margins`. If you run the example,
you can see that the results are remarkably similar.

Using the code above, this post attempts to recreate the functionality
of the `margins` command from the `margins` package with ordinal
logistic regression models.

The adapted function is below:

``` r
bayes_dydx_ordinal.factor <- function(model, data = NULL, variable, re_formula = NULL){
  
  # Get data from model where data = NULL
  if(is.null(data) == T){
    d <- model$data
  } else {
    d <- data
  }

  
  # Get outcome from model
  resp <- model$formula$resp
  resp_ord <- model$formula$resp
  # Omit outcome from data
  d <-
    d %>%
    dplyr::select(-resp)
  
  d <-
    d %>% 
    group_by_all() %>% 
    count(name = "w") %>% 
    ungroup()
  

  # Get factor levels
  levs <- levels(as.factor(d[[variable]]))
  base <- levs[1L]
  cont <- levs[-1L]
  
  
  # Create empty list for fitted draws
  f <- list()
  
  # For each list add fitted draws
  for (i in seq_along(levs)){
    
    # Fix variable in each list to factor level
    d[[variable]] <- levs[i]
    
    f[[i]] <- 
      d %>% 
      add_fitted_draws(model = model,
                       re_formula = NULL,
                       value = "eff") %>% 
      ##### Need to include ordered within response variable
      group_by_at(vars(".draw", ".category")) %>% 
      #group_by_at(vars(".draw")) %>% 
      summarise(
        # eff = sum(eff),
        # w = sum(w),
        # eff_w_a = sum(eff*w),
        eff_w = sum(eff * w)/sum(w)) %>% 
      #dplyr::select(eff_w) %>% 
      #dplyr::select(eff_w, w, eff, eff_w_a) %>% 
      ungroup() %>% 
      select(-.draw)
    
    # Compute contrast if not base level
    if (i > 1){
      f[[i]]$eff_w <- f[[i]]$eff_w - f[[1]][[2]] #### Change here because also included the order of response
    }
    
    # # Rename column
    names(f[[i]]) <- c(paste0("Category", levs[i]), levs[i])
    #names(f[[i]]) <- levs[i]
    
  }

  # Remove data frame
  d <- NULL
  
  # Create output object
  out <- do.call(cbind, f)

  # Return AMEs
  if (length(cont) == 1){
    
    out <- out %>% tibble() %>% select(1,cont)
    names(out)[2] <- "est"
    
    out %>% 
      mutate(
        var = variable,
        resp = cont
      ) %>% 
      dplyr::select(Category=1,var, resp, est)
    
  } else {
    
    out %>% 
      select(1, cont) %>% 
      tidyr::pivot_longer(cols = -1 ,
                          names_to = "variable", values_to = "value") %>%
      rename(resp = variable, est = value,
             Category = 1)
    
    
  }
  
} ### Close Function
```

The example is from
[here](https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/).

Below is the example for `margins`.

We next run the ordinal Bayesian model using `brms`. The code is below:

``` r
# Fit Bayesian model
bayes_ordinal <- brm(formula = health ~ female + black + age,
             family=cumulative("logit"),
             prior = c(prior(normal( 0, 1), class = b)),
             data = nhanes2_a,
            warmup = 1000, 
            iter = 2000, 
             chains = 2,
             cores = 2)

AMEByVar = function(x,y) {
  
 ParedEst = bayes_dydx_ordinal.factor(bayes_ordinal , nhanes2_a, variable = x ) %>% 
    group_by(Category, resp) %>% 
    summarise(p50 = median(est)) %>% 
    ungroup() %>% 
    mutate(Var = x)

}
 
BayesAME = purrr::map(as.list(c("black", "female")), AMEByVar) %>% 
  bind_rows(.)
```

As a first step, we can compare the log odds ratios between the Bayesian
model with normally distributed priors and the previous model with flat
priors:

As we can see, the coefficients are of similar value.

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#zpcvshgtjk .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zpcvshgtjk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zpcvshgtjk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zpcvshgtjk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zpcvshgtjk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zpcvshgtjk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zpcvshgtjk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zpcvshgtjk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zpcvshgtjk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zpcvshgtjk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zpcvshgtjk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zpcvshgtjk .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#zpcvshgtjk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zpcvshgtjk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zpcvshgtjk .gt_from_md > :first-child {
  margin-top: 0;
}

#zpcvshgtjk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zpcvshgtjk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zpcvshgtjk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#zpcvshgtjk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zpcvshgtjk .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#zpcvshgtjk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zpcvshgtjk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zpcvshgtjk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zpcvshgtjk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zpcvshgtjk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#zpcvshgtjk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zpcvshgtjk .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#zpcvshgtjk .gt_left {
  text-align: left;
}

#zpcvshgtjk .gt_center {
  text-align: center;
}

#zpcvshgtjk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zpcvshgtjk .gt_font_normal {
  font-weight: normal;
}

#zpcvshgtjk .gt_font_bold {
  font-weight: bold;
}

#zpcvshgtjk .gt_font_italic {
  font-style: italic;
}

#zpcvshgtjk .gt_super {
  font-size: 65%;
}

#zpcvshgtjk .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="zpcvshgtjk" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Coefficient Comparison

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

term

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3">

<span class="gt_column_spanner">Bayesian Model Estimates</span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3">

<span class="gt_column_spanner">Flat Prior Model Estimates</span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Q2.5

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Q97.5

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

conf.low

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

conf.high

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

female1

</td>

<td class="gt_row gt_right">

\-0.117

</td>

<td class="gt_row gt_right">

\-0.185

</td>

<td class="gt_row gt_right">

\-0.051

</td>

<td class="gt_row gt_right">

\-0.117

</td>

<td class="gt_row gt_right">

\-0.187

</td>

<td class="gt_row gt_right">

\-0.047

</td>

</tr>

<tr>

<td class="gt_row gt_left">

black1

</td>

<td class="gt_row gt_right">

\-0.881

</td>

<td class="gt_row gt_right">

\-0.997

</td>

<td class="gt_row gt_right">

\-0.766

</td>

<td class="gt_row gt_right">

\-0.884

</td>

<td class="gt_row gt_right">

\-0.999

</td>

<td class="gt_row gt_right">

\-0.770

</td>

</tr>

<tr>

<td class="gt_row gt_left">

age

</td>

<td class="gt_row gt_right">

\-0.041

</td>

<td class="gt_row gt_right">

\-0.043

</td>

<td class="gt_row gt_right">

\-0.039

</td>

<td class="gt_row gt_right">

\-0.041

</td>

<td class="gt_row gt_right">

\-0.043

</td>

<td class="gt_row gt_right">

\-0.039

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

We next use the adapted function `bayes_dydx_ordinal.factor` to compare
the marginal effects.

Below we compare the marginal effects from the
`bayes_dydx_ordinal.factor` function to the `margins` function.
Importantly, there is no documentation on how `margins` works with
ordinal regressions. There is great documentation
[here](https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html)
and a great technical
[paper](https://cran.r-project.org/web/packages/margins/margins.pdf),
but neither covers ordinal regression.

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#effrybtziz .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#effrybtziz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#effrybtziz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#effrybtziz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#effrybtziz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#effrybtziz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#effrybtziz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#effrybtziz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#effrybtziz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#effrybtziz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#effrybtziz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#effrybtziz .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#effrybtziz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#effrybtziz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#effrybtziz .gt_from_md > :first-child {
  margin-top: 0;
}

#effrybtziz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#effrybtziz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#effrybtziz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#effrybtziz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#effrybtziz .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#effrybtziz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#effrybtziz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#effrybtziz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#effrybtziz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#effrybtziz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#effrybtziz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#effrybtziz .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#effrybtziz .gt_left {
  text-align: left;
}

#effrybtziz .gt_center {
  text-align: center;
}

#effrybtziz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#effrybtziz .gt_font_normal {
  font-weight: normal;
}

#effrybtziz .gt_font_bold {
  font-weight: bold;
}

#effrybtziz .gt_font_italic {
  font-style: italic;
}

#effrybtziz .gt_super {
  font-size: 65%;
}

#effrybtziz .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="effrybtziz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal" style>

Average Marginal Effects Comparison

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Health Status

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Response

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">

<span class="gt_column_spanner">Average Marginal Effects</span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

AME

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

AME\_Bayes

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

poor

</td>

<td class="gt_row gt_left">

black

</td>

<td class="gt_row gt_right">

0.073849996

</td>

<td class="gt_row gt_right">

0.073619180

</td>

</tr>

<tr>

<td class="gt_row gt_left">

poor

</td>

<td class="gt_row gt_left">

female

</td>

<td class="gt_row gt_right">

0.007427055

</td>

<td class="gt_row gt_right">

0.007393476

</td>

</tr>

<tr>

<td class="gt_row gt_left">

fair

</td>

<td class="gt_row gt_left">

black

</td>

<td class="gt_row gt_right">

0.073849996

</td>

<td class="gt_row gt_right">

0.092264582

</td>

</tr>

<tr>

<td class="gt_row gt_left">

fair

</td>

<td class="gt_row gt_left">

female

</td>

<td class="gt_row gt_right">

0.007427055

</td>

<td class="gt_row gt_right">

0.011796992

</td>

</tr>

<tr>

<td class="gt_row gt_left">

average

</td>

<td class="gt_row gt_left">

black

</td>

<td class="gt_row gt_right">

0.073849996

</td>

<td class="gt_row gt_right">

0.020769338

</td>

</tr>

<tr>

<td class="gt_row gt_left">

average

</td>

<td class="gt_row gt_left">

female

</td>

<td class="gt_row gt_right">

0.007427055

</td>

<td class="gt_row gt_right">

0.006539995

</td>

</tr>

<tr>

<td class="gt_row gt_left">

good

</td>

<td class="gt_row gt_left">

black

</td>

<td class="gt_row gt_right">

0.073849996

</td>

<td class="gt_row gt_right">

\-0.067437263

</td>

</tr>

<tr>

<td class="gt_row gt_left">

good

</td>

<td class="gt_row gt_left">

female

</td>

<td class="gt_row gt_right">

0.007427055

</td>

<td class="gt_row gt_right">

\-0.006814134

</td>

</tr>

<tr>

<td class="gt_row gt_left">

excellent

</td>

<td class="gt_row gt_left">

black

</td>

<td class="gt_row gt_right">

0.073849996

</td>

<td class="gt_row gt_right">

\-0.119175235

</td>

</tr>

<tr>

<td class="gt_row gt_left">

excellent

</td>

<td class="gt_row gt_left">

female

</td>

<td class="gt_row gt_right">

0.007427055

</td>

<td class="gt_row gt_right">

\-0.018958325

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

As we can see the marginal effects from the `margins` package do not
match `bayes_dydx_ordinal.factor`. However, the `margins` estimates do
not vary by outcome level. They are very similar at the first level, or
health status equal to poor.

### Looking at STATA

STATA users seem to use average marginal effects more. This forum
details the general process of average marginal
effects()\[<https://www.statalist.org/forums/forum/general-stata-discussion/general/1465336-ordered-probit-marginal-effects>\].

The basic idea is for each level of your dependent variable there should
be a separate average marginal effect. The interpretation is then a one
unit change in x is associated with a given probability of being in a
category.

There is a paper that use the same `nhanes2` dataset and provide average
marginal effects by each outcome. The paper is
[here](https://www3.nd.edu/~rwilliam/stats3/Margins05.pdf).

The third page provides the comparison. I put them into a table and
compared them with the `bayes_dydx_ordinal.factor`. The paper only
provides the average marginal effects for the independent variable
`black`.

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ylbjoidyuu .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ylbjoidyuu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ylbjoidyuu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ylbjoidyuu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ylbjoidyuu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ylbjoidyuu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ylbjoidyuu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ylbjoidyuu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ylbjoidyuu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ylbjoidyuu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ylbjoidyuu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ylbjoidyuu .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ylbjoidyuu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ylbjoidyuu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ylbjoidyuu .gt_from_md > :first-child {
  margin-top: 0;
}

#ylbjoidyuu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ylbjoidyuu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ylbjoidyuu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ylbjoidyuu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ylbjoidyuu .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ylbjoidyuu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ylbjoidyuu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ylbjoidyuu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ylbjoidyuu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ylbjoidyuu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ylbjoidyuu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ylbjoidyuu .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ylbjoidyuu .gt_left {
  text-align: left;
}

#ylbjoidyuu .gt_center {
  text-align: center;
}

#ylbjoidyuu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ylbjoidyuu .gt_font_normal {
  font-weight: normal;
}

#ylbjoidyuu .gt_font_bold {
  font-weight: bold;
}

#ylbjoidyuu .gt_font_italic {
  font-style: italic;
}

#ylbjoidyuu .gt_super {
  font-size: 65%;
}

#ylbjoidyuu .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="ylbjoidyuu" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="3" class="gt_heading gt_title gt_font_normal" style>

Average Marginal Effects Comparison - 2

</th>

</tr>

<tr>

<th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Health Status

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

AME

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

p50

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

poor

</td>

<td class="gt_row gt_right">

0.074

</td>

<td class="gt_row gt_right">

0.074

</td>

</tr>

<tr>

<td class="gt_row gt_left">

fair

</td>

<td class="gt_row gt_right">

0.093

</td>

<td class="gt_row gt_right">

0.092

</td>

</tr>

<tr>

<td class="gt_row gt_left">

average

</td>

<td class="gt_row gt_right">

0.021

</td>

<td class="gt_row gt_right">

0.021

</td>

</tr>

<tr>

<td class="gt_row gt_left">

good

</td>

<td class="gt_row gt_right">

\-0.068

</td>

<td class="gt_row gt_right">

\-0.067

</td>

</tr>

<tr>

<td class="gt_row gt_left">

excellent

</td>

<td class="gt_row gt_right">

\-0.120

</td>

<td class="gt_row gt_right">

\-0.119

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

As we can see, our estimates line up close to the estimates provided by
Richard Williams’ paper.

### Changes to the function:

The changes to the function are very few:

The first change results from `add_fitted_draws`. When we use this
function with an ordinal regression model, it adds a variable to the
output. The variable is called `.category` which represents the levels
of the dependent variable. The simple alteration can be seen below:

The first code block shows the previous function:

The change is simple:

  - From: `group_by_at(vars( ".category"))`
  - To: `group_by_at(vars(".draw", ".category"))`

This allows us to get a predicted draws from the data used to model.

``` r
 f[[i]] <- 
      d %>% 
      add_fitted_draws(model = model,
                       re_formula = NULL,
                       value = "eff") %>% 
      ##### Need to include ordered within response variable
      group_by_at(vars(".draw", ".category")) %>% 
      #group_by_at(vars(".draw")) %>% 
      summarise(
        # eff = sum(eff),
        # w = sum(w),
        # eff_w_a = sum(eff*w),
        eff_w = sum(eff * w)/sum(w)) %>% 
      #dplyr::select(eff_w) %>% 
      #dplyr::select(eff_w, w, eff, eff_w_a) %>% 
      ungroup() %>% 
      select(-.draw)
    
    # Compute contrast if not base level
    if (i > 1){
      f[[i]]$eff_w <- f[[i]]$eff_w - f[[1]][[2]] #### Change here because also included the order of response
    }
```

The contrast code also needs to change as the `data.frame` `f[[1]]` now
has two columns. The first column represents the category of outcome
variable.

There are other changes regarding the format of the final output, but
they are simple small changes using `dplyr`.

### Interpretation:

The interpretation of the average marginal effects for an ordinal model,
as discussed earlier is a one-unit change in the independent variable is
associated with a x% change of being in a given category.

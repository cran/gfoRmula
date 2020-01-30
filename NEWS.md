Package Updates
---------------

### Changes in Version 0.3.0 (2020-01-30)

-   Added wrapper function called `gformula()` for the
    `gformula_survival()`, `gformula_continuous_eof()`, and
    `gformula_binary_eof()` functions. Users should now use the more
    general `gformula()` function to apply the g-formula.
-   Added option for users to specify the values for lags at
    pre-baseline times by including rows at time -1, -2, …, -i.
-   Added an example data set called `continuous_eofdata_pb`, which
    illustrates how to prepare a data set with pre-baseline times
-   Added option for users to pass in “control parameters” (e.g.,
    maximum number of iterations, maxit, in glm.control) when fitting
    models for time-varying covariates via the `covparams$control`
    argument. (Thanks to @jerzEG for the suggestion)
-   Added option for users to access the fitted models for the
    time-varying covariates, outcome, and competing event (if
    applicable). See `model_fits` argument of the `gformula()` function
-   Added simulated data under the natural course to the `sim_data`
    component of the output of the `gformula()` function
-   Added a progress bar for the number of bootstrap samples completed.
    See the `show_progress` argument of the `gformula()` function for
    further details
-   Added `summary()`, `coef()`, and `vcov()` S3 methods for objects of
    class ‘gformula’
-   Added argument `fits` in the `print.gformula_survival()`,
    `print.gformula_continuous_eof()`, and `print.gformula_binary_eof()`
    functions. Added argument `all_times` in the
    `print.gformula_survival()` function
-   Fixed minor bug in the `lagavg()` function
-   Fixed bug occuring when not using lags of the intervention
    variable(s)
-   Fixed bug occuring in the truncation beyond covariate ranges.
    (Thanks to Louisa Smith)
-   Updates to the documentation

### Changes in Version 0.2.1 (2019-08-24)

-   First version released on CRAN
    (<https://CRAN.R-project.org/package=gfoRmula>)
-   Updates to the documentation

### Changes in Version 0.2.0 (2019-08-22)

-   Removed `example_intervention1()`, `example_intervention2()`, and
    `visit_sum_orig()`, as these functions are not used internally and
    users should not directly apply them
-   Removed export of `visit_sum()` and `natural()`, as these functions
    are used internally and users should not directly apply them
-   Updates to the documentation

### Changes in Version 0.1.1 (2019-08-21)

-   Minor updates to the documentation

### Changes in Version 0.1.0 (2019-08-17)

-   First version released on GitHub
    (<https://github.com/CausalInference/gfoRmula>)

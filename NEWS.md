# y2municipal 0.2.3
## New features
* `methodology()` function now shows the median length of interview (LOI) along with other methodology details

# y2municipal 0.2.2
## Bug fixes
* base_ns was not calculating correctly.


# y2municipal 0.2.1
## new features
* `topline_freqs()` & `topline_appendix()` - added an error message for when no variables are provided or when none of the variables provided have the proper y2 prefixes. 


# y2municipal 0.2.0
## Breaking changes
* Dropping two functions: *topline()* and *read_data_names_fonts()*
* Updated Easter egg functions. 


# y2municipal 0.1.3
## new features
* `topline_freqs()` Has a new argument *silently* which hides message output (e.g., progress of completing freqs on variables or printing of variables not included in the topline). 
* `topline_freqs()` Has a new column in the output: *base_ns* which notes the number of observations that had a response for any given question, or set of questions (for multiple select blocks). This new column is used by Internbot in its calculations.  


# y2municipal 0.1.1
## new functions
* `topline()` split into two functions: `topline_appendix()` and `topline_freqs()`


# y2municipal 0.1.0
## bug fixes
* `topline()` - fixed a bug where topline() wouldn't run.


# y2municipal 0.0.6
## new features
* `topline()` - now adds in prompt/label for numeric/ranked questions. 
## bug fixes
* `topline()` - fixed a bug where topline() wouldn't run without a weight_var.


# y2municipal 0.0.5
## bug fixes
* `topline()` - now categorizes max diff (md_) questions as multiple select. Needed to switch to multiple select instead of single select because NA category often still means respondent sees the question; they just haven't chosen it as top/bot.


# y2municipal 0.0.3
## bug fixes
* `methodology()`  - MOE now always shows 2 digits, even if both are 0 (e.g. MOE: +/- 9.00)
* `topline()` - now runs even if there are no variables with the proper prefixes. Still gives a warning for variables not shown in the topline. 


# y2municipal 0.0.2
## New Functions
* `methodology()`  Get the following survey methodology information from a data frame:
1. Length of interview (LOI)
2. Fielding dates
3. Survey margin of error (MOE)
* Easter egg functions, don't worry about it

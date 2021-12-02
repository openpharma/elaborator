## Notes

### Second submission Version 1.1

> Missing Rd-tags left in:
      elaborator_draw_boxplot_color.Rd: \value
      elaborator_perform_unlist.Rd: \value
      elaborator_server.Rd: \arguments,  \value

We renamed the function 'elaborator_draw_boxplot_color' into 'elaborator_draw_scheme_preview' and
renamed function 'elaborator_perform_unlist' into 'elaborator_derive_equal_values'. 
The files 'elaborator_perform_unlist.Rd' and 'elaborator_draw_boxplots_color.Rd' were removed.

Missing tags in 'elaborator_server.Rd' and 'elaborator_ui.Rd' were added.

> Have the issues why your package was archived been fixed?
Please explain this in the submission comments.

The issue has been fixed. The packages failed checks, because of an update of the R package 'shinyjs'.
With the update to shinyjs version 2.0.0, the parameter 'function' in the 'extendShinyjs()'-function is required.
For more details see the NEWS file of the shinyjs package: 
2.0.0 (2020-08-24)
*BREAKING CHANGE When using extendShinyjs(), the functions parameter must always be provided.

We removed all functions from 'shinyjs' and also the package dependency in elaborator version 1.1. 

### First submission Version 1.1

> If there are references describing the methods in your package, please 
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for 
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

There are no references to include regarding this package so far.

>Please rather use the Authors@R field and declare Maintainer, Authors 
and Contributors with their appropriate roles with person() calls.
e.g. something like:
Authors@R: c(person("Alice", "Developer", role = c("aut", "cre","cph"),
                      email = "alice.developer@some.domain.net"),
               person("Bob", "Dev", role = "aut") )

Changed DESCRIPTION file as described.

> Please add \value to .Rd files regarding exported methods and explain 
the functions results in the documentation. Please write about the 
structure of the output (class) and also what the output means. (If a 
function does not return a value, please document that too, e.g. 
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
e.g.:
      elaborator_calculate_color_index.Rd: \value
      elaborator_calculate_pattern_number.Rd: \value
      elaborator_calculate_pattern.Rd: \value
      elaborator_calculate_ref_pattern.Rd: \value
      elaborator_calculate_spearman_distance.Rd: \value
      elaborator_derive_test_values.Rd: \value
      elaborator_draw_boxplot_color.Rd: \value
      elaborator_draw_curved_line.Rd: \value
...

Added all missing tags to the functions of the package.

> Functions which are supposed to only run interactively (e.g. shiny) 
should be wrapped in if(interactive()).
Please replace \dontrun{} with if(interactive()){} if possible, then 
users can see that the functions are not intended for use in scripts.

Wrapped example in if(interactive()) rather than \dontrun{}.

## Test environments
* local OS X install, R 4.0.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Steffen Jeske <steffen.jeske.ext@bayer.com>’


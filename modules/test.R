################################################################################-
# ----- Description -------------------------------------------------------------
#
# description
#
# ------------------------------------------------------------------ #
# Authors@R: author
# Date: year/month
#

################################################################################-
# ----- Libraries ---------------------------------------------------------------



################################################################################-
# ----- Module UI -----

module_module_function_ui <- function(id) {
  ns <- NS(id)

  
}

################################################################################-
# ----- Module Server -----

module_module_function_server <- function(id, data, external_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    help_func <- module_module_function_functions

    
  })
}

################################################################################-
# ----- Generate Testdata -----

module_module_function_testdata <- list(

    last_location = "",
    encrypted_with = "",
    data = structure(list())

)

################################################################################-
# ----- Helping Functions -----

module_module_function_functions <- list(

  
)
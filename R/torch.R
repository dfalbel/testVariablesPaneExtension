ark_variable_display_value.torch_tensor <- function(x, ...) {
  torch:::make_str_torch_tensor(x)
}

ark_variable_display_value.nn_module <- function(x, ...) {
  paste0(
    "An `nn_module` containing ",
    scales::comma(torch:::get_parameter_count(attr(x, "module"))),
    " parameters."
  )
}

ark_variable_get_children.nn_module <- function(x, ...) {
  x$parameters
}

ark_variable_has_children.nn_module <- function(x, ...) {
  TRUE
}
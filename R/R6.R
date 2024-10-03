
Person <- R6::R6Class("Person",
  public = list(
    name = NULL,
    initialize = function(name) {
      self$name <- name
    },
    greet = function() {
      cat("Hello ", name)
    }
  ),
  active = list(
    name2 = function() {
      paste0("Hello ", name)
    }
  ),
  private = list(
    myPrivateMethod = function() {
      cat("I'm private\n")
    },
    privatefield = 1
  )
)

ark_variable_display_value.R6 <- function(x, ...) {
  paste0("R6 object with class ", class(x)[1])
}

ark_variable_has_children.R6 <- function(x) {
  TRUE
}

ark_variable_get_children.R6 <- function(x) {
  nms <- ls(x)
  children <- nms |> 
    lapply(function(name) {
      if (rlang::env_binding_are_active(x, name)) {
        new_mask("Active Binding", FALSE)
      } else if (rlang::env_binding_are_lazy(x, name)) {
        new_mask("Lazy Binding", FALSE)
      } else {
        x[[name]]
      }
    })
  
  names(children) <- nms
  children <- children[!sapply(children, is.function)]

  children <- append(children, list(methods = new_mask("Methods", TRUE)))
  if ("private" %in% ls(x[[".__enclos_env__"]])) {
    children <- append(children, list(private = new_mask("Private", TRUE)))
  }
  children
}

ark_variable_get_child_at.R6 <- function(x, ..., name, index) {
  if (!is.null(name) && name == "methods") {
    nms <- ls(x) |> 
      purrr::discard(function(nm) {
        rlang::env_binding_are_active(x, nm) ||
          rlang::env_binding_are_lazy(x, nm) ||
          !is.function(x[[nm]])
      })
    print(nms)
    return(rlang::env_get_list(x, nms))
  }

  if (!is.null(name) && name == "private") {
    private_env <- x[[".__enclos_env__"]][["private"]]
    nms <- ls(private_env)
    children <- rlang::env_get_list(private_env, nms)
    return(children)
  }

  x[[name]]
}


new_mask <- function(display_value, has_children) {
  structure(
    list(display_value = display_value, has_children = has_children),
    class = "mask"
  )
}

ark_variable_display_value.mask <- function(x, ...) {
  x$display_value
}

ark_variable_has_children.mask <- function(x, ...) {
  x$has_children
}

ark_variable_display_type.mask <- function(x, ...) {
  ""
}

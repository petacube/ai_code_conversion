source("print_debug.R")
#' Pivot data from long to wide
#
#' @description
#' `pivot_wider()` "widens" data, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' [pivot_longer()].
#'
#' Learn more in `vignette("pivot")`.
#'
#' @details
#' `pivot_wider()` is an updated approach to [spread()], designed to be both
#' simpler to use and to handle more use cases. We recommend you use
#' `pivot_wider()` for new code; `spread()` isn't going away but is no longer
#' under active development.
#'
#' @seealso [pivot_wider_spec()] to pivot "by hand" with a data frame that
#'   defines a pivoting specification.
#' @inheritParams pivot_longer
#' @param id_cols <[`tidy-select`][tidyr_tidy_select]> A set of columns that
#'   uniquely identify each observation. Typically used when you have
#'   redundant variables, i.e. variables whose values are perfectly correlated
#'   with existing variables.
#'
#'   Defaults to all columns in `data` except for the columns specified through
#'   `names_from` and `values_from`. If a tidyselect expression is supplied, it
#'   will be evaluated on `data` after removing the columns specified through
#'   `names_from` and `values_from`.
#' @param id_expand Should the values in the `id_cols` columns be expanded by
#'   [expand()] before pivoting? This results in more rows, the output will
#'   contain a complete expansion of all possible values in `id_cols`. Implicit
#'   factor levels that aren't represented in the data will become explicit.
#'   Additionally, the row values corresponding to the expanded `id_cols` will
#'   be sorted.
#' @param names_from,values_from <[`tidy-select`][tidyr_tidy_select]> A pair of
#'   arguments describing which column (or columns) to get the name of the
#'   output column (`names_from`), and which column (or columns) to get the
#'   cell values from (`values_from`).
#'
#'   If `values_from` contains multiple values, the value will be added to the
#'   front of the output column.
#' @param names_sep If `names_from` or `values_from` contains multiple
#'   variables, this will be used to join their values together into a single
#'   string to use as a column name.
#' @param names_prefix String added to the start of every variable name. This is
#'   particularly useful if `names_from` is a numeric vector and you want to
#'   create syntactic variable names.
#' @param names_glue Instead of `names_sep` and `names_prefix`, you can supply
#'   a glue specification that uses the `names_from` columns (and special
#'   `.value`) to create custom column names.
#' @param names_sort Should the column names be sorted? If `FALSE`, the default,
#'   column names are ordered by first appearance.
#' @param names_vary When `names_from` identifies a column (or columns) with
#'   multiple unique values, and multiple `values_from` columns are provided,
#'   in what order should the resulting column names be combined?
#'
#'   - `"fastest"` varies `names_from` values fastest, resulting in a column
#'     naming scheme of the form: `value1_name1, value1_name2, value2_name1,
#'     value2_name2`. This is the default.
#'
#'   - `"slowest"` varies `names_from` values slowest, resulting in a column
#'     naming scheme of the form: `value1_name1, value2_name1, value1_name2,
#'     value2_name2`.
#' @param names_expand Should the values in the `names_from` columns be expanded
#'   by [expand()] before pivoting? This results in more columns, the output
#'   will contain column names corresponding to a complete expansion of all
#'   possible values in `names_from`. Implicit factor levels that aren't
#'   represented in the data will become explicit. Additionally, the column
#'   names will be sorted, identical to what `names_sort` would produce.
#' @param values_fill Optionally, a (scalar) value that specifies what each
#'   `value` should be filled in with when missing.
#'
#'   This can be a named list if you want to apply different fill values to
#'   different value columns.
#' @param values_fn Optionally, a function applied to the value in each cell
#'   in the output. You will typically use this when the combination of
#'   `id_cols` and `names_from` columns does not uniquely identify an
#'   observation.
#'
#'   This can be a named list if you want to apply different aggregations
#'   to different `values_from` columns.
#' @param unused_fn Optionally, a function applied to summarize the values from
#'   the unused columns (i.e. columns not identified by `id_cols`,
#'   `names_from`, or `values_from`).
#'
#'   The default drops all unused columns from the result.
#'
#'   This can be a named list if you want to apply different aggregations
#'   to different unused columns.
#'
#'   `id_cols` must be supplied for `unused_fn` to be useful, since otherwise
#'   all unspecified columns will be considered `id_cols`.
#'
#'   This is similar to grouping by the `id_cols` then summarizing the
#'   unused columns using `unused_fn`.
#' @param ... Additional arguments passed on to methods.
#' @export
#' @examples
#' # See vignette("pivot") for examples and explanation
#'
#' fish_encounters
#' fish_encounters %>%
#'   pivot_wider(names_from = station, values_from = seen)
#' # Fill in missing values
#' fish_encounters %>%
#'   pivot_wider(names_from = station, values_from = seen, values_fill = 0)
#'
#' # Generate column names from multiple variables
#' us_rent_income
#' us_rent_income %>%
#'   pivot_wider(
#'     names_from = variable,
#'     values_from = c(estimate, moe)
#'   )
#'
#' # You can control whether `names_from` values vary fastest or slowest
#' # relative to the `values_from` column names using `names_vary`.
#' us_rent_income %>%
#'   pivot_wider(
#'     names_from = variable,
#'     values_from = c(estimate, moe),
#'     names_vary = "slowest"
#'   )
#'
#' # When there are multiple `names_from` or `values_from`, you can use
#' # use `names_sep` or `names_glue` to control the output variable names
#' us_rent_income %>%
#'   pivot_wider(
#'     names_from = variable,
#'     names_sep = ".",
#'     values_from = c(estimate, moe)
#'   )
#' us_rent_income %>%
#'   pivot_wider(
#'     names_from = variable,
#'     names_glue = "{variable}_{.value}",
#'     values_from = c(estimate, moe)
#'   )
#'
#' # Can perform aggregation with `values_fn`
#' warpbreaks <- as_tibble(warpbreaks[c("wool", "tension", "breaks")])
#' warpbreaks
#' warpbreaks %>%
#'   pivot_wider(
#'     names_from = wool,
#'     values_from = breaks,
#'     values_fn = mean
#'   )
#'
#' # Can pass an anonymous function to `values_fn` when you
#' # need to supply additional arguments
#' warpbreaks$breaks[1] <- NA
#' warpbreaks %>%
#'   pivot_wider(
#'     names_from = wool,
#'     values_from = breaks,
#'     values_fn = ~ mean(.x, na.rm = TRUE)
#'   )
pivot_wider <- function(data,
                        ...,
                        id_cols = NULL,
                        id_expand = FALSE,
                        names_from = name,
                        names_prefix = "",
                        names_sep = "_",
                        names_glue = NULL,
                        names_sort = FALSE,
                        names_vary = "fastest",
                        names_expand = FALSE,
                        names_repair = "check_unique",
                        values_from = value,
                        values_fill = NULL,
                        values_fn = NULL,
                        unused_fn = NULL) {
print_debug(data)
print_debug(...)
print_debug(id_cols)
print_debug(id_expand)
print_debug(names_from)
print_debug(names_prefix)
print_debug(names_sep)
print_debug(names_glue)
print_debug(names_sort)
print_debug(names_vary)
print_debug(names_expand)
print_debug(names_repair)
print_debug(values_from)
print_debug(values_fill)
print_debug(values_fn)
print_debug(unused_fn)
  # TODO: Use `check_dots_used()` after removing the `id_cols` compat behavior
  UseMethod("pivot_wider")
}

#' @export
pivot_wider.data.frame <- function(data,
                                   ...,
                                   id_cols = NULL,
                                   id_expand = FALSE,
                                   names_from = name,
                                   names_prefix = "",
                                   names_sep = "_",
                                   names_glue = NULL,
                                   names_sort = FALSE,
                                   names_vary = "fastest",
                                   names_expand = FALSE,
                                   names_repair = "check_unique",
                                   values_from = value,
                                   values_fill = NULL,
                                   values_fn = NULL,
                                   unused_fn = NULL) {
print_debug(data)
print_debug(...)
print_debug(id_cols)
print_debug(id_expand)
print_debug(names_from)
print_debug(names_prefix)
print_debug(names_sep)
print_debug(names_glue)
print_debug(names_sort)
print_debug(names_vary)
print_debug(names_expand)
print_debug(names_repair)
print_debug(values_from)
print_debug(values_fill)
print_debug(values_fn)
print_debug(unused_fn)
print_debug(names_from)
  names_from <- enquo(names_from)
  values_from <- enquo(values_from)
print_debug(values_from)
print_debug(names_from)

print_debug(values_from)
print_debug(data)
print_debug(names_from)
print_debug(values_from)
print_debug(names_prefix)
print_debug(names_sep)
print_debug(names_glue)
print_debug(names_sort)
print_debug(names_vary)
print_debug(names_expand)
  spec <- build_wider_spec(
    data = data,
    names_from = !!names_from,
    values_from = !!values_from,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_glue = names_glue,
    names_sort = names_sort,
    names_vary = names_vary,
    names_expand = names_expand,
    error_call = current_env()
  )

print_debug(spec)
print_debug(id_cols)
print_debug(...)
  id_cols <- compat_id_cols(
    id_cols = {{ id_cols }},
    ...,
    fn_call = match.call(expand.dots = FALSE)
  )

print_debug(id_cols)
print_debug(data)
print_debug(id_cols)
print_debug(names_from)
print_debug(values_from)
  id_cols <- build_wider_id_cols_expr(
    data = data,
    id_cols = !!id_cols,
    names_from = !!names_from,
    values_from = !!values_from
  )

print_debug(id_cols)
print_debug(data)
print_debug(spec)
print_debug(id_cols)
print_debug(id_expand)
print_debug(names_repair)
print_debug(values_fill)
print_debug(values_fn)
print_debug(unused_fn)
  pivot_wider_spec(
    data = data,
    spec = spec,
    id_cols = !!id_cols,
    id_expand = id_expand,
    names_repair = names_repair,
    values_fill = values_fill,
    values_fn = values_fn,
    unused_fn = unused_fn,
    error_call = current_env()
  )
}

#' Pivot data from long to wide using a spec
#'
#' This is a low level interface to pivoting, inspired by the cdata package,
#' that allows you to describe pivoting with a data frame.
#'
#' @keywords internal
#' @export
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#' @inheritParams pivot_wider
#' @param spec A specification data frame. This is useful for more complex
#'  pivots because it gives you greater control on how metadata stored in the
#'  columns become column names in the result.
#'
#'   Must be a data frame containing character `.name` and `.value` columns.
#'   Additional columns in `spec` should be named to match columns in the
#'   long format of the dataset and contain values corresponding to columns
#'   pivoted from the wide format.
#'   The special `.seq` variable is used to disambiguate rows internally;
#'   it is automatically removed after pivoting.
#' @param id_cols <[`tidy-select`][tidyr_tidy_select]> A set of columns that
#'   uniquely identifies each observation. Defaults to all columns in `data`
#'   except for the columns specified in `spec$.value` and the columns of the
#'   `spec` that aren't named `.name` or `.value`. Typically used when you have
#'   redundant variables, i.e. variables whose values are perfectly correlated
#'   with existing variables.
#'
#' @examples
#' # See vignette("pivot") for examples and explanation
#'
#' us_rent_income
#' spec1 <- us_rent_income %>%
#'   build_wider_spec(names_from = variable, values_from = c(estimate, moe))
#' spec1
#'
#' us_rent_income %>%
#'   pivot_wider_spec(spec1)
#'
#' # Is equivalent to
#' us_rent_income %>%
#'   pivot_wider(names_from = variable, values_from = c(estimate, moe))
#'
#' # `pivot_wider_spec()` provides more control over column names and output format
#' # instead of creating columns with estimate_ and moe_ prefixes,
#' # keep original variable name for estimates and attach _moe as suffix
#' spec2 <- tibble(
#'   .name = c("income", "rent", "income_moe", "rent_moe"),
#'   .value = c("estimate", "estimate", "moe", "moe"),
#'   variable = c("income", "rent", "income", "rent")
#' )
#'
#' us_rent_income %>%
#'   pivot_wider_spec(spec2)
pivot_wider_spec <- function(data,
                             spec,
                             ...,
                             names_repair = "check_unique",
                             id_cols = NULL,
                             id_expand = FALSE,
                             values_fill = NULL,
                             values_fn = NULL,
                             unused_fn = NULL,
                             error_call = current_env()) {
print_debug(data)
print_debug(spec)
print_debug(...)
print_debug(names_repair)
print_debug(id_cols)
print_debug(id_expand)
print_debug(values_fill)
print_debug(values_fn)
print_debug(unused_fn)
print_debug(error_call)
print_debug(...)
  check_dots_empty0(...)
print_debug(data)
print_debug(error_call)
  check_data_frame(data, call = error_call)

print_debug(spec)
print_debug(error_call)
  spec <- check_pivot_spec(spec, call = error_call)
  check_bool(id_expand, call = error_call)
print_debug(id_expand)
print_debug(error_call)
print_debug(spec)

  names_from_cols <- names(spec)[-(1:2)]
print_debug(spec)
print_debug(.value)
  values_from_cols <- vec_unique(spec$.value)

print_debug(values_from_cols)
print_debug(data)
print_debug(id_cols)
print_debug(names_from_cols)
print_debug(values_from_cols)
print_debug(error_call)
  id_cols <- select_wider_id_cols(
    data = data,
    id_cols = {{ id_cols }},
    names_from_cols = names_from_cols,
    values_from_cols = values_from_cols,
    error_call = error_call
  )

print_debug(id_cols)
print_debug(values_fn)
print_debug(values_from_cols)
print_debug(error_call)
  values_fn <- check_list_of_functions(values_fn, values_from_cols, call = error_call)

print_debug(values_fn)
  unused_cols <- setdiff(names(data), c(id_cols, names_from_cols, values_from_cols))
print_debug(unused_fn)
print_debug(unused_cols)
print_debug(error_call)
  unused_fn <- check_list_of_functions(unused_fn, unused_cols, call = error_call)
  unused_cols <- names(unused_fn)
print_debug(unused_fn)

  if (is.null(values_fill)) {
    values_fill <- list()
print_debug(values_fill)
  } else if (is_scalar(values_fill)) {
print_debug(values_from_cols)
print_debug(values_fill)
    values_fill <- rep_named(values_from_cols, list(values_fill))
  } else if (!vec_is_list(values_fill)) {
print_debug(values_fill)
print_debug(values_fill)
print_debug(error_call)
    cli::cli_abort(
      "{.arg values_fill} must be {.code NULL}, a scalar, or a named list, not {.obj_type_friendly {values_fill}}.",
      call = error_call
    )
  }
  values_fill <- values_fill[intersect(names(values_fill), values_from_cols)]

  input <- data
  # Early conversion to tibble because data.table returns zero rows if
  # zero cols are selected. Also want to avoid the grouped-df behavior
  # of `complete()`.
print_debug(data)
  data <- as_tibble(data)
  data <- data[vec_unique(c(id_cols, names_from_cols, values_from_cols, unused_cols))]
print_debug(id_cols)
print_debug(names_from_cols)
print_debug(values_from_cols)
print_debug(unused_cols)
print_debug(data)

print_debug(data)
  if (id_expand) {
print_debug(data)
print_debug(id_cols)
print_debug(values_fill)
    data <- complete(data, !!!syms(id_cols), fill = values_fill, explicit = FALSE)
print_debug(id_cols)
print_debug(values_fill)
  }
print_debug(data)
print_debug(data)

  # Figure out rows in output
  rows <- data[id_cols]
print_debug(rows)
  row_id <- vec_group_id(rows)
  nrow <- attr(row_id, "n")
print_debug(row_id)
print_debug(rows)
print_debug(row_id)
  rows <- vec_slice(rows, vec_unique_loc(row_id))
print_debug(row_id)

print_debug(rows)
print_debug(rows)
  n_unused_fn <- length(unused_fn)

  unused <- vector("list", length = n_unused_fn)
  names(unused) <- unused_cols

  if (n_unused_fn > 0L) {
    # This can be expensive, only compute if we are using `unused_fn`
print_debug(row_id)
print_debug(loc)
    unused_locs <- vec_group_loc(row_id)$loc
  }
print_debug(unused_locs)

  for (i in seq_len(n_unused_fn)) {
    unused_col <- unused_cols[[i]]
    unused_fn_i <- unused_fn[[i]]

    unused_value <- data[[unused_col]]

print_debug(unused_value)
print_debug(unused_locs)
print_debug(unused_col)
print_debug(unused_fn_i)
print_debug(error_call)
    unused[[i]] <- value_summarize(
      value = unused_value,
      value_locs = unused_locs,
      value_name = unused_col,
      fn = unused_fn_i,
      fn_name = "unused_fn",
      error_call = error_call
    )
  }
print_debug(unused)

print_debug(unused)
print_debug(nrow)
  unused <- tibble::new_tibble(unused, nrow = nrow)

print_debug(unused)
  duplicate_names <- character(0L)

  value_specs <- unname(split(spec, spec$.value))
print_debug(value_specs)
  value_out <- vec_init(list(), length(value_specs))

print_debug(value_out)
  for (i in seq_along(value_out)) {
    value_spec <- value_specs[[i]]
    value_name <- value_spec$.value[[1]]
    value <- data[[value_name]]

    cols <- data[names(value_spec)[-(1:2)]]
print_debug(cols)
print_debug(value_spec)
    col_id <- vec_match(as_tibble(cols), value_spec[-(1:2)])
print_debug(cols)
print_debug(value_spec)
    value_id <- data.frame(row = row_id, col = col_id)
print_debug(col_id)
print_debug(col_id)

    value_fn <- values_fn[[value_name]]

print_debug(value_id)
    if (is.null(value_fn) && vec_duplicate_any(value_id)) {
      # There are unhandled duplicates. Handle them with `list()` and warn.
      value_fn <- list
      duplicate_names <- c(duplicate_names, value_name)
    }

    if (!is.null(value_fn)) {
print_debug(value_id)
      result <- vec_group_loc(value_id)
      value_id <- result$key
print_debug(result)
      value_locs <- result$loc

print_debug(value)
print_debug(value_locs)
print_debug(value_name)
print_debug(value_fn)
print_debug(error_call)
      value <- value_summarize(
        value = value,
        value_locs = value_locs,
        value_name = value_name,
        fn = value_fn,
        fn_name = "values_fn",
        error_call = error_call
      )
    }
print_debug(value)

    ncol <- nrow(value_spec)

    fill <- values_fill[[value_name]]
    if (is.null(fill)) {
print_debug(value)
print_debug(nrow)
print_debug(ncol)
      out <- vec_init(value, nrow * ncol)
    } else {
print_debug(out)
print_debug(fill)
      stopifnot(vec_size(fill) == 1)
print_debug(fill)
print_debug(value)
print_debug(error_call)
      fill <- vec_cast(fill, value, call = error_call)
      out <- vec_rep_each(fill, nrow * ncol)
print_debug(fill)
print_debug(nrow)
print_debug(ncol)
print_debug(fill)
    }
print_debug(out)
print_debug(out)
print_debug(value_id)
print_debug(row)
print_debug(nrow)
print_debug(value_id)
print_debug(col)
print_debug(value)
    out <- vec_assign(out, value_id$row + nrow * (value_id$col - 1L), value)

print_debug(out)
print_debug(out)
print_debug(value_spec)
print_debug(.name)
    value_out[[i]] <- chop_rectangular_df(out, value_spec$.name)
  }
print_debug(value_out)

  if (length(duplicate_names) > 0L) {
print_debug(duplicate_names)
    duplicate_names <- glue::backtick(duplicate_names)
    duplicate_names <- glue::glue_collapse(duplicate_names, sep = ", ", last = " and ")
print_debug(duplicate_names)
print_debug(duplicate_names)

print_debug(duplicate_names)
    group_cols <- c(id_cols, names_from_cols)
print_debug(group_cols)
    group_cols <- backtick_if_not_syntactic(group_cols)
    group_cols <- glue::glue_collapse(group_cols, sep = ", ")
print_debug(group_cols)
print_debug(group_cols)

print_debug(group_cols)
    cli::cli_warn(c(
      "Values from {duplicate_names} are not uniquely identified; output will contain list-cols.",
      "*" = "Use `values_fn = list` to suppress this warning.",
      "*" = "Use `values_fn = {{summary_fun}}` to summarise duplicates.",
      "*" = "Use the following dplyr code to identify duplicates.",
      " " = "  {{data}} |>",
      " " = "    dplyr::summarise(n = dplyr::n(), .by = c({group_cols})) |>",
      " " = "    dplyr::filter(n > 1L)"
    ))
  }

  # `check_pivot_spec()` ensures `.name` is unique. Name repair shouldn't be needed.
print_debug(value_out)
  values <- vec_cbind(!!!value_out, .name_repair = "minimal")

print_debug(values)
  # Recreate desired column order of the new spec columns (#569)
  values <- values[spec$.name]

print_debug(rows)
print_debug(values)
print_debug(unused)
print_debug(names_repair)
print_debug(error_call)
  out <- wrap_error_names(vec_cbind(
print_debug(rows)
print_debug(values)
print_debug(unused)
print_debug(names_repair)
print_debug(error_call)
    rows,
    values,
    unused,
    .name_repair = names_repair,
    .error_call = error_call
  ))

print_debug(out)
print_debug(out)
print_debug(input)
print_debug(out)
  reconstruct_tibble(input, out)
}

#' @export
#' @rdname pivot_wider_spec
#' @inheritParams pivot_wider
build_wider_spec <- function(data,
                             ...,
                             names_from = name,
                             values_from = value,
                             names_prefix = "",
                             names_sep = "_",
                             names_glue = NULL,
                             names_sort = FALSE,
                             names_vary = "fastest",
                             names_expand = FALSE,
                             error_call = current_env()) {
print_debug(data)
print_debug(...)
print_debug(names_from)
print_debug(values_from)
print_debug(names_prefix)
print_debug(names_sep)
print_debug(names_glue)
print_debug(names_sort)
print_debug(names_vary)
print_debug(names_expand)
print_debug(error_call)
print_debug(...)
  check_dots_empty0(...)

print_debug(names_from)
print_debug(data)
print_debug(error_call)
  names_from <- tidyselect::eval_select(
    enquo(names_from),
print_debug(names_from)
    data,
    allow_rename = FALSE,
    allow_empty = FALSE,
    error_call = error_call
  )
  values_from <- tidyselect::eval_select(
print_debug(values_from)
print_debug(data)
print_debug(error_call)
print_debug(names_from)
    enquo(values_from),
print_debug(values_from)
    data,
    allow_rename = FALSE,
    allow_empty = FALSE,
    error_call = error_call
  )

print_debug(values_from)
print_debug(names_prefix)
print_debug(error_call)
  check_string(names_prefix, call = error_call)
print_debug(names_sep)
print_debug(error_call)
  check_string(names_sep, call = error_call)
print_debug(names_glue)
print_debug(error_call)
  check_string(names_glue, allow_null = TRUE, call = error_call)
print_debug(names_sort)
print_debug(error_call)
  check_bool(names_sort, call = error_call)
print_debug(names_expand)
print_debug(error_call)
  check_bool(names_expand, call = error_call)

print_debug(names_vary)
print_debug(error_call)
  names_vary <- arg_match0(
    arg = names_vary,
    values = c("fastest", "slowest"),
    arg_nm = "names_vary",
    error_call = error_call
  )

print_debug(names_vary)
print_debug(data)
  data <- as_tibble(data)
  data <- data[names_from]
print_debug(data)

  if (names_expand) {
    # `expand()` always does sort + unique
print_debug(data)
print_debug(data)
    row_ids <- expand(data, !!!syms(names(data)))
print_debug(data)
  } else {
print_debug(row_ids)
print_debug(row_ids)
print_debug(data)
    row_ids <- vec_unique(data)

print_debug(row_ids)
    if (names_sort) {
print_debug(row_ids)
      row_ids <- vec_sort(row_ids)
    }
print_debug(row_ids)
  }

print_debug(paste)
print_debug(row_ids)
print_debug(names_sep)
  row_names <- exec(paste, !!!row_ids, sep = names_sep)

print_debug(row_names)
print_debug(names_prefix)
print_debug(row_names)
  out <- tibble(
    .name = vec_paste0(names_prefix, row_names)
print_debug(names_prefix)
print_debug(row_names)
  )

print_debug(out)
  if (length(values_from) == 1) {
    out$.value <- names(values_from)
  } else {
    if (names_vary == "fastest") {
print_debug(out)
print_debug(values_from)
      out <- vec_rep(out, vec_size(values_from))
print_debug(values_from)
      out$.value <- vec_rep_each(names(values_from), vec_size(row_ids))
print_debug(out)
print_debug(values_from)
print_debug(row_ids)
print_debug(out)
print_debug(row_ids)
      row_ids <- vec_rep(row_ids, vec_size(values_from))
print_debug(out)
print_debug(row_ids)
print_debug(values_from)
print_debug(out)
print_debug(values_from)
    } else {
print_debug(row_ids)
print_debug(row_ids)
print_debug(out)
print_debug(values_from)
      out <- vec_rep_each(out, vec_size(values_from))
print_debug(values_from)
      out$.value <- vec_rep(names(values_from), vec_size(row_ids))
print_debug(out)
print_debug(values_from)
print_debug(row_ids)
print_debug(out)
print_debug(row_ids)
      row_ids <- vec_rep_each(row_ids, vec_size(values_from))
print_debug(out)
print_debug(row_ids)
print_debug(values_from)
print_debug(out)
print_debug(values_from)
    }
print_debug(row_ids)
print_debug(row_ids)

print_debug(out)
print_debug(.value)
print_debug(names_sep)
print_debug(out)
print_debug(.name)
    out$.name <- vec_paste0(out$.value, names_sep, out$.name)
  }
print_debug(out)

print_debug(out)
print_debug(row_ids)
  out <- vec_cbind(out, as_tibble(row_ids), .name_repair = "minimal")
print_debug(row_ids)
  if (!is.null(names_glue)) {
print_debug(out)
print_debug(out)
print_debug(out)
print_debug(names_glue)
    out$.name <- as.character(glue::glue_data(out, names_glue))
  }
print_debug(out)

  out
}

build_wider_id_cols_expr <- function(data,
                                     id_cols = NULL,
                                     names_from = name,
                                     values_from = value,
                                     error_call = caller_env()) {
print_debug(data)
print_debug(id_cols)
print_debug(names_from)
print_debug(values_from)
print_debug(error_call)
print_debug(names_from)
print_debug(data)
print_debug(error_call)
  names_from <- tidyselect::eval_select(
    enquo(names_from),
print_debug(names_from)
    data,
    allow_rename = FALSE,
    error_call = error_call
  )

print_debug(names_from)
print_debug(values_from)
print_debug(data)
print_debug(error_call)
  values_from <- tidyselect::eval_select(
    enquo(values_from),
print_debug(values_from)
    data,
    allow_rename = FALSE,
    error_call = error_call
  )

print_debug(values_from)
print_debug(data)
print_debug(id_cols)
print_debug(names_from)
print_debug(values_from)
print_debug(error_call)
  out <- select_wider_id_cols(
    data = data,
    id_cols = {{ id_cols }},
    names_from_cols = names(names_from),
    values_from_cols = names(values_from),
    error_call = error_call
  )

print_debug(out)
print_debug(out)
  expr(c(!!!out))
}

select_wider_id_cols <- function(data,
                                 id_cols = NULL,
                                 names_from_cols = character(),
                                 values_from_cols = character(),
                                 error_call = caller_env()) {
print_debug(data)
print_debug(id_cols)
print_debug(names_from_cols)
print_debug(values_from_cols)
print_debug(error_call)
print_debug(id_cols)
  id_cols <- enquo(id_cols)

print_debug(id_cols)
  # Remove known non-id-cols so they are never selected
  data <- data[setdiff(names(data), c(names_from_cols, values_from_cols))]

print_debug(id_cols)
  if (quo_is_null(id_cols)) {
    # Default selects everything in `data` after non-id-cols have been removed
    return(names(data))
  }

print_debug(id_cols)
print_debug(id_cols)
print_debug(data)
print_debug(error_call)
print_debug(cnd)
print_debug(cnd)
print_debug(names_from_cols)
print_debug(values_from_cols)
print_debug(error_call)
  try_fetch(
print_debug(id_cols)
print_debug(data)
print_debug(error_call)
    id_cols <- tidyselect::eval_select(
      enquo(id_cols),
print_debug(id_cols)
      data,
      allow_rename = FALSE,
      error_call = error_call
    ),
    vctrs_error_subscript_oob = function(cnd) {
print_debug(id_cols)
print_debug(cnd)
print_debug(cnd)
print_debug(names_from_cols)
print_debug(values_from_cols)
print_debug(error_call)
      rethrow_id_cols_oob(cnd, names_from_cols, values_from_cols, error_call)
    }
  )

  names(id_cols)
}

rethrow_id_cols_oob <- function(cnd, names_from_cols, values_from_cols, call) {
print_debug(cnd)
print_debug(names_from_cols)
print_debug(values_from_cols)
print_debug(call)
  i <- cnd[["i"]]

print_debug(i)
  check_string(i, .internal = TRUE)

  if (i %in% names_from_cols) {
print_debug(i)
print_debug(call)
    stop_id_cols_oob(i, "names_from", call = call)
  } else if (i %in% values_from_cols) {
print_debug(i)
print_debug(call)
    stop_id_cols_oob(i, "values_from", call = call)
  } else {
    # Zap this special handler, throw the normal condition
    zap()
  }
}

stop_id_cols_oob <- function(i, arg, call) {
print_debug(i)
print_debug(arg)
print_debug(call)
print_debug(call)
  cli::cli_abort(
    c(
      "`id_cols` can't select a column already selected by `{arg}`.",
      i = "Column `{i}` has already been selected."
    ),
    parent = NA,
    call = call
  )
}

compat_id_cols <- function(id_cols,
                           ...,
                           fn_call,
                           error_call = caller_env(),
                           user_env = caller_env(2)) {
print_debug(id_cols)
print_debug(...)
print_debug(fn_call)
print_debug(error_call)
print_debug(user_env)
print_debug(...)
  dots <- enquos(...)

print_debug(dots)
  # If `id_cols` is specified by name by the user, it will show up in the call.
  # Otherwise, default args don't show up in the call so it won't be there.
  user_specified_id_cols <- "id_cols" %in% names(fn_call)

  # For compatibility (#1353), assign the first value of `...` to `id_cols` if:
  # - The user didn't specify `id_cols`.
  # - There is exactly 1 unnamed element in `...`.
  use_compat_id_cols <-
    !user_specified_id_cols &&
    length(dots) == 1L &&
print_debug(dots)
    !is_named(dots)

  if (use_compat_id_cols) {
    id_cols <- dots[[1L]]
print_debug(id_cols)
print_debug(user_env)
    warn_deprecated_unnamed_id_cols(id_cols, user_env = user_env)
  } else {
print_debug(id_cols)
    id_cols <- enquo(id_cols)
    check_dots_empty0(..., call = error_call)
print_debug(...)
print_debug(error_call)
print_debug(id_cols)
  }

  id_cols
}

warn_deprecated_unnamed_id_cols <- function(id_cols, user_env = caller_env(2)) {
print_debug(id_cols)
print_debug(user_env)
print_debug(id_cols)
  id_cols <- as_label(id_cols)
print_debug(user_env)

print_debug(id_cols)
  lifecycle::deprecate_warn(
    when = "1.3.0",
    what = I(cli::format_inline(
      "Specifying the {.arg id_cols} argument by position"
    )),
    details = cli::format_inline(
      "Please explicitly name {.arg id_cols}, like {.code id_cols = {id_cols}}."
    ),
    always = TRUE,
    user_env = user_env
  )
}
print_debug(id_cols)

# Helpers -----------------------------------------------------------------

value_summarize <- function(value, value_locs, value_name, fn, fn_name, error_call = caller_env()) {
print_debug(value)
print_debug(value_locs)
print_debug(value_name)
print_debug(fn)
print_debug(fn_name)
print_debug(error_call)
print_debug(value)
print_debug(value_locs)
  value <- vec_chop(value, value_locs)

print_debug(value)
  if (identical(fn, list)) {
    # The no-op case, for performance
    return(value)
  }

print_debug(value)
print_debug(fn)
  value <- map(value, fn)

print_debug(value)
print_debug(value)
  sizes <- list_sizes(value)
  invalid_sizes <- sizes != 1L
print_debug(sizes)

  if (any(invalid_sizes)) {
print_debug(error_call)
    size <- sizes[invalid_sizes][[1]]

    cli::cli_abort(
      c(
        "Applying {.arg {fn_name}} to {.var {value_name}} must result in a single summary value per key.",
        i = "Applying {.arg {fn_name}} resulted in a vector of length {size}."
      ),
      call = error_call
    )
  }
print_debug(size)

print_debug(value)
  value <- list_unchop(value)

print_debug(value)
  value
}

# Wrap a "rectangular" vector into a data frame
chop_rectangular_df <- function(x, names) {
print_debug(x)
print_debug(names)
print_debug(names)
  n_col <- vec_size(names)
  n_row <- vec_size(x) / n_col
print_debug(x)
print_debug(n_col)
print_debug(n_col)

print_debug(n_row)
  indices <- vector("list", n_col)

  start <- 1L
  stop <- n_row

  for (i in seq_len(n_col)) {
print_debug(start)
print_debug(stop)
    indices[[i]] <- seq2(start, stop)
    start <- start + n_row
print_debug(indices)
    stop <- stop + n_row
  }

print_debug(x)
print_debug(indices)
  out <- vec_chop(x, indices)
  names(out) <- names
print_debug(out)
print_debug(out)
print_debug(n_row)
  tibble::new_tibble(out, nrow = n_row)
}

is_scalar <- function(x) {
print_debug(x)
  if (is.null(x)) {
    return(FALSE)
  }

print_debug(x)
  if (vec_is_list(x)) {
print_debug(x)
print_debug(x)
print_debug(x)
    (vec_size(x) == 1) && !have_name(x)
  } else {
print_debug(x)
    vec_size(x) == 1
  }
}

backtick_if_not_syntactic <- function(x) {
print_debug(x)
  ok <- make.names(x) == x
  ok[is.na(x)] <- FALSE

print_debug(x)
print_debug(ok)
  x[!ok] <- glue::backtick(x[!ok])

print_debug(x)
  x
}

## ----message = FALSE----------------------------------------------------------
knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
knitr::opts_chunk$set(warning = FALSE, message = FALSE, error = FALSE, echo = TRUE)
ggplot2::theme_set(ggplot2::theme_bw())
library(codebook)
library(dplyr)

## -----------------------------------------------------------------------------
# library(qualtRics) # currently not on CRAN, so commented out
results <- readRDS(system.file("extdata", "ryan.rds", package = "codebook"))
metadata_ex <- readRDS(system.file("extdata", "metadata.rds", package = "codebook"))

## -----------------------------------------------------------------------------
results <- results %>% rio::gather_attrs()
attributes(results)$label$Q7

## -----------------------------------------------------------------------------
names(metadata_ex$questions) <- lapply(metadata_ex$questions, function(x) {
  x$questionName
})

## -----------------------------------------------------------------------------
qs <- names(metadata_ex$questions)
qs <- qs[qs %in% names(attributes(results)$label)]

## -----------------------------------------------------------------------------
init <- vector("list", ncol(results)) 
names(init) <- names(results)
attributes(results)$item <- init
attributes(results)$item[qs] <- metadata_ex$questions[qs]

## -----------------------------------------------------------------------------
results <- results %>% rio::spread_attrs()

## -----------------------------------------------------------------------------
results <- results %>% select(ResponseSet, Q7, Q10)
if (!knit_by_pkgdown) knitr::opts_chunk$set(echo = FALSE)

## -----------------------------------------------------------------------------
metadata(results)$name <- "MOCK Qualtrics dataset"
metadata(results)$description <- "a MOCK dataset used to show how to import Qualtrics metadata into the codebook R package"
metadata(results)$identifier <- "doi:10.5281/zenodo.1326520"
metadata(results)$datePublished <- "2018-08-01"
metadata(results)$creator <- list(
      "@type" = "Person",
      givenName = "Ruben", familyName = "Arslan",
      email = "ruben.arslan@gmail.com", 
      affiliation = list("@type" = "Organization",
        name = "MPI Human Development, Berlin"))
metadata(results)$url <- "https://rubenarslan.github.io/codebook/articles/codebook_qualtrics.html"
metadata(results)$temporalCoverage <- "2018" 
metadata(results)$spatialCoverage <- "Nowhere" 

## -----------------------------------------------------------------------------
# We don't want to look at the code in the codebook.
knitr::opts_chunk$set(warning = TRUE, message = TRUE, echo = FALSE)

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (exists("testing")) {
	indent = '#' # ugly hack so _regression_summary can be "spun" (variables included via `r ` have to be available)
	results = data("bfi")
	metadata(results)$description <- data_description_default(bfi)
}

meta <- metadata(results)
description <- meta$description
meta <- recursive_escape(meta)

## ----results='asis'-----------------------------------------------------------
if (exists("name", meta)) {
  glue::glue(
    "__Dataset name__: {name}",
    .envir = meta)
}

## ----results='asis'-----------------------------------------------------------
cat(description)

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("temporalCoverage", meta)) {
  glue::glue(
    "- __Temporal Coverage__: {temporalCoverage}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("spatialCoverage", meta)) {
  glue::glue(
    "- __Spatial Coverage__: {spatialCoverage}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("citation", meta)) {
  glue::glue(
    "- __Citation__: {citation}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("url", meta)) {
  glue::glue(
    "- __URL__: [{url}]({url})",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("identifier", meta)) {
  if (stringr::str_detect(meta$identifier, "^doi:")) {
    meta$identifier <- paste0('<a href="https://dx.doi.org/', 
      stringr::str_match(meta$identifier, "^doi:(.+)")[,2], '">', 
      meta$identifier, '</a>')
  }
  glue::glue(
    "- __Identifier__: {identifier}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("datePublished", meta)) {
  glue::glue(
    "- __Date published__: {datePublished}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("creator", meta)) {
  cat("- __Creator__:")
  knitr::kable(tibble::enframe(meta$creator))
}

## -----------------------------------------------------------------------------
meta <- meta[setdiff(names(meta),
                     c("creator", "datePublished", "identifier",
                       "url", "citation", "spatialCoverage", 
                       "temporalCoverage", "description", "name"))]
if(length(meta)) {
  knitr::kable(meta)
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack
}
if (exists("testing")) {
	results <- data.frame()
	survey_repetition <- 'single'
	reliabilities <- list()
	md_pattern <- data.frame()
}

## ----missingness_all_setup----------------------------------------------------
if (length(md_pattern)) {
  if (knitr::is_html_output() && requireNamespace("rmarkdown", quietly = TRUE)) {
    rmarkdown::paged_table(md_pattern, options = list(rows.print = 10))
  } else {
    knitr::kable(md_pattern)
  }
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack
}
if (exists("testing")) {
	results <- data.frame()
	survey_repetition <- 'single'
	reliabilities <- list()
	missingness_report <- ''
	data_info <- '' 
	survey_overview <- '' 
	scales_items <- c()
	detailed_items <- TRUE
	detailed_scales <- TRUE
}

## -----------------------------------------------------------------------------
knitr::asis_output(data_info)

## -----------------------------------------------------------------------------
knitr::asis_output(survey_overview)

## ----scales-------------------------------------------------------------------
if (detailed_variables || detailed_scales) {
  knitr::asis_output(paste0(scales_items, sep = "\n\n\n", collapse = "\n\n\n"))
}

## -----------------------------------------------------------------------------
missingness_report

## -----------------------------------------------------------------------------
items

## -----------------------------------------------------------------------------
jsonld

## ----cb-----------------------------------------------------------------------
codebook(results, survey_repetition = "single",
         metadata_table = knit_by_pkgdown, metadata_json = knit_by_pkgdown)

## -----------------------------------------------------------------------------
if (!knit_by_pkgdown) {
  codebook:::escaped_table(codebook_table(results))
}


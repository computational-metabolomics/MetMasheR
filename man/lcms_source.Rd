% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_annotation_class.R
\name{lcms_source}
\alias{lcms_source}
\title{Import annotation source}
\usage{
lcms_source(input_file, add_cols = list(), tag = "", ...)
}
\arguments{
\item{input_file}{(character) The file annotations are imported from.}

\item{add_cols}{(list) A (named) list of additional columns to be added to the table and populated with the provided value. The default is \code{list()}.}

\item{tag}{(character) An abbreviation used to identify this source in annotation tables. The default is \code{""}.}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{lcms_source} object with the following \code{output} slots:
\tabular{ll}{
\code{imported} \tab          (lcms_table) The imported annotations as an lcms_table object. \cr
}
}
\description{
Imports an annotation source.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotation_class.R
\name{annotation_table}
\alias{annotation_table}
\title{An annotation source}
\usage{
annotation_table(
  annotations = data.frame(id = numeric(0)),
  tag = "",
  id_column = "id",
  ...
)
}
\arguments{
\item{annotations}{(data.frame) Data frame of imported annotations. The default is \code{data.frame(id = numeric(0))}.}

\item{tag}{(character) A (short) character string that is used to represent this table e.g. in column names, or source columns. The default is \code{""}.}

\item{id_column}{(character)  The default is \code{"id"}.}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{annotation_table} object with the following \code{output} slots:
\tabular{ll}{
\code{annotations} \tab          (data.frame) Data frame of imported annotations. \cr
}
}
\description{
Base class for annotation sources.
}

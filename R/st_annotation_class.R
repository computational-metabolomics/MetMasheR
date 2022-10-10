#' @include mspurity_annotation_class.R
#' @export st_annotation
st_annotation = function(input_file,tag='ST',...) {
    # new object
    out = new_struct('st_annotation',
                     input_file = input_file,
                     tag = tag,
                     ...
    )
    return(out)
}


.st_annotation<-setClass(
    "st_annotation",
    contains = c('mspurity_annotation')
)


#' @export
setMethod(f = "import_annotations",
          signature = c("st_annotation"),
          definition = function(obj) {
              semi_targeted_data <- read.table(obj$input_file, sep="\t", 
                                               stringsAsFactors = F, header=TRUE)
              
              semi_targeted_data <- semi_targeted_data[, c("library_compound_name",
                                                           "library_accession", "library_precursor_type", "library_source_name",
                                                           "library_rt", "rtdiff","library_precursor_mz",
                                                           "grp_name","grpid", "dpc", "rdpc", "cdpc","mcount", "allcount", "mpercent")]
              obj$annotations=semi_targeted_data
              return(obj)
          }
)



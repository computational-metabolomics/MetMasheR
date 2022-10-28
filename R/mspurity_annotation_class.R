#' @include annotation_class.R
#' @export mspurity_annotation
mspurity_annotation = function(input_file,tag = 'msPurity',...) {
    # new object
    out = new_struct('mspurity_annotation',
                     input_file = input_file,
                     tag = tag,
                     ...
    )
    return(out)
}

.mspurity_annotation<-setClass(
    "mspurity_annotation",
    contains = c('lcms_table')
)

# autocompletion, return sample_meta column names
#' @export
#' @method .DollarNames mspurity_annotation
.DollarNames.mspurity_annotation <- function(x, pattern = "") {
    struct:::.DollarNames.struct_class(x,pattern)
}

#' @export 
setMethod('.DollarNames','mspurity_annotation',.DollarNames.mspurity_annotation)

#' @export
setMethod(f = "import_source",
          signature = c("mspurity_annotation"),
          definition = function(M,...) {
              in_data <- read.table(obj$input_file, sep="\t", 
                                    stringsAsFactors = F, header=TRUE)
              obj$annotations=in_data
              return(obj)
          }
)
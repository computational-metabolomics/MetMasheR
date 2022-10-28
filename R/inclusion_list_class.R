#' @include annotation_class.R
#' @export inclusion_list
inclusion_list = function(input_file, tag = 'IL',...) {
    # new object
    out = new_struct('inclusion_list',
        input_file=input_file,
        tag=tag,
        ...
    )
    return(out)
}


.inclusion_list<-setClass(
    "inclusion_list",
    contains = c('lcms_table')
)


#' @export
setMethod(f = "import_source",
    signature = c("inclusion_list"),
    definition = function(M,...) {
        file_in <- read.csv(file=obj$input_file, sep=",",check.names = FALSE)
        obj$annotations=file_in
        return(obj)
    }
)



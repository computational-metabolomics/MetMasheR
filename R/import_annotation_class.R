#' @eval get_description('import_annotation')
#' @export
#' @include annotation_class.R
import_annotation = function(...) {
    out=struct::new_struct('import_annotation',
        ...)
    return(out)
}


.import_annotation<-setClass(
    "import_annotation",
    contains = c('model'),
    slots = c(imported='entity'),
    prototype=list(
        name = 'Import annotation',
        description = paste0('Imports an annotation source as part of a model.sequence.'),
        type = 'univariate',
        predicted = 'imported',
        .outputs=c('imported'),
        imported = entity(
            name = 'Imported annotations',
            description=paste0('The imported annotations as an annotation_source object'),
            type='annotation_source'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("import_annotation","annotation_source"),
    definition=function(M,D) {
        
        M$imported = import_annotations(D)
        
        return(M)
    }
)





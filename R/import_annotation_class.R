#' @eval get_description('import_annotation')
#' @export
#' @include annotation_class.R
import_annotation = function(input_file,add_cols,...) {
    out=struct::new_struct('import_annotation',
        input_file=input_file,
        add_cols=add_cols,
        ...)
    return(out)
}


.import_annotation<-setClass(
    "import_annotation",
    contains = c('model'),
    slots = c(
        imported='entity',
        input_file='entity',
        add_cols='entity'),
    prototype=list(
        name = 'Import annotation source',
        description = paste0('Imports an annotation source.'),
        type = 'import',
        predicted = 'imported',
        .params=c('input_file','add_cols','tag'),
        .outputs=c('imported'),
        imported = entity(
            name = 'Imported annotations',
            description=paste0('The imported annotations as an annotation_table object'),
            type='annotation_table'
        ),
        input_file = entity(
            name = 'Input file',
            description = "The file annotations are imported from.",
            value=character(0),
            type='character',
            max_length = 1
        ),
        add_cols = entity(
            name='additonal columns',
            description=paste0('A (named) list of additional columns to be ',
            'added to the table and populated with the provided value.'),
            type='list',
            max_length=Inf
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("import_annotation","annotation_table"),
    definition=function(M,D) {
        
        stop('No import method defined for "',class(M)[1],'" objects.')
        
    }
)





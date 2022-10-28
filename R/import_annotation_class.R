#' @eval get_description('annotation_source')
#' @export
#' @include annotation_class.R
annotation_source = function(input_file,add_cols=list(),tag='',...) {
    out=struct::new_struct('annotation_source',
        input_file=input_file,
        add_cols=add_cols,
        tag=tag,
        ...)
    return(out)
}


.annotation_source<-setClass(
    "annotation_source",
    contains = c('model'),
    slots = c(
        imported='entity',
        input_file='entity',
        add_cols='entity',
        tag='entity'),
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
        ),
        tag = entity(
            name='Source tag',
            description = paste0('An abbreviation used to identify this source ',
                'in annotation tables'),
            type='character',
            max_length=1
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("annotation_source","annotation_table"),
    definition=function(M,D) {
        
        stop('No import method defined for "',class(M)[1],'" objects.')
        
    }
)

#' @export
setMethod(f="import_source",
    signature=c("annotation_source"),
    definition=function(M) {
        O = output_obj(M,'imported')
        A = new_struct(O$type[1])
        M = model_apply(M,A)
        return(predicted(M))
        
    }
)


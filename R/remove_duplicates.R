#' @eval get_description('remove_duplicates')
#' @export
#' @include annotation_class.R
remove_duplicates = function(mode,columns,...) {
    out=struct::new_struct('remove_duplicates',
        mode = mode,
        columns = columns,
        ...)
    return(out)
}


.remove_duplicates<-setClass(
    "remove_duplicates",
    contains = c('model'),
    slots = c(
        mode ='enum',
        columns = 'entity',
        updated = 'entity'),
    prototype=list(
        name = 'Remove duplicates',
        description = paste0('A wrapper around dplyr::distinct to remove duplicate rows from the annotation table.'),
        type = 'univariate',
        predicted = 'updated',
        .params = c('columns','mode'),
        .outputs=c('updated'),
        updated = entity(
            name = 'Updated annotations',
            description=paste0('The updated annotations as an annotation_table object'),
            type='annotation_table'
        ),
        columns = entity(
            name = 'Columns',
            description = 'The column names considered (or not) when searching for duplicates.',
            type='character'
        ),
        mode = enum(
            name = 'Mode',
            description = c('include'='The named columns are considered when searching for duplicates',
                            'exclude'='The named columns are not considered when searching for duplicates'),
            type = 'character',
            allowed = c('include','exclude'),
            value = 'exclude'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("remove_duplicates","annotation_table"),
    definition=function(M,D) {
        
        cols=M$columns
        if (M$mode=='exclude') {
            cols=colnames(D$annotations)
            w=which(cols %in% M$columns)
            cols=cols[-w]
        }
        A = dplyr::distinct(D$annotations,across(all_of(cols)),.keep_all = TRUE)
        D$annotations=A
        M$updated=D
        
        return(M)
    }
)

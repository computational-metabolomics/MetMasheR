#' @eval get_description('add_columns')
#' @export
#' @include annotation_class.R
add_columns = function(new_columns,by,...) {
    
    check=by %in% colnames(new_columns)
    if (!check) {
        stop('parameter "by" must refer to a column name of the "new_columns",
            " data.frame')
    }
    
    out=struct::new_struct('add_columns',
        new_columns = new_columns,
        by = by,
        ...)
    return(out)
}


.add_columns<-setClass(
    "add_columns",
    contains = c('model'),
    slots = c(
        updated='entity',
        new_columns = 'entity',
        by = 'entity'),
    prototype=list(
        name = 'Add columns',
        description = paste0('A wrapper around dplyr::left_join. Adds columns to an annotation table by performing ',
        'a left-join with an input data.frame. (annotations on the left).'),
        type = 'univariate',
        predicted = 'updated',
        .params = c('new_columns','by'),
        .outputs=c('updated'),
        updated = entity(
            name = 'Updated annotations',
            description=paste0('The updated annotations as an annotation_table object'),
            type='annotation_table'
        ),
        new_columns = entity(
            name = 'New columns',
            description = 'A data.frame (rhs) to be left-joined with the annotation table (lhs).',
            type='data.frame'
        ),
        by = entity(
            name = 'By',
            description = 'A character vector of variables to join by.',
            type='character'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("add_columns","annotation_table"),
    definition=function(M,D) {
        
        A = D$annotations
        B = M$new_columns
        
        C = dplyr::left_join(A,B,by=M$by)
        
        D$annotations = C
        M$updated = D
        
        return(M)
    }
)





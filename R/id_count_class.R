#' @eval get_description('id_counts')
#' @export
#' @include annotation_class.R
id_counts = function(
    id_column,
    count_column = 'id_counts',
    ...) {
    
   
    out=struct::new_struct('id_counts',
        id_column=id_column,
        count_column=count_column,
        ...)
    return(out)
}



.id_counts<-setClass(
    "id_counts",
    contains = c('model'),
    slots=c(
        id_column='entity',
        count_column = 'entity',
        updated = 'entity'
    ),
    
    prototype=list(
        name = 'id counts',
        description = paste0('Adds the number of times an identical identifier is present to each record.'),
        type = 'univariate',
        predicted = 'updated',
        .params=c('id_column','count_column'),
        .outputs=c('updated'),
        updated=entity(
            name='Updated annotations',
            description = 'The input annotation source with the newly generated column.',
            type='annotation_table'
        ),
        
        count_column=entity(
            name='count column name',
            description = 'The name of the new column to store the counts in.',
            type='character'
        ),
       id_column=entity(
            name='id column name',
            description = 'column name of the variable ids in variable_meta.',
            type='character'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("id_counts","annotation_table"),
    definition=function(M,D) {
        
        X = D$annotations
        
        # if no rows add count column then return
        if (nrow(X)==0){
            X[[M$count_column]]=numeric(0)
            D$annotations=X
            M$updated=D
            return(M)
        }
        
        # for each record
        X[[M$count_column]]=0 # prefill with zero
        for (k in seq_len(nrow(X))) {
            # get the identifier
            id = X[[M$id_column]][k]
            
            # skip if NA or no id
            if (length(id)==0) {
                next
            }
            if (is.na(id)) {
                next
            }
            
            # number of occurrences
            n = sum(X[[M$id_column]]==id,na.rm=TRUE)
            # update annotations
            X[[M$count_column]][k]=n
        }
        D$annotations=X
        M$updated=D
        
        return(M)
    }
)

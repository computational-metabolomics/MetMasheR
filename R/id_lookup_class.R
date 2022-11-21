#' @eval get_description('id_db_lookup')
#' @export
#' @include annotation_class.R
id_db_lookup = function(
    annotation_column,
    database_column,
    id_database,
    include = NULL,
    tag = NULL,
    not_found = NA,
    ...) {
    out=struct::new_struct('id_db_lookup',
        annotation_column = annotation_column,
        database_column = database_column,
        id_database = id_database,
        include = include,
        tag = tag,
        not_found = not_found,
        ...)
    return(out)
}



.id_db_lookup<-setClass(
    "id_db_lookup",
    contains = c('model'),
    slots=c(
        annotation_column = 'entity',
        database_column = 'entity',
        id_database = 'entity',
        include = 'entity',
        updated='entity',
        tag = 'entity',
        not_found = 'entity'
    ),
    
    prototype=list(
        name = 'ID lookup by database',
        description = paste0('Search a database (data.frame) for annotation matches based on values in a specified column.'),
        type = 'univariate',
        predicted = 'updated',
        libraries = 'dplyr',
        .params=c('annotation_column','database_column','id_database','include','tag','not_found'),
        .outputs=c('updated'),
        annotation_column = entity(
            name = 'Annotions column name',
            description ='The annotation table column name to use as the reference for searching the database e.g. "HMBD_ID"',
            type=c('character'),
            value='V1',
            max_length=1
        ),
        database_column = entity(
            name = 'Database column name',
            description ='The database column to search for matches to the values in annoation_column.',
            type=c('character'),
            value='',
            max_length=1
        ),
        id_database = entity(
            name = 'id database',
            description =c(
                'A database of annotations and identifiers to be searched.'
            ),
            type=c('data.frame')
        ),
        include=entity(
            name = 'Include columns',
            description = 'The name of the database columns to be added to the annotations. If NULL, all columns are retained.',
            type=c('character','NULL'),
            max_length = Inf
        ),
        updated=entity(
            name='Updated annotations',
            description = 'The input annotation sources is updated with matching columns from the is database.',
            type='annotation_table'
        ),
        tag=entity(
            name = 'Column name tag',
            description = paste0('A string appended to the column names from the database. ',
                'Used to distinguish columns from different databases with identical column names.',
                'If tag = NULL then the column names are not changed.'),
            type=c('character','NULL'),
            max_length = 1
        ),
        not_found = entity(
            name = 'Not found',
            description = 'The returned value when there are no matches.',
            type = c('character','numeric','logical','NULL'),
            max_length = 1
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("id_db_lookup","annotation_table"),
    definition=function(M,D) {
        
        X = D$annotations
        
        # match class to db
        X[[M$annotation_column]]=as(X[[M$annotation_column]],class(M$id_database[[M$database_column]]))
        
        # for each annotation
        OUT = apply(X,1,function(x){
            # search for database rows that match the annotation column
            w = which(M$id_database[[M$database_column]] == x[[M$annotation_column]])
            
            if (length(w)==0){
                # if no hits in db then return no_match
                found = M$id_database[1,,drop=FALSE]
                found[1,]=M$not_found
                found[[M$database_column]]=x[[M$annotation_column]]
            } else {
                found = M$id_database[w,,drop=FALSE]
            }
            return(found)
        })
        OUT = do.call(rbind,OUT)
        
        # remove duplicates
        OUT = OUT[!duplicated(OUT),]
        
        # include only requested
        OUT = OUT[,c(M$database_column,M$include)]
       
        # add tags
        if (!is.null(M$tag)) {
            colnames(OUT) = paste0(M$tag,'_',colnames(OUT))
        }
        
        # match by provided columns
        by  = M$database_column
        names(by) = M$annotation_column
        
        # merge with original table
        merged = dplyr::left_join(X,OUT,by=by)

        D$annotations=merged
        
        M$updated=D
        
        return(M)
    }
)





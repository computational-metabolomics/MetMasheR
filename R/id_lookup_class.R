#' @eval get_description('id_db_lookup')
#' @export
#' @include annotation_class.R
id_db_lookup = function(
    annotation_column,
    database_column,
    id_database,
    include = NULL,
    tag = NULL,
    ...) {
    out=struct::new_struct('id_db_lookup',
        annotation_column = annotation_column,
        database_column = database_column,
        id_database = id_database,
        include = include,
        tag = tag,
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
        tag = 'entity'
    ),
    
    prototype=list(
        name = 'ID lookup by database',
        description = paste0('Search a database (data.frame) for annotation matches based on values in a specified column.'),
        type = 'univariate',
        predicted = 'updated',
        libraries = 'dplyr',
        .params=c('annotation_column','database_column','id_database','include','tag'),
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
            type='annotation_source'
        ),
        tag=entity(
            name = 'Column name tag',
            description = paste0('A string appended to the column names from the database. ',
                'Used to distinguish columns from different databases with identical column names.',
                'If tag = NULL then the column names are not changed.'),
            type=c('character','NULL'),
            max_length = 1
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("id_db_lookup","annotation_source"),
    definition=function(M,D) {
        
        X = D$annotations
        
        # add order column
        X$.orig_order=1:nrow(X)
        
        # only include named columns
        if (is.null(M$include)) {
            include_cols = colnames(M$id_database)
        } else {
            include_cols = unique(c(M$database_column,M$include))
        }
        
        # match class to db
        X[[M$annotation_column]]=as(X[[M$annotation_column]],class(M$id_database[[M$database_column]]))
        
        # for each annotation
        OUT = apply(X,1,function(x){
            # search for database rows that match the annotation column
            w = which(M$id_database[[M$database_column]] == x[[M$annotation_column]])
            found = M$id_database[w,include_cols]
            # remove duplicates
            found = found[!duplicated(found),]
            # add tags if requested
            if (!is.null(M$tag)) {
                colnames(found)=paste0(M$tag,'_',colnames(found))
            }
            return(found)
        })
        OUT = do.call(rbind,OUT)
        
        # remove duplicates
        OUT = OUT[!duplicated(OUT),]
       
        # merge with original table
        if (!is.null(M$tag)) {
            db_col=paste0(M$tag,'_',M$database_column)
        } else {
            db_col=M$database_column
        }
        merged = merge(X,OUT,by.x=M$annotation_column,by.y=db_col,all.x=TRUE,sort=FALSE)
    

        
        # put into original order
        merged=merged[order(merged$.orig_order),]
        rownames(merged)=1:nrow(merged)
        w=which(colnames(merged)=='.orig_order')
        merged=merged[,-w]
        
        D$annotations=merged
        
        M$updated=D
        
        return(M)
    }
)





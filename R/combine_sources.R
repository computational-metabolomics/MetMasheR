#' @eval get_description('combine_tables')
#' @export
#' @include annotation_class.R
combine_tables = function(
    source_list,
    matching_columns=NULL,
    keep_cols=NULL,
    tag_ids=FALSE,
    source_col='annotation_table',
    ...) {
    
    # if source list is an annotation_table, make it a list
    if (is(source_list,'annotation_table')) {
        source_list=list(source_list)
    }
    
    # check fcns are all functions
    if (is(source_list,'list')) {
        if (length(source_list)>0) {
            check=all(unlist(lapply(source_list,is,class2='annotation_table')))
            if (!check) {
                stop("all source_list items must be annotation_table objects.")
            }
        }
    }
    
    out=struct::new_struct('combine_tables',
        source_list=source_list,
        matching_columns = matching_columns,
        keep_cols = keep_cols,
        tag_ids = tag_ids,
        source_col = source_col,
        ...)
    return(out)
}



.combine_tables<-setClass(
    "combine_tables",
    contains = c('model'),
    slots=c(
        source_list='entity',
        matching_columns = 'entity',
        keep_cols = 'entity',
        tag_ids = 'entity',
        source_col = 'entity',
        combined_table='entity'
    ),
    
    prototype=list(
        name = 'Combine annotation sources (tables)',
        description = paste0('Annotation tables are joined and matching columns merged.'),
        type = 'univariate',
        predicted = 'combined_table',
        .params=c('source_list','matching_columns','keep_cols','tag_ids','source_col'),
        .outputs=c('combined_table'),
        combined_table=entity(
            name='Combined annotation tables',
            description = 'The annotation tabel after combining the input tables.',
            type='annotation_table'
        ),
        source_list=entity(
            name='Source list',
            description = 'A list of annotation sources to be combined.',
            type='list'
        ),
        matching_columns=entity(
            name='Matching columns',
            description = paste0('A named list of columns to be created by merging columns ',
            'from individual sources. E.g. list("cake" = c("cake_1", "cake2"))',
            'will create a new column called "cake" by merging the "cake_1" and "cake_2" ',
            'if they are found in the input tables.'),
            type=c('list','NULL'),
            value=NULL
        ),
        keep_cols=entity(
            name='Keep columns',
            description = 'A list of column names to keep in the combined table (padded with NA) if detected in one of the input tables.',
            type=c('character','NULL'),
            value=NULL
        ),
        tag_ids = entity(
            name = 'Tag ids',
            description = paste0('Create a column of new identifiers for each annotation ',
            'by merging the id from each source with the annotation tag. e.g.', 
            'an annotation with id = "A1" for source tagged "XY" is given a new ', 
            'id "A1_XY" to prevent duplicate IDs from multiple sources.'),
            type='logical',
            value = TRUE
        ),
        source_col = entity(
            name = 'Source column',
            description = paste0('The column name that will be created to contain ',
                ' a tag to indicate which source the annotation originated from.'), 
            type='character',
            value = 'annotation_table'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("combine_tables",'annotation_table'),
    definition=function(M,D) {
        A = c(M$source_list,D) # makes a list
        M$combined_table = combine_annotations(
                                A = A,
                                matching_columns=M$matching_columns,
                                keep_cols=M$keep_cols,
                                tag_ids=M$tag_ids,
                                source_col=M$source_col)
        
        return(M)
    }
)


#' @export
setMethod(f="model_apply",
    signature=c("combine_tables",'list'),
    definition=function(M,D) {
        A = c(M$source_list,D)
        M$combined_table = combine_annotations(
                                A = A,
                                matching_columns=M$matching_columns,
                                keep_cols=M$keep_cols,
                                tag_ids=M$tag_ids,
                                source_col=M$source_col)
        
        return(M)
    }
)



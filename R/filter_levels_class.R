#' @eval get_description('filter_levels')
#' @export
#' @include annotation_class.R
filter_levels = function(
    column_name,
    levels,
    mode='exclude',
    ...) {
    out=struct::new_struct('filter_levels',
        column_name=column_name,
        levels=levels,
        mode=mode,
        ...)
    return(out)
}



.filter_levels<-setClass(
    "filter_levels",
    contains = c('model'),
    slots=c(
        column_name='entity',
        levels='entity',
        mode='enum',
        filtered='entity',
        flags='entity'
    ),
    
    prototype=list(
        name = 'Filter by factor levels',
        description = paste0('Removes (or includes) annotations such that the named column excludes (or includes) the specified levels.'),
        type = 'univariate',
        predicted = 'filtered',
        .params=c('column_name','levels','mode'),
        .outputs=c('filtered','flags'),
        column_name = entity(
            name = 'Column name',
            description ='The column name to filter.',
            type=c('character'),
            value='V1',
            max_length=1
        ),
        levels = entity(
            name = 'Levels',
            description ='The levels to filter by.',
            type=c('character'),
            value='',
            max_length=Inf
        ),
        mode = enum(
            name = 'Filter mode',
            description =c(
                'exclude'='The specified levels are removed from the annotation table.',
                'include'='Only the specified levels are retained in the annotation table.'
            ),
            type=c('character'),
            value='exclude',
            max_length=1,
            allowed=c('exclude','include')
        ),
        filtered=entity(
            name = 'Filtered annotations',
            description = 'annotation_source after filtering.',
            type='annotation_source',
            max_length = Inf
        ),
        flags=entity(
            name = 'Flags',
            description = 'A list of flags indicating which annotations were removed.',
            value=data.frame(),
            type='data.frame',
            max_length = Inf
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("filter_levels","annotation_source"),
    definition=function(M,D) {
        
        X = D$annotations
        
        if (nrow(X)==0){
            M$filtered=D
            return(M)
        }
        
        # convert to char for comparison
        X[[M$column_name]] = as.character(X[[M$column_name]])
        
        # flag for removal
        if (M$mode=='include') {
            w = which(!(X[[M$column_name]] %in% M$levels))
        } else {
            w = which((X[[M$column_name]] %in% M$levels))
        }
        
        OUT=X[[M$column_name]] < -Inf
        OUT[w]=TRUE
        
        flags = data.frame(flags=OUT,value=X[[M$column_name]])
        colnames(flags)[2]=M$column_name
        
        rownames(flags)=rownames(X)
        
        M$flags=flags
        
        # remove
        X = X[!M$flags[,1],]
        
        D$annotations=X
        
        M$filtered=D
        
        return(M)
    }
)





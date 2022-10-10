#' @eval get_description('lipidmaps_lookup')
#' @export
#' @include annotation_class.R
lipidmaps_lookup = function(
        search_column,
        output_columns,
        return_columns,
        context,
        context_item,
        ...) {
    out=struct::new_struct('lipidmaps_lookup',
        search_column=search_column,
        output_columns=output_columns,
        return_columns=return_columns,
        context=context,
        context_item=context_item,
        ...)
    return(out)
}



.lipidmaps_lookup<-setClass(
    "lipidmaps_lookup",
    contains = c('model'),
    slots=c(
        search_column='entity',
        output_columns='entity',
        return_columns='entity',
        context='entity',
        updated='entity',
        context_item='entity',
        results_table='entity'
    ),
    
    prototype=list(
        name = 'LipidMaps api lookup',
        description = paste0('Search the LipidMaps database using the API'),
        type = 'univariate',
        predicted = 'updated',
        .params=c('search_column','output_columns','return_columns','context','context_item'),
        .outputs=c('updated','results_table'),
        search_column = entity(
            name = 'Search column',
            description ='The column of the annotation table that contains search terms',
            type=c('character'),
            value='V1',
            max_length=1
        ),
        output_columns = entity(
            name = 'Output columns',
            description ='The column names to store the results. Must be the same length as return_columns.',
            type=c('character'),
            value='V1',
            max_length=Inf
        ),
        return_columns = entity(
            name = 'Return columns',
            description=c('The name of the columns to return from the results of the search. Must be value(s) from the following list: ',
                    compound='input,regno,lm_id,name,sys_name,synonyms,abbrev,abbrev_chains,core,main_class,sub_class,exactmass,formula,inchi,inchi_key,hmdb_id,chebi_id,pubchem_cid,smiles,kegg_id',
                    gene='gene_id,lmp_id,gene_name,gene_symbol,gene_synonyms,chromosome,map_location,summary,taxid,species,species_long',
                    protein='uniprot_id,lmp_id,gene_id,gene_name,gene_symbol,taxid,species,species_long,mrna_id,refseq_id,protein_gi,protein_entry,protein_name,seq_length,seq'
                ),
            type=c('character'),
            value='input',
            max_length=Inf
        ),
        updated=entity(
            name = 'Updated annotation',
            description = 'The annotation_source after being updated with the results of the search.',
            type='annotation_source',
            max_length = Inf
        ),
        context=entity(
            name = 'Search context',
            description = 'The search API context. Must be one of compound,gene,protein',
            type='character',
            max_length = Inf
        ),
        context_item=entity(
            name = 'Context item',
            description = c('The context item being searched. Must be:',
                compound='lm_id,formula,inchi_key,pubchem_cid,hmdb_id,kegg_id,chebi_id,smiles,abbrev,abbrev_chains',
                gene='lmp_id,gene_id,gene_name,gene_symbol,tax_id',
                protein='lmp_id,gene_id,gene_name,gene_symbol,tax_id,mrna_id,refseq_id,protein_gi,protein_entry,protein_name,uniprot_id'),
            type='character',
            max_length = Inf
        ),
        results_table=entity(
            name='results table',
            description='The returned search results in full',
            type='data.frame'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("lipidmaps_lookup","annotation_source"),
    definition=function(M,D) {
        
        baseurl='https://www.lipidmaps.org/rest'
        
        expected=list(
            compound=c('input','regno','lm_id','name','sys_name','synonyms','abbrev','abbrev_chains','core','main_class','sub_class','exactmass','formula','inchi','inchi_key','hmdb_id','chebi_id','pubchem_cid','smiles','kegg_id'),
            gene=c('gene_id','lmp_id','gene_name','gene_symbol','gene_synonyms','chromosome','map_location','summary','taxid','species','species_long'),
            protein=c('uniprot_id','lmp_id','gene_id','gene_name','gene_symbol','taxid','species','species_long','mrna_id','refseq_id','protein_gi','protein_entry','protein_name','seq_length','seq')
        )
        if (!all(M$return_columns %in% expected[[M$context]])) {
            stop("return_columns includes a value that isnt from the expected list for this context.")
        }
        
        RESULT=list()
        for (k in 1:nrow(D$annotations)) {
            urlk=paste(baseurl,M$context,M$context_item,URLencode(D$annotations[[M$search_column]][k]),'all','json',sep='/')
            
            # try 3 times in case of time out
            failed=TRUE # flag to detect api failure
            for (attempt in 1:3) {
                response = httr::GET(urlk)
                
                Sys.sleep(0.1) # short delay so we dont overwhelm the api
                
                if (response$status_code<405) { # 404 if compound not found
                    failed=FALSE
                    break  # if no error then dont try again
                }
            }
            
            if (failed) {
                stop('pubchem api failed after 3 attempts')
            }
            
            response = httr::content(response,as = 'text',encoding = 'UTF-8')
            J = jsonlite::fromJSON(response)
            
            if (length(J)>0) {
                if (names(J)[1] == 'input') {
                    J=list(J)
                }
            }
            
            # populate missing columns with NA
            J=lapply(J,function(x){
                n=names(x)
                if (any(!(expected[[M$context]] %in% n))) {
                    x[expected[[M$context]][!(expected[[M$context]] %in% n)]]=NA
                }
                x=x[expected[[M$context]]]# reorder
                return(x)
            })
            
            J=lapply(J,as.data.frame)
            J=do.call(rbind,J)
            
            # return NA if no results
            if (is.null(J)) {
                J=data.frame(matrix(NA,nrow=1,ncol=length(expected[[M$context]])+1))
                colnames(J)=c(expected[[M$context]],'.rowid')
            }
            
            # add ids
            J$.rowid=k
            
            # same order of columns
            J=J[,c(expected[[M$context]],'.rowid')]
            
            RESULT[[k]]=J
        }
        RESULT=do.call(rbind,RESULT)

        
        M$results_table=RESULT
        
        # reduce to requested
        RESULT=RESULT[,c(M$return_columns,'.rowid')]
        colnames(RESULT)=c(M$output_columns,'.rowid')
        
        # bind to annotations
        D$annotations$.rowid=1:nrow(D$annotations)
        D$annotations=dplyr::left_join(D$annotations,RESULT,".rowid")
        
        # remove extra column
        D$annotations$.rowid=NULL
        
        M$updated=D
        return(M)
    }
)





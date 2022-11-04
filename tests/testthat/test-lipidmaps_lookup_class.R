with_mock_dir('lipid_maps_get',{
    test_that('lipidmaps_api search works', {
        db =data.frame(
            dbid = c('A','B','C'),
            rt = c(10,100,200),
            mz = c(499.99,500,500.01),
            lipid=c('not_a_lipid','TG(16:0_16:1_18:2)','another_not_lipid')
        )
        
        AN = lcms_table(
            annotations = db,
            id_column='dbid',
            rt_column='rt',
            mz_column='mz'
        )
        
        M = lipidmaps_lookup(
            search_column  = 'lipid',
            output_columns = c('inchikey','hmdb'),
            return_columns = c('inchi_key','hmdb_id'),
            context = 'compound',
            context_item = 'abbrev_chains'
        )
        
        M = model_apply(M,AN)
        
        out=predicted(M)$annotations
        
        expect_true(out$inchikey[2]=='HDFLQJUGWGNORO-BJPYDGQASA-N')
        expect_true(out$hmdb[2]=='HMDB05379')
        expect_true(is.na(out$inchikey[1]))
        expect_true(is.na(out$inchikey[3]))
        expect_true(is.na(out$hmdb[1]))
        expect_true(is.na(out$hmdb[3]))
    })
})


source('/NAS/wg_zsx/data2/sxz/code/data_tools/vector.tools.r')
gethw =function (src, dest = NULL, obsutil = "/NAS/wg_zsx/bin/obsutil", 
    method = "cp",refresh = F){
    obs.src = sub("/NAS/wg_zsx/data2", "obs://hpc1", src)
    if (is.null(dest)) {
        dest = src
    }
    dest.dir = dir.create(matched_sub(dest, "^.+/"), recursive = T, 
        showWarnings = F)
    obs.cmd = paste(obsutil, method, obs.src, dest, sep = " ", 
        collapse = "")
    if((!file.exists(dest)) | refresh){system(obs.cmd)}
    dest
}
# suppressPackageStartupMessages({
#     require(tidyverse)
#     require(yaml)
# })

# package2py = function(package, package_filename,gzip = F){
#     dir.create(package_filename)
#     to_df = package %>% lapply(is.data.frame) %>% unlist
#     df_names = package%>% names %>% .[to_df]
#     df_filenames = df_names %>% as.list %>% sapply(function(df_name){
#         df_filename = paste0(package_filename,'/',df_name)
#         write.table(x = package[[df_name]], file = df_filename,
#                     quote = F,sep = '\t',
#                     row.names = T,col.names = T)
#         df_filename
#     })
#     pacakge_yaml = paste0(package_filename, '/package.yaml')
#     write_yaml(c(package[!to_df],list(dfs = df_names)), pacakge_yaml , fileEncoding = "UTF-8")
#     if(gzip){
#         wd = getwd()
#         setwd(package_filename)
#         tar.gz = paste0( package_filename,'.tar.gz')
#         zip_cmd = paste0('tar cvfz ',tar.gz,' *')
#         clean_cmd = paste0('rm -Rf ',package_filename)
#         system(zip_cmd)
#         system(clean_cmd)
#         setwd(wd)
#         output = tar.gz
#     }else{output = package_filename}
#     output
# }

# #package2py(package = hmc.package, package_filename = '/NAS/wg_zsx/data3/sxz/test')

# transfer.ssh = function(src, dest, key_file, port = 22,execute =T,dest_isparent = endsWith(dest,'/')){
#     if(dest_isparent){
#         if(!(endsWith(dest,'/'))){dest = paste0(dest,'/')}
#         dest_path = paste0(dest, src %>% extract.between('/','$'))
#     }else{
#         dest_path= dest
#     }
#     transfer.cmd = paste('scp -r -i', key_file, '-P',port,src,dest_path,collapse = ' ')
#     if(execute){system(transfer.cmd)}
#     dest
# }

# #transfer.ssh('/NAS/wg_zsx/data3/sxz/test','ivan@119.3.189.217:/sfs/sxz/test','/home/sxz/.ssh/id_rsa')

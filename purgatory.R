x <- data.table(
  `Sidecar files` = dir('example/', pattern = '.+\\.dop', ignore.case = T)
)

y <- data.table(
  Matches = dir('example/', ignore.case = T)
)

x <- file.path('example/', '20181005123724-_MGL0123.CR2') %>% read_exif()
tempImg <- tempfile(pattern = 'preview_', fileext = '.jpg')
writeBin(base64enc::base64decode(x$PreviewImage), tempImg)
base64enc::base64decode(x$ThumbnailImage, file(tempImg,'wb'))

paste0('data:image/jpeg;', gsub('base64:', 'base64,', x$ThumbnailImage)) %>%
  base64enc::base64decode(., file(tempImg,'wb'))

paste0('data:image/jpeg;', gsub('base64:', 'base64,', x$ThumbnailImage)) %>%
  strsplit(.,",")

x$PreviewImage %>% substr(., 8, nchar(.)) %>% base64enc::base64decode(., file('example/test.jpg', 'wb'))

# fiddling around to make absolutely sure on how to dump images from exif bundle.
path <- 'example/20181003150115-_MGL8642-2-389.CR2';path2 <- substr(path,1L,nchar(path)-4L)
x <- path %>% read_exif(tags = c('ThumbnailImage','PreviewImage'))
con <- file(paste0(path2,'_preview_64.jpg'), 'wb'); x$PreviewImage %>% substr(., 8, nchar(.)) %>% base64enc::base64decode(., con); close(con)
con <- file(paste0(path2,'_thumb_64.jpg'), 'wb'); x$ThumbnailImage %>% substr(., 8, nchar(.)) %>% base64enc::base64decode(., con); close(con)
exiftool_call(args = '-b -PreviewImage -w _preview.jpg', fnames = path)
exiftool_call(args = '-b -ThumbnailImage -w _thumb.jpg', fnames = path)

p1 <- x$PreviewImage %>% substr(., 8, nchar(.))
p2 <- base64encode(paste0(path2, '_preview_64.jpg'))
p3 <- base64encode(paste0(path2, '_preview.jpg'))


t1 <- x$ThumbnailImage %>% substr(., 8, nchar(.))
t2 <- base64encode(paste0(path2, '_thumb_64.jpg'))
t3 <- base64encode(paste0(path2, '_thumb.jpg'))

nchar(c(p1, p2, p3))
nchar(c(t1, t2, t3))

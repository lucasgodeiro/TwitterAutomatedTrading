## code to prepare `DATASET` dataset goes here

positive_dictionary <- my_dictionary[['positive_terms']]

negative_dictionary <- c(my_dictionary[['negative_terms']],'VXX')
my_dictionary <- list(positive_terms = positive_dictionary , negative_terms = negative_dictionary)

usethis::use_data(my_dictionary, overwrite = TRUE, compress = 'xz')

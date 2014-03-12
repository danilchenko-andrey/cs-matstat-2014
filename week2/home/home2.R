download.file(url='http://beta.compscicenter.ru/media/homework_supplement/86/data_home_2.txt', destfile='data/week2-home.txt')

input.file <- file('data/week2-home.txt')
data <- strsplit(readLines(input.file), '\t')
close(input.file)

hist(sapply(data, length), breaks=5)
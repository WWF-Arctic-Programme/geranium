a1 <- readxl::read_excel("Boris.xlsx",sheet=3)
a2 <- readxl::read_excel("Nikita.xlsx",sheet=3)
dim(a1)
dim(a2)
str(a1[[9]])
str(a2[[9]])
identical(a1[[9]],a2[[9]])
#a2[[1]]-a1[[1]]




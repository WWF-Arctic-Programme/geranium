if (packageVersion("ursa")>"3.9.6") {
   remove.packages("ursa")
   install.packages("ursa")
}
#require(ursa)
list1 <- ursa::envi_list(path="predefined",pattern="region",recursive=TRUE)
ursa::envi_remove(list1)


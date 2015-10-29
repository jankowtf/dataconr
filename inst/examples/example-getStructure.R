
# data.frame --------------------------------------------------------------

inst <- data.frame(a = 1, b = 2, c = 3)
getStructure(inst)

# list --------------------------------------------------------------------

inst <- list(a = 1, b = 2, c = 3)
getStructure(inst)

# character ---------------------------------------------------------------

inst <- letters
getStructure(inst)
names(inst) <- letters
getStructure(inst)


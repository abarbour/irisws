".First.lib" <- function(lib, pkg)
{
  library.dynam("Rsac", pkg, lib)
  cat("Rsac is loaded.\n")
}

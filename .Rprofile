# Ensure local project library is always loaded first if present
local({
  local_lib <- file.path(getwd(), "r_libs")
  if (dir.exists(local_lib)) {
    .libPaths(c(local_lib, .libPaths()))
  }
})

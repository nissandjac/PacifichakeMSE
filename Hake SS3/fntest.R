fntest <- function{...}{
  orig_dir <- getwd()
  setwd(tempdir())
  on.exit(setwd(orig_dir))
  All_inputs <- list(..., DLL = DLL)
  save(All_inputs, file = "All_inputs.RData")
  DLL <- All_inputs$DLL
  DLLfull <- paste0(orig_dir, "/", DLL)
  txt <- c("library( TMB )", paste0("dyn.load(dynlib('", DLLfull, 
                                    "'))"), "load( 'All_inputs.RData' )", "Obj <- do.call(TMB::MakeADFun, All_inputs)")
  writeLines(txt, paste0(DLL, ".R"))
  Bdg_output <- gdbsource(paste0(DLL, ".R"))
  if (length(grep("#0", Bdg_output)) > 0) {
    message("Model has errors")
    print(Bdg_output)
    stop()
  }
  TMB::MakeADFun(..., DLL = DLL)
}

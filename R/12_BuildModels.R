library(reticulate)

if (F) {
  # Setup Python Environment
  # Make sure microsoft VS and CUDA is installed before you start
  
  install_miniconda(force = TRUE)
  
  conda_list()
  conda_create("aec_model")
  
  use_condaenv("aec_model")
  conda_install("aec_model","cupy")
  conda_install("aec_model","git")
  conda_install("aec_model","git+https://github.com/StatMixedML/Py-BoostLSS.git",pip=T)
}

use_condaenv("aec_model")


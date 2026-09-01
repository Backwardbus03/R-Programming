
# ==============================================================================
# File: install_setup.R
# Purpose: Complete R + Python + TensorFlow/Keras setup
# Run this script ONLY ONCE
# ==============================================================================

cat("============================================\n")
cat(" R / TensorFlow Installation Setup\n")
cat("============================================\n\n")


# ==============================================================================
# 1. Install R packages
# ==============================================================================

cat("Installing required R packages...\n")

packages <- c(
  "reticulate",
  "tensorflow",
  "keras"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# BiocManager
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# EBImage
if (!requireNamespace("EBImage", quietly = TRUE)) {
  BiocManager::install("EBImage")
}


# ==============================================================================
# 2. Load reticulate
# ==============================================================================

library(reticulate)

Sys.unsetenv("RETICULATE_PYTHON")


# ==============================================================================
# 3. Install Miniconda
# ==============================================================================

cat("\nInstalling Miniconda...\n")

reticulate::install_miniconda(
  path = reticulate::miniconda_path(),
  update = FALSE,
  force = TRUE
)

cat("Miniconda installation finished.\n")


# ==============================================================================
# 4. Check Conda
# ==============================================================================

cat("\nChecking Conda...\n")

conda_path <- reticulate::conda_binary()

cat(
  "Conda found at:\n",
  conda_path,
  "\n"
)


# ==============================================================================
# 5. Create r-reticulate environment
# ==============================================================================

cat("\nCreating r-reticulate environment...\n")

existing_envs <- reticulate::conda_list()

if (!"r-reticulate" %in% existing_envs$name) {
  
  reticulate::conda_create(
    envname = "r-reticulate",
    python_version = "3.10"
  )
  
  cat("r-reticulate environment created.\n")
  
} else {
  
  cat("r-reticulate already exists.\n")
}


# ==============================================================================
# 6. Select Environment
# ==============================================================================

reticulate::use_condaenv(
  "r-reticulate",
  required = TRUE
)


# ==============================================================================
# 7. Install TensorFlow
# ==============================================================================

cat("\nInstalling TensorFlow...\n")

tensorflow::install_tensorflow(
  envname = "r-reticulate"
)


# ==============================================================================
# 8. Test TensorFlow
# ==============================================================================

cat("\nTesting TensorFlow...\n")

library(tensorflow)

tf <- tensorflow::tf

cat(
  "TensorFlow version:",
  tf$`__version__`,
  "\n"
)

cat("\nAvailable devices:\n")

print(
  tf$config$list_physical_devices()
)


# ==============================================================================
# 9. Finished
# ==============================================================================

cat("\n============================================\n")
cat(" INSTALLATION COMPLETE\n")
cat("============================================\n")
cat("\nRestart R/RStudio before running the assignment.\n")
cat("Then run:\n")
cat("Assignment_4_Image_Classification.R\n\n")
#' ---
#' title: "Image Recognition Pipeline"
#' output:
#'   pdf_document:
#'     latex_engine: xelatex
#' ---

# Disable warnings and messages globally during render
knitr::opts_chunk$set(
  warning = FALSE, 
  message = FALSE, 
  verbose = FALSE,
  comment = NA
)

# Environment setup
Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "3")
Sys.setenv(TF_ENABLE_ONEDNN_OPTS = "0")

library(EBImage)
library(keras3)

img_dir <- getwd()
pictures <- c('p1.jpg', 'p2.jpg', 'p3.jpg', 'p4.jpg', 'p5.jpg', 'p6.jpg', 
              'c1.jpg', 'c2.jpg', 'c3.jpg', 'c4.jpg', 'c5.jpg', 'c6.jpg')

picarr <- list()
for(i in 1:12) {
  picarr[[i]] <- readImage(file.path(img_dir, pictures[i]))
}

for(i in 1:12) {
  picarr[[i]] <- resize(picarr[[i]], 28, 28)
}

for (i in 1:12) {
  picarr[[i]] <- array_reshape(picarr[[i]], c(28, 28, 3))
}

x_train <- NULL
for (i in c(1:5, 7:11)) {
  x_train <- rbind(x_train, as.vector(picarr[[i]]))
}


x_test <- rbind(as.vector(picarr[[6]]), as.vector(picarr[[12]]))

y_train <- c(0,0,0,0,0,1,1,1,1,1)
y_test  <- c(0, 1)

train_labels <- to_categorical(y_train)
test_labels  <- to_categorical(y_test)

model <- keras_model_sequential(
  layers = list(
    keras_input(shape = c(2352)),
    layer_dense(units = 256, activation = 'relu'),
    layer_dense(units = 128, activation = 'relu'),
    layer_dense(units = 2, activation = 'softmax')
  )
)

model %>% compile(
  loss = 'categorical_crossentropy', 
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, train_labels, 
  epochs = 30, 
  batch_size = 32, 
  validation_split = 0.2,
  verbose = 0
)

plot(history)

model %>% evaluate(x_train, train_labels, verbose = 0)

prob <- model %>% predict(x_train, verbose = 0)
pred <- max.col(prob) - 1
table(Predicted = pred, Actual = y_train)
cbind(prob, Predicted = pred, Actual = y_train)
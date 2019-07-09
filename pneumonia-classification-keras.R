library(keras)
library(pROC)
library(caret)

setwd("C:/Users/J/Documents/R Projects/Data/chest-xray-pneumonia/chest_xray/chest_xray")

train_dir  = 'C:/Users/J/Documents/R Projects/Data/chest-xray-pneumonia/chest_xray/chest_xray/train'
validation_dir  = 'C:/Users/J/Documents/R Projects/Data/chest-xray-pneumonia/chest_xray/chest_xray/val'
test_dir = 'C:/Users/J/Documents/R Projects/Data/chest-xray-pneumonia/chest_xray/chest_xray/test'
set.seed(123)

train_datagen = image_data_generator(
  rescale = 1/255,
  rotation_range = 5,
  width_shift_range = 0.1,
  height_shift_range = 0.05,
  shear_range = 0.1,
  zoom_range = 0.15,
  horizontal_flip = TRUE,
  vertical_flip = FALSE,
  fill_mode = "reflect"
)

validation_datagen <- image_data_generator(rescale = 1/255)  
test_datagen <- image_data_generator(rescale = 1/255)  

training_batch_size = 32
validation_batch_size = 32

train_generator <- flow_images_from_directory(
  train_dir,                            # Target directory  
  train_datagen,                        # Data generator
  classes = c('NORMAL', 'PNEUMONIA'),
  target_size = c(224, 224),            # Resizes all images
  batch_size = training_batch_size,
  class_mode = "categorical",
  shuffle = T,
  seed = 123
)

validation_generator <- flow_images_from_directory(
  validation_dir,
  classes = c('NORMAL', 'PNEUMONIA'),
  validation_datagen,
  target_size = c(224, 224),
  batch_size = validation_batch_size,
  class_mode = "categorical",
  shuffle = T,
  seed = 123
)

test_generator <- flow_images_from_directory(
  test_dir,
  classes = c('NORMAL', 'PNEUMONIA'),
  test_datagen,
  target_size = c(224, 224),
  batch_size = 1,
  class_mode = "categorical",
  shuffle = FALSE
)

#Transfer learning

conv_base <- application_inception_resnet_v2(
  weights = 'imagenet',
  include_top = FALSE,
  input_shape = c(224, 224, 3)
)

#Create a new top,
model <- keras_model_sequential() %>% 
  conv_base %>% 
  layer_global_average_pooling_2d(trainable = T) %>%
  layer_dropout(rate = 0.2, trainable = T) %>%
  layer_dense(units = 224, activation = "relu", trainable = T) %>% 
  layer_dense(units = 2, activation = "softmax", trainable = T)

#Don't train the base,
freeze_weights(conv_base)

set.seed(123)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-6),
  metrics = c("accuracy")
)

training_step_size = ceiling(length(list.files(train_dir, recursive = T)) / training_batch_size)
validation_step_size = ceiling(length(list.files(validation_dir, recursive = T)) / validation_batch_size)


pneumonia_adjustment = length(list.files(paste(train_dir, '/NORMAL/', sep = ""), recursive = T)) / length(list.files(paste(train_dir, '/PNEUMONIA/', sep = ""), recursive = T))

set.seed(123)

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  class_weight = list("0"=1,"1"=pneumonia_adjustment),
  epochs = 30,
  validation_data = validation_generator,
  validation_steps = ceiling(length(list.files(validation_dir, recursive = T)) / validation_batch_size)
)

plot(history)

#Make predictions on the test set,
preds = predict_generator(model,
                          test_generator,
                          steps = length(list.files(test_dir, recursive = T)))

#Do some tidying,
predictions = data.frame(test_generator$filenames)
predictions$prob_pneumonia = preds[,2]
colnames(predictions) = c('Filename', 'Prob_Pneumonia')

head(predictions, 10)
predictions$Class_predicted = 'Normal'
predictions$Class_predicted[predictions$Prob_Pneumonia >= 0.5] = 'Pneumonia'
predictions$Class_actual = 'Normal'
predictions$Class_actual[grep("PNEUMONIA", predictions$Filename)] = 'Pneumonia'


predictions$Class_actual = as.factor(predictions$Class_actual)
predictions$Class_predicted = as.factor(predictions$Class_predicted)

#ROC,
roc = roc(response = predictions$Class_actual, 
          predictor = as.vector(predictions$Prob_Pneumonia), 
          ci=T,
          levels = c('Normal', 'Pneumonia'))
threshold = coords(roc, x = 'best', best.method='youden')
threshold

#Boxplot
boxplot(predictions$Prob_Pneumonia ~ predictions$Class_actual,
        main = 'Probabilities of Normal vs Pneumoia',
        ylab = 'Probability',
        col = 'light blue')
abline(h=threshold[1], col = 'red', lwd = 3)

predictions$Class_predicted = 'Normal'
predictions$Class_predicted[predictions$Prob_Pneumonia >= threshold[1]] = 'Pneumonia'

#Create a confusion matrix
cm = confusionMatrix(table(predictions$Class_predicted, predictions$Class_actual),positive = 'Pneumonia')
cm

paste("AUC =", round(roc$auc, 3))
plot(roc)
ci_sens = ci.se(roc, specificities = seq(from=0, to=1, by=0.01), boot.n = 2000)
 plot(ci_sens, type="shape", col="#00860022", no.roc=TRUE)
abline(h=cm$byClass['Sensitivity'], col = 'red', lwd = 2)
abline(v=cm$byClass['Specificity'], col = 'red', lwd = 2)

#Find high sensitivity points, but not so high that specificity is terrible,
thresholds = coords(roc, x = "local maximas")
thresholds = as.data.frame(t(thresholds))
thresholds[(thresholds$sensitivity > 0.85 & thresholds$sensitivity < 0.9),]


# keras_save(model, path = "C:/Users/J/Documents/R Projects/Data/chest-xray-pneumonia/chest_xray/chest_xray/kerasmodel.h5")
save_model_weights_hdf5(model, "C:/Users/J/Documents/R Projects/Data/chest-xray-pneumonia/chest_xray/chest_xray/keras/kerasmodel.h5", overwrite = TRUE)
load_model_weights_hdf5(model, "C:/Users/J/Documents/R Projects/Data/chest-xray-pneumonia/chest_xray/chest_xray/keras/kerasmodel.h5", by_name = FALSE,
                        skip_mismatch = FALSE, reshape = FALSE)


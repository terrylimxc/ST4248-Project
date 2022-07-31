library(ISLR)
library(torch)
library(luz) # high-level interface for torch
library(torchvision) # for datasets and image transformation
library(torchdatasets) # for datasets we are going to use
library(zeallot)
library(caret)
torch_manual_seed(13)

data <- read.csv("C:/Users/terry/Downloads/data_scaled.csv")
data <- data[-c(1)] # Remove column X
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)], 
                                         as.factor)
data$NUM_DRINKSPERWK <- as.integer(data$NUM_DRINKSPERWK)
data$NUM_POORMENTHLTH <- as.integer(data$NUM_POORMENTHLTH)
data$NUM_POORPHYHLTH <- as.integer(data$NUM_POORPHYHLTH)
data$NUM_SLEEP <- as.integer(data$NUM_SLEEP)

attach(data)

set.seed(4248)
idx <- createDataPartition(y = IS_DIABETIC.1, p=0.7, list=FALSE)
#train <- data[idx,]
train <- idx

train <- downSample(x = subset(data[train,], 
                               select = -c(IS_DIABETIC.1)),
                    y = as.factor(data$IS_DIABETIC.1[train]))

test <- data[as.numeric(rownames(data[-idx,])),]

train_x <- model.matrix(Class ~ . - 1, data = train)
train_y <- as.numeric(train$Class)-1 #0 for No, 1 for Yes
test_x <- model.matrix(IS_DIABETIC.1 ~ . - 1, data = test)
test_y <- as.numeric(test$IS_DIABETIC.1)-1 #0 for No, 1 for Yes


###
modnn <- nn_module(
  initialize = function(input_size) {
    self$hidden <- nn_linear(input_size, 100)
    self$hidden1 <- nn_linear(100, 50)
    self$hidden2 <- nn_linear(50, 20)
    self$activation <- nn_relu()
    self$dropout <- nn_dropout(0.2)
    self$output <- nn_linear(20, 1)
  },
  forward = function(x) {
    x %>%
      self$hidden() %>%
      self$activation() %>%
      self$dropout() %>%
      
      self$hidden1() %>%
      self$activation() %>%
      self$dropout() %>%
      
      self$hidden2() %>%
      self$activation() %>%
      self$dropout() %>%
      
      self$output() %>%
      torch_flatten(start_dim = 1)
  }
)
###

###
modnn <- modnn %>%
  setup(
    loss = nn_bce_with_logits_loss(),
    optimizer = optim_rmsprop,
    metrics = list(luz_metric_binary_accuracy_with_logits())
  ) %>%
  set_hparams(input_size = ncol(train_x))

###

###
system.time(
fitted <- modnn %>%
  fit(
    data = list(torch_tensor(train_x,dtype=torch_float()), torch_tensor(train_y)),
    valid_data = list(torch_tensor(test_x,dtype=torch_float()), torch_tensor(test_y)),
    dataloader_options = list(batch_size = 256),
    epochs = 15
  )
)
###

###
library(ModelMetrics)
plot(fitted)
y.nn <- {torch_sigmoid(predict(fitted,list(torch_tensor(test_x,dtype=torch_float()), torch_tensor(test_y)) )) > 0.5} %>% as.integer()
table(predict = y.nn, truth = test_y)
conf <- table(predict = y.nn, truth = test_y)
(conf[1,1]+conf[2,2]) / length(y.nn)
recall <- conf[2,2] / (conf[2,2] + conf[1,2])
recall
precision <- conf[2,2] / (conf[2,2] + conf[2,1])
precision
auc(test_y, y.nn)
f1 <- 2*(precision*recall)/ (precision + recall)
f1
###

my.glm = glm(IS_DIABETIC~. -1, data=data[-testid,],family="binomial")
y.lr = as.numeric(predict.glm(my.glm,newdata = data[testid,], type="response")>0.5)
table(y.nn,y[testid])
table(y.lr,y[testid])
02_rf

library(sf)
library(iml)
library(vip)
library(mlr3)
library(ranger)
library(mlr3tuning)
library(mlr3learners)

# format the data
# df <- st_drop_geometry(acs_data)

names(acs_data_new)
df <- acs_data_new |>
  dplyr::select(!c("pob_p_pr", "pob_p_us", 
                   "pob_p_elsewhere", 
                   "pob_p_foreign",
                   "pop_p_hisp_other"))
# mutate(id = 1:nrow(df))
# rownames(df) <- 1:nrow(df)

summary(df)

# df <- acs_data_new

# df <- df |>
#   dplyr::select(3:7) |>
#   mutate(id = 1:nrow(df))

# define the task
task_other <- TaskRegr$new(id = "df", backend = df, 
                           target = "pop_p_other")

task_other$col_roles

# define the performance measure
measure = msr("regr.rmse")

## Define the learner
lrn_rf = lrn("regr.ranger",
             predict_type = "response",
             importance = "permutation" )

## Define the resampling method
## 1st Run - Simple holdout 0.8
resamp_hout = mlr3::rsmp("holdout",
                         ratio = 0.8)

## Instantiate the resampling method
resamp_hout$instantiate(task_other)

# run
rf_other = resample(task = task_other,
                    learner = lrn_rf,
                    resampling = resamp_hout,
                    store_models = TRUE)


# fit

rf_other$score(measure)

rf_other$aggregate(measure)

## variable importance

vi_other <- vi(rf_other$learners[[1]])


vip(vi_other)
library(ggplot2)
ggplot(vi_other, aes(reorder(Variable, Importance), Importance)) + 
  geom_bar(stat="identity", color="NA", 
           position=position_dodge()) + 
  # geom_errorbar(aes(ymin = Importance-StDev, 
  #                   ymax = Importance+StDev), width = 0.2) +
  coord_flip() + theme_bw() + scale_x_discrete(name = "Variable")


## partial dependence plots


# lrn_rf$train(task_other)
lrn_rf$model

x = df[which(names(df) != "pop_p_other")]

predictor = Predictor$new(model = rf_other,
                          data = x,
                          y = df$pop_p_other)
imp = FeatureImp$new(predictor = predictor,
                     loss = "rmse")

p1 <- pdp::partial(rf_other, 
                   train = task_other$data(),
                   pred.var = "age_p_0_5", 
                   prob = TRUE,
                   plot = TRUE, rug = TRUE, 
                   plot.engine = "ggplot2") + theme_light() + 
  ggtitle(paste("PDP:", "test")) 
print(p1)

pdp::partial(lrn_rf, 
             train = task_other$data(),
             pred.var = "age_p_0_5", 
             prob = TRUE,
             plot = TRUE, rug = TRUE, 
             plot.engine = "ggplot2") + theme_light() + 
  ggtitle(paste("PDP:", "test"))

ale = FeatureEffect$new(predictor = predictor,
                        feature = "age_p_0_5",
                        grid.size = 10)

rf.lstat = FeatureEffect$new(predictor = rf_other,
                             feature = "lstat")

ale$plot()
rf_other$task


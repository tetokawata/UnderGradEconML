pacman::p_load(
  tidyverse,
  mlr3verse,
  mlr3mbo
)

Raw <- read_csv("Public/Example.csv") |> 
  select(-District)

Task <- as_task_regr(
  Raw,
  "Price"
)

Tree <- lrn("regr.rpart") |> 
  lts()

Terminal <- trm(
  "evals",
  n_evals = 100)

TreeMbo <- AutoTuner$new(
  tuner = tnr("mbo"),
  learner = Tree,
  resampling = rsmp("holdout"),
  terminator = Terminal
)

TreeMbo$id <- "MBO"

TreeRandom <- AutoTuner$new(
  tuner = tnr("random_search"),
  learner = Tree,
  resampling = rsmp("holdout"),
  terminator = Terminal
)

TreeRandom$id <- "Random"

Design <- benchmark_grid(
  tasks = Task,
  learners = list(
    TreeRandom,
    TreeMbo
    ),
  resamplings = rsmp(
    "holdout",
    ratio = 0.8) 
)

BenchMark <- benchmark(Design)

BenchMark$aggregate(msr("regr.rsq"))

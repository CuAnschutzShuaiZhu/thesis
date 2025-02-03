
i <- 1
sample_size <- 200
datapartition <- 0.3

sample <- run_simulation_train_test(1,200,0.5)
get_element(sample)%>%print()
get_evaluation_metric2(sample)

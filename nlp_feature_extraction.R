library(superml)

set_tokenizer = function(codestates_data, n){
  CountVec <- CountVectorizer$new(max_features = n, remove_stopwords = c("\n", "\r")) # find top  terms in data
  CountVec$fit(codestates_data["Code"])
  CountVec
}

extract_features_dataset = function(subject_data, tokenizer, main_data, early_dataset, codestates_data)
{
  dataset_features <- c()
  student_ids <- unique(subject_data$SubjectID)
  for (id in student_ids){  # create row with all features
    tf_of_all_problems <- find_tf_of_all_problems(id, tokenizer, main_data, early_dataset, codestates_data)
    dataset_features <- rbind(dataset_features, unname(tf_of_all_problems))
  }
  as.tibble(dataset_features)
}

find_tf_of_all_problems = function(
    stud_id,
    tokenizer,
    main_data,
    early_dataset,
    codestates_data
){
  tf_problem <- c()
  problem_ids <- unique(early_dataset$ProblemID)
  for (pro_id in problem_ids){
    tf_problem <- c(tf_problem, unname(find_tf_of_problem(stud_id, pro_id, tokenizer, main_data, codestates_data)))
  }
  tf_problem
}

find_tf_of_problem = function(
    stud_id,
    pro_id,
    tokenizer,
    main_data,
    codestates_data
){
  # find code states ids that are associated to student id and problem id 
  student_main_data <- filter(main_data,(SubjectID == stud_id) & (ProblemID == pro_id))
  student_main_data <- unique(student_main_data["CodeStateID"]) 
  
  # load all code submissions based on code state ids
  code_sub <- filter(codestates_data, (codestates_data$CodeStateID %in% student_main_data$CodeStateID))
  code_sub <- code_sub["Code"]
  
  if (dim(code_sub)[1] == 0 | dim(code_sub)[2] == 0 | is.null(dim(code_sub))){
    features <- tokenizer$transform(" ")
  }else{
    features <- tokenizer$transform(code_sub$Code)  # tokenize
    features <- colSums(features) # count number of terms
  }
  features
}

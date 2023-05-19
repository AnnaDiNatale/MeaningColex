similarities_from_adjacency = function(adjmat, beta) {
  #Compute similarity matrix from adjacency matrix
  
  norm = scale(adjmat, center = FALSE, scale = colSums(adjmat))
  ones = matrix(0, nrow(adjmat), ncol(adjmat))
  diag(ones) = 1
  before_inv = ones - beta * norm

  write.table(before_inv, "before_inv.csv", row.names = FALSE, col.names = FALSE, sep = ",")
  ##we need to save the table because the inversion will be done with a pyhton script
  #Sprint("Table saved")
  
  #Run python script
  #print('Run python script')
  use_python("/usr/bin/python3.8", required = T) #INSERT YOUR PYTHON 3.8 path
  py_run_file("invert.py")
  inverse = data.matrix(read.csv("after_inv.csv", header = FALSE)) ##loading the result of the Python script
  
  #Compute similarities
  # print('Compute similarities')
  result = inverse %*% norm
  file.remove("before_inv.csv")
  file.remove("after_inv.csv")
  
  return(result)
}
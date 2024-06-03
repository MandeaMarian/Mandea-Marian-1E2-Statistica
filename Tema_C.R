
#C1 

# Punctul a)
generate_permutation = function(n) {
  U = runif(n)
  # Get the permutation by sorting the indices based on the random values
  permutation = order(U)
  return(permutation)
}


generate_binary_strings = function(n, k) {
  
  binary_matrix = matrix(sample(0:1, n * k, replace = TRUE), nrow = n, ncol = k)
  
  
  binary_strings = apply(binary_matrix, 1, function(x) paste(x, collapse = ""))
  
  return(binary_strings)
}


sort_binary_strings = function(binary_strings, permutation) {
  sorted_strings = binary_strings[permutation]
  return(sorted_strings)
}


main = function(n, k) {
  permutation = generate_permutation(n)
  binary_strings = generate_binary_strings(n, k)
  
  sorted_strings = sort_binary_strings(binary_strings, permutation)
  
  cat("Generated Permutation:\n", permutation, "\n")
  cat("Generated Binary Strings:\n", binary_strings, "\n")
  cat("Sorted Binary Strings:\n", sorted_strings, "\n")
  
  return(sorted_strings)
}

n = 5
k = 4
main(n, k)


# Punctul b)

compare_bitstrings = function(Wi, Wj) {
  Lij = min(length(Wi), length(Wj))
  
  for (h in 1:Lij) {
    if (Wi[h] < Wj[h]) {
      return(TRUE)  
    } else if (Wi[h] > Wj[h]) {
      return(FALSE)
    }
  }
  
  if (length(Wi) < length(Wj)) {
    while (length(Wi) < length(Wj)) {
      Wi = c(Wi, sample(0:1, 1, replace = TRUE))
      if (Wi[length(Wi)] != Wj[length(Wi)]) {
        return(Wi[length(Wi)] < Wj[length(Wi)])
      }
    }
  } else if (length(Wj) < length(Wi)) {
    while (length(Wj) < length(Wi)) {
      Wj = c(Wj, sample(0:1, 1, replace = TRUE))
      if (Wj[length(Wj)] != Wi[length(Wj)]) {
        return(Wj[length(Wj)] < Wi[length(Wj)])
      }
    }
  }
  
  while (TRUE) {
    new_bit_Wi = sample(0:1, 1, replace = TRUE)
    new_bit_Wj = sample(0:1, 1, replace = TRUE)
    Wi = c(Wi, new_bit_Wi)
    Wj = c(Wj, new_bit_Wj)
    if (new_bit_Wi < new_bit_Wj) {
      return(TRUE)
    } else if (new_bit_Wi > new_bit_Wj) {
      return(FALSE)
    }
  }
}

set.seed(123)
Wi = sample(0:1, 4, replace = TRUE)
Wj = sample(0:1, 4, replace = TRUE)

cat("Wi:", Wi, "\n")
cat("Wj:", Wj, "\n")

is_less = compare_bitstrings(Wi, Wj)
cat("Wi este lexicografic strict mai mic decat Wj:", is_less, "\n")



#Punctul c)

compare_words = function(word1, word2) {
  
  if (word1 < word2) {
    return(-1)
  } else if (word1 > word2) {
    return(1)
  } else {
    return(0)
  }
}

partition = function(arr, low, high) {
  
  pivot_index = sample(low:high, 1)
  arr[c(pivot_index, high)] = arr[c(high, pivot_index)]
  pivot = arr[high]
  
  i = low - 1
  for (j in low:(high - 1)) {
    if (compare_words(arr[j], pivot) < 0) {
      i =i + 1
      arr[c(i, j)] = arr[c(j, i)]
    }
  }
  
  arr[c(i + 1, high)] = arr[c(high, i + 1)]
  return(i + 1)
}

quicksort = function(arr, low, high) {
  
  if (low < high) {
    pi = partition(arr, low, high)
    arr = quicksort(arr, low, pi - 1)
    arr = quicksort(arr, pi + 1, high)
  }
  return(arr)
}

sort_binary_strings = function(binary_strings) {
  return(quicksort(binary_strings, 1, length(binary_strings)))
}

binary_strings = c("101", "010", "111", "001", "100", "011", "000", "110")
sorted_strings = sort_binary_strings(binary_strings)
print(sorted_strings)





# C2: 

# Funcție pentru generarea grafului G
generate_graph = function(num_nodes, num_edges) {
  edges = matrix(sample(1:num_nodes, 2 * num_edges, replace = TRUE), ncol = 2)
  return(list(num_nodes = num_nodes, edges = edges))
}

# Funcție pentru determinarea cardinalului tăieturii E(A, B)
calculate_cut_cardinality = function(A, B, edges) {
  cut_edges = sum(apply(edges, 1, function(edge) {
    (edge[1] %in% A && edge[2] %in% B) || (edge[1] %in% B && edge[2] %in% A)
  }))
  return(cut_edges)
}

# Funcție pentru determinarea unei tăieturi
random_cut = function(graph) {
  num_nodes = graph$num_nodes
  edges = graph$edges
  n = floor(num_nodes / 2)
  
  A = sample(1:num_nodes, n)
  B = setdiff(1:num_nodes, A)
  cardinality = calculate_cut_cardinality(A, B, edges)
  
  return(list(A = A, B = B, cardinality = cardinality))
}


run_random_cut_algorithm = function(num_nodes, num_edges) {
  set.seed(42)
  graph = generate_graph(num_nodes, num_edges)
  result = random_cut(graph)
  
  cat("Setul A:", toString(result$A), "\n")
  cat("Setul B:", toString(result$B), "\n")
  cat("Cardinalul tăieturii este:", result$cardinality, "\n")
}


num_nodes = 10 # Numărul total de noduri (2n sau 2n + 1)
num_edges = 15 # Numărul de muchii


run_random_cut_algorithm(num_nodes, num_edges)
# Punctul b)
# b) Pentru a crește șansele de a găsi o tăietură de cardinal cât mai mare, rulam algoritmul de mai multe ori și să păstrăm tăietura cu cardinalul maxim găsit. Aceasta se bazează pe faptul că rularea multiplă a unui algorit crește probabilitatea de a găsi soluția optimă.









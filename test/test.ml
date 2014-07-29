open LMI

let _ = 
	let mat_map f = List.map (fun row -> List.map f row) in

	let a = 
  Matrix.Num_mat.matrix_of_list_list (mat_map Matrix.num_of_string  [
    ["0.9379"; "-0.0381"; "-0.0414"];
    ["-0.0404";"0.968";"-0.0179"];
    ["0.0142"; "-0.0197"; "0.9823"];
  ]) in

	let b = 
  Matrix.Num_mat.matrix_of_list_list (mat_map Matrix.num_of_string [
    ["0.0237"];
    ["0.0143"];
    ["0.0077"];
  ]
  ) in
	let c = Num_mat.kronecker_sym 10 4 6 in
	Num_mat.pp_matrix_expr (Format.std_formatter) c;
	Format.printf "\n";
	exit 0;


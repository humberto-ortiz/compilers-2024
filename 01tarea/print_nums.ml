(* print_nums - print the numbers up to n *)
(* Humberto Ortiz-Zuazaga *)

(* first try, prints numbers down from n to 0 *)
let rec print_nums n =
  print_int n;
  print_newline ();
  if n <= 1 then
    ()
  else
    print_nums (n - 1)
in
  print_nums 5

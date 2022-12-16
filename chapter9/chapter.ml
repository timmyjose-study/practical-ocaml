(*
  APis already included by the Pervasives module: 

  // Note that these are all buffered

  open_in, open_out
  open_in_binary, open_out_binary
  open_in_gen, open_out_gen, // can specify modes
  input_char, output_char,
  input_line, output_string,
  input, output // operates on string buffer
  really_input, really_output, // operates on string buffer; partial read/write not allowed
  pos_in, pos_out,
  seek_in, seek_out,
  in_channel_length, out_channel_length,
  close_in, close_out
*)

open Random;;
Random.self_init ();;

(* print the contents of the file *)
let catfile filename = 
  let rec print_all_lines in_chan = 
    output_string stdout ((input_line in_chan) ^ "\n");
    print_all_lines in_chan
  in
    let in_file = open_in filename in
    try
      print_all_lines in_file
    with
      End_of_file -> close_in in_file

(* read a random chunk of data from the file *)
let random_catfile filename = 
  let in_file = open_in filename in
  let len = (Unix.stat filename).Unix.st_size in
  let start = Random.int len in
  let segment = Random.int (len - start) in
  let buf = Buffer.create segment in
  let _ = 
    seek_in in_file start;
    Buffer.add_channel buf in_file segment in
  close_in in_file;
  output_string stdout (Buffer.contents buf)

(* copy the contents of the input file to the given output file, creating
 the output file if it doesn't already exist
 *)
let copy_file infilename outfilename = 
  let rec copy_contents in_file out_file =
    try
      output_string out_file ((input_line in_file) ^ "\n");
      copy_contents in_file out_file
    with
      End_of_file -> close_out out_file; close_in in_file;
  in
    let in_file = open_in infilename in
    let out_file = open_out outfilename in
    copy_contents in_file out_file

(* seeks and writes in an output file *)
let seek_demo filename = 
  let file = open_out filename in
  Printf.fprintf file "Hello, %s\n" "world";
  seek_out file 10a0;
  Printf.fprintf file "Adios, %s\n" "mundo!";
  close_out file
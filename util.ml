let read_process command =
  let buffer_size = 2048 in
  let buffer = Buffer.create buffer_size in
  let string = String.create buffer_size in
  let in_channel = Unix.open_process_in command in
  let chars_read = ref 1 in
  while !chars_read <> 0 do
    chars_read := input in_channel string 0 buffer_size;
    Buffer.add_substring buffer string 0 !chars_read
  done;
  Pervasives.ignore (Unix.close_process_in in_channel);
  Buffer.contents buffer

let write_to_temp_file text =
  let tmp_path = Filename.temp_file "" "" in
  let tmp_file = open_out tmp_path in
  let _ = output_string tmp_file text in
  (tmp_path)

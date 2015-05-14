open Core_kernel.Std
open Bap.Std
open Project
open Option
open Format

module Cmdline = struct
  open Cmdliner

  let info =
    let doc =
      "Output addresses for function arguments" in
    Term.info ~doc "Argument finder"

  let outfile : string Term.t =
    let doc = "New line separated list of addresses
      of arguments to functions" in
    Arg.(value & opt string "" & info ["outfile"] ~doc)

  let process_args outfile =
    outfile

  let parse argv =
    Term.eval ~argv (Term.(pure process_args $outfile), info)
    |> function
    | `Ok x -> x
    | _ -> exit 1
end

open Custom_arm_abi

let write_output output outfile =
  let filename = outfile in
  Out_channel.with_file filename ~f:(fun chan ->
      Out_channel.output_lines chan (output |> List.map ~f:(fun x ->
          sprintf "%x" (Word.to_int x |> ok_exn))))

(*
let write_output_syms syms_and_bounds =
  let lines =
    List.map syms_and_bounds ~f:(fun x ->
        let sym,min,max = x in
        sprintf "%s %x %x" sym min max) in
  let filename = Sys.getenv "OUTFILEFUN" in
  Out_channel.with_file filename ~f:(fun chan ->
      Out_channel.output_lines chan lines)
*)

(* Retrieve the statements which populate function call arguments
 * (see comments above) *)
let get_stmts block exps =
  let rev_bil =
    Block.insns block |> List.map ~f:(fun (mem,insn) ->
        List.cartesian_product [mem] (Insn.bil insn)) |>
    List.concat |> List.rev in
  List.fold rev_bil ~init:[] ~f:(fun acc (mem,stmt) ->
      match stmt with
      | Bil.Move (var,expr) ->
        let exp_var = Bil.var var in
        if List.exists exps ~f:(fun v -> v = exp_var) &&
           (not @@ List.exists acc ~f:(fun (_mem,v) -> v = exp_var)) then
          (mem,exp_var) :: acc else acc
      | _ -> acc)

(* Gets the ABI information of a [dest_block]. The custom ABI module
 * only returns ABI information here if it's one of the funtions we
 * are interested in, e.g. strcpy *)
let handle_dest block dest_block project parent_sym parent_mem acc =
  let res = Table.find_addr project.symbols @@ Block.addr dest_block in
  Option.fold ~init:acc res ~f:(fun acc (mem, target) ->
      (* if the target matches strcpy and other funcs, we want to find its args *)
      let abi = new Custom_arm_abi.custom ~sym:target mem dest_block in
      let args = abi#args in
      if List.length args > 0 then
        begin
          let extract_args = List.map args ~f:snd in
          let result = get_stmts block extract_args in
          List.fold ~init:acc result ~f:(fun acc (mem,stmt) ->
              let hex = Addr.to_int (Memory.min_addr mem) |> ok_exn in
              Format.printf "%x\n" hex;
              let st = Disasm.insn_at_addr project.disasm (Memory.min_addr mem) in
              match st with
              | Some (mem,insn) ->
                (* symbol, address of the arg of interest, min and max addr of this funciton *)
                ((parent_sym, Memory.min_addr mem), (Memory.min_addr parent_mem, Memory.max_addr parent_mem)) :: acc
              | None -> acc)
        end
      else acc)

(* Gets blocks which are associated with calls and passes
 * these on to attempt retrieval of args *)
let analyze sym bound entry project =
  let blocks = Block.dfs ~bound entry in
  Seq.fold ~init:[] blocks ~f:(fun acc block ->
      Seq.fold ~init:acc (Block.dests block) ~f:(fun acc dests ->
          match dests with
          | `Unresolved _ -> acc
          | `Block (_, `Cond) -> acc
          | `Block (_, `Fall) -> acc
          | `Block (dest_block, `Jump) ->
            handle_dest block dest_block project sym bound acc))

let main args project =
  let output =
    Table.foldi ~init:[] project.symbols ~f:(fun mem sym acc ->
        (* find block associated with mem of sym *)
        match Table.find (Disasm.blocks project.disasm) mem with
        | None -> Format.eprintf "Symbol %a undefined!\n" String.pp sym; acc
        | Some entry_block ->
          (* TODO needs a separate type *)
          analyze sym mem entry_block project @ acc) in
  let addrs = List.map output ~f:(fun x -> fst x |> snd) in
(*
  let sym_and_bounds = List.map output ~f:(fun x ->
      let sym = fst x |> fst in
      let min = snd x |> fst |> Word.to_int |> ok_exn in
      let max = snd x |> snd |> Word.to_int |> ok_exn in
      (sym,min,max)) |> List.dedup in
*)
  let outfile = Cmdline.parse args in
  write_output addrs outfile;
(*   write_output_syms sym_and_bounds; *)
  project

let () = register_plugin_with_args  main

open Core_kernel.Std
open Bap.Std
open Project

class custom ?image ?sym mem blk = object(self)
  inherit ARM.ABI.stub
  method! id = ["gnueabi"; "linux"; "unknown"; "custom"]
  method! specific = true
  method! choose other =
    if List.mem other#id "gnueabi" then
      Int.compare (List.length self#id) (List.length other#id)
    else 0

  method! return_value = Some (Bil.var ARM.CPU.r0)
  method! args =
    let vars = match sym with
(*
      | Some "strlen"
      | Some "gets"
      | Some "malloc" -> [r0]
      | Some "snprintf"
      | Some "strcat"
      | Some "strcpy"
*)
      | Some "__gets_chk"
      | Some "__printf_chk" -> [ARM.CPU.r0; ARM.CPU.r1]
(*
      | Some "memset"
      | Some "fgets"
      | Some ".sprintf" (* TODO handle varable length args *)
      | Some "strncpy"
      | Some "memcpy"
*)
      | Some "__strcat_chk"
      | Some "__stpcpy_chk" -> [ARM.CPU.r0; ARM.CPU.r1; ARM.CPU.r2]
      | Some "__memmove_chk"
      | Some "__fgets_chk"
      | Some "__strncat_chk"
      | Some "__strncpy_chk"
      | Some "__memcpy_chk"
      | Some "__mempcpy_chk"
      | Some "__memset_chk" -> [ARM.CPU.r0; ARM.CPU.r1; ARM.CPU.r2; ARM.CPU.r3]
      | Some "__sprintf_chk" (* not sure, var args *)
      | Some "__vsprintf_chk" -> [ARM.CPU.r0; ARM.CPU.r1; ARM.CPU.r2; ARM.CPU.r3; ARM.CPU.r4] (* actually not sure *)
      | Some "__vsnprintf_chk" -> [ARM.CPU.r0; ARM.CPU.r1; ARM.CPU.r2; ARM.CPU.r3; ARM.CPU.r4; ARM.CPU.r5] (* var args *)
      | Some _
      | None -> [] in
    List.map vars ~f:(fun r -> None, Bil.var r)
  method! records = [[]]
end

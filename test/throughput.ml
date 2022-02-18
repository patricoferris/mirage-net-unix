(*
 * Copyright (C) 2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf

let run test =
  Eio_linux.run @@ fun _ ->
  Eio.Std.Switch.run @@ fun sw -> test ~sw ()

let test_open_close ~sw () =
  let t = Netif.connect ~sw "tap0" in
  printf "tap0: connected\n%!";
  Netif.disconnect t;
  printf "tap0: disconnected\n%!"

let unwrap_result = function
  | Ok v -> v
  | Error trace -> Fmt.pr "%a" Error.pp_trace trace

let sz = 1_000_000_000
let n = 1

let test_write ~sw () =
  let t = Netif.connect ~sw "tap0" in
  let mtu = Netif.mtu t in
  let t0 = Unix.gettimeofday () in

  Eio.Std.Fibre.all
    (List.init n (fun _ () ->
         for _ = 0 to sz / n / mtu do
           Netif.write t ~size:mtu (fun data ->
            for i = 0 to ((mtu - 1) / 8) do 
            Cstruct.LE.set_uint64 data (2*i) (Int64.of_int i)
            done;
            mtu) |> unwrap_result
         done));

  Printf.printf "Wrote 1G in %.2fs" (Unix.gettimeofday () -. t0)

let _ = run test_write


(* 

single cstruct copied into uring : parallelism of 32 = Wrote 1G in 0.56s


*)
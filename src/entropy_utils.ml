(* AIFAD - Automated Induction of Functions over Algebraic Datatypes

   Copyright © 2002 Austrian Research Institute for Artificial Intelligence
   Copyright © 2003- Markus Mottl <markus.mottl@gmail.com>

   This library is free software; you can redistribute it and/or modify it under
   the terms of the GNU Lesser General Public License as published by the Free
   Software Foundation; either version 2.1 of the License, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
   details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA *)

let log_2 = log 2.0
let log2 n = log n /. log_2
let icont p = -.log2 p
let calc_probs_icont ps = Array.map icont ps

(* Frequency-based multinomial estimation *)

module Freq = struct
  let calc_entropy histo n =
    if n = 0 then 0.0
    else
      let rec loop sum ix =
        if ix < 0 then sum
        else
          let freq = histo.(ix) in
          if freq = 0 then loop sum (ix - 1)
          else
            let ffreq = float freq in
            loop (sum +. (ffreq *. log ffreq)) (ix - 1)
      in
      let sum = loop 0.0 (Array.length histo - 1) in
      let f_n = float n in
      log2 f_n -. (sum /. f_n /. log_2)

  let calc_prob histo cnstr n = float histo.(cnstr) /. float n

  let calc_probs histo n =
    let f_n = float n in
    let cnv freq = float freq /. f_n in
    Array.map cnv histo

  let calc_unique_prob _ _ = 1.0

  let calc_unique_entropy n_cnstrs freq =
    -.log2 (calc_unique_prob n_cnstrs freq)
end

(* Ristad's Natural Law of Succession *)

module Ristad = struct
  let histo_coll_count_full cnt freq = if freq = 0 then cnt else cnt + 1
  let histo_count_full histo = Array.fold_left histo_coll_count_full 0 histo
  let laplace_prob n k ni = float (ni + 1) /. float (n + k)

  let calc_prob histo cnstr n =
    let k = Array.length histo in
    let q = histo_count_full histo in
    let ni = histo.(cnstr) in
    if q = k then laplace_prob n k ni
    else
      let f_n = float n in
      let t = (f_n *. f_n) +. float (n + q + q) in
      if ni = 0 then float (q * (q + 1)) /. float (k - q) /. t
      else float (ni + 1) *. float (n + 1 - q) /. t

  let calc_probs histo n =
    let k = Array.length histo in
    let q = histo_count_full histo in
    if q = k then Array.map (laplace_prob n k) histo
    else
      let f_n = float n in
      let tl = (f_n *. f_n) +. float (n + q + q) in
      let t = float (n + 1 - q) /. tl in
      let zero_prob = float (q * (q + 1)) /. float (k - q) /. tl in
      let cnv ni = if ni = 0 then zero_prob else float (ni + 1) *. t in
      Array.map cnv histo

  let calc_entropy histo n =
    if n = 0 then 0.0
    else
      let k = Array.length histo in
      let q = histo_count_full histo in
      let q_n = q + n in
      if k = q then
        let rec loop acc ix =
          let ffreq_1 = float (histo.(ix) + 1) in
          let new_acc = acc +. (ffreq_1 *. log2 ffreq_1) in
          if ix = 0 then new_acc else loop new_acc (ix - 1)
        in
        let n_k = float (n + k) in
        ((float q_n *. log2 n_k) -. loop 0.0 (k - 1)) /. n_k
      else
        let rec loop acc ix =
          let freq = histo.(ix) in
          if freq = 0 then if ix = 0 then acc else loop acc (ix - 1)
          else
            let ffreq_1 = float (freq + 1) in
            let new_acc = acc +. (ffreq_1 *. log2 ffreq_1) in
            if ix = 0 then new_acc else loop new_acc (ix - 1)
        in
        let f_n = float n in
        let tl = (f_n *. f_n) +. float (q_n + q) in
        let qq1 = float (q * (q + 1)) in
        let ni0 = qq1 *. log2 (qq1 /. float (k - q) /. tl) in
        let tu = float (n + 1 - q) in
        -.(ni0 +. (tu *. loop (float q_n *. log2 (tu /. tl)) (k - 1))) /. tl

  let calc_unique_prob n_cnstrs n =
    if n_cnstrs = 1 then 1.0
    else
      let f_n = float n in
      let f_n2 = f_n *. f_n in
      let f_n2_n = f_n2 +. f_n in
      f_n2_n /. (f_n2_n +. 2.0)

  let calc_unique_entropy n_cnstrs freq =
    -.log2 (calc_unique_prob n_cnstrs freq)
end

include Ristad

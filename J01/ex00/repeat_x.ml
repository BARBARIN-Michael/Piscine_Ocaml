(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mbarbari <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 10:26:05 by mbarbari          #+#    #+#             *)
(*   Updated: 2015/11/03 10:42:43 by mbarbari         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_x n =
    let rec repstr n str =
    if (n >= 0) then
        repstr (n - 1) (str ^ "x")
    else
        str
    in
        repstr (n - 1) ""


let () =
    print_endline (repeat_x (5));
    print_endline (repeat_x (15))

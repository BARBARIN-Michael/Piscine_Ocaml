(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_string.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mbarbari <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 10:45:27 by mbarbari          #+#    #+#             *)
(*   Updated: 2015/11/03 12:14:19 by mbarbari         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_string ?(str="x") n =
    let rec rep stri len =
        if (n < 0) then
             "Error"
        else if (len <> 0) then
            rep (str^stri) (len - 1)
        else
            stri
    in
        rep str (n - 1)

let () =
    print_endline (repeat_string ~str:"what" 5)

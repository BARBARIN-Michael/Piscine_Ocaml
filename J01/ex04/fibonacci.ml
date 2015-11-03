(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fibonacci.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mbarbari <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 13:17:09 by mbarbari          #+#    #+#             *)
(*   Updated: 2015/11/03 17:54:20 by barbare          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let fibonacci n =
    let rec save vp vi len =
        if (len < 0) then
            (-1)
        else if (len = 0) then
            vp 
        else
            save vi (vi + vp) (len - 1)
    in
        save 0 1 n

let () =
    print_int (fibonacci (-1));
    print_char '\n';
    print_int (fibonacci 1);
    print_char '\n';
    print_int (fibonacci 3);
    print_char '\n';
    print_int (fibonacci 6);
    print_char '\n'

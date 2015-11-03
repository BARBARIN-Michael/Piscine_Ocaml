(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: barbare <barbare@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 15:58:49 by barbare           #+#    #+#             *)
(*   Updated: 2015/11/03 17:53:25 by barbare          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec converges f x n =
    if (n <= 0) then
        f x = x
    else if (f x = x) then
       true 
    else
        converges f (f x) (n - 1)

let () =
    print_endline (string_of_bool(converges (( * ) 2) 2 5));
    print_endline (string_of_bool(converges (fun x -> x / 2) 2 3));
    print_endline (string_of_bool(converges (fun x -> x / 2) 2 2))


(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: barbare <barbare@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 16:55:20 by barbare           #+#    #+#             *)
(*   Updated: 2015/11/03 18:48:37 by barbare          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f a b =
    let rec sum acc cmp =
        if (cmp > b) then
            acc
        else 
            sum (f cmp +. acc) (cmp + 1)
    in
        sum 0.0 a

let () =
    print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10)

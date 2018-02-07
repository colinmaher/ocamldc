(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let reverse   = List.rev
    let map       = List.map
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let trimzeros list =
        let rec trimzeros' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                    let cdr' = trimzeros' cdr
                    in  match car, cdr' with
                        | 0, [] -> []
                        | car, cdr' -> car::cdr'
        in trimzeros' list
    
    let rec cmp' (l1 : int list) (l2 : int list) = 
        if List.length l1 < List.length l2 then -1
        else if List.length l1 > List.length l2 then 1
        else match (l1, l2) with
            | [], [] -> 0
            | [], l2 -> -1
            | l1, [] -> 1
            | l1, l2 ->
                let rev1 = reverse l1 in
                let rev2 = reverse l2 in
                if car rev1 < car rev2 then -1
                else if car rev1 > car rev2 then 1
                (* revert tails to original order for recursion *)
                else cmp' (reverse (cdr rev1)) (reverse (cdr rev2)) 

    
    let rec add' (list1 : int list) (list2 : int list) (carry : int) = 
        match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            let sum = car1 + car2 + carry
            in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' (list1 : int list) (list2 : int list) (carry : int) = 
        match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, _       -> list2
        | list1, [], carry   -> 
            let dif = (car list1) - carry in
            let cdr1 = cdr list1 in
            if dif >= 0  then dif::cdr1
            else (dif + 10) :: sub' cdr1 [] 1
        | car1::cdr1, car2::cdr2, carry ->
            if carry = 1 then 
                sub' ((car1 - carry)::cdr1) list2 0
            else
                let diff = car1 - car2 in
                if diff >= 0 then diff::(sub' cdr1 cdr2 0) 
                else (diff + 10) :: sub' cdr1 cdr2 1

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let cmp = cmp' value1 value2 in
        if neg1 = neg2 then 
            Bigint (neg1, add' value1 value2 0)
        else if cmp > 0 then 
            Bigint (neg1, (sub' value1 value2 0))
        else if cmp < 0 then 
            Bigint (neg2, (sub' value2 value1 0))
        else zero


    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let cmp = cmp' value1 value2 in
        if neg1 = neg2 then 
            if neg1 = Pos then
                if cmp > 0 then 
                    Bigint (Pos, trimzeros (sub' value1 value2 0))
                else if cmp < 0 then 
                    Bigint (Neg, trimzeros (sub' value2 value1 0))
                else zero
            else 
                if cmp > 0 then 
                    Bigint (Neg, trimzeros (sub' value1 value2 0))
                else if cmp < 0 then 
                    Bigint (Pos, trimzeros (sub' value2 value1 0))
                else zero
        else Bigint (neg1, add' value1 value2 0)

    let double number = add number number

    let rec mul' (multiplier, powerof2, multiplicand') =
        if powerof2 > multiplier
        then multiplier, 0
        else let remainder, product =
                mul' (multiplier, double powerof2, double multiplicand')
            in  if remainder < powerof2
                then remainder, product
                else remainder - powerof2, product + multiplicand'

    let rec mul' smaller powerof2 larger =
        if cmp > 0 then smaller, 0
        else let remainder, product = 
                mul' (smaller, (add' powerof2 powerof2 0), (add' larger larger 0))
             in  if remainder < p
        add 

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2 then Bigint (Pos, mul' value1 1 value2)
        else Bigint (Neg, mul' value1 1 value2)

    let div = add

    let rem = add

    let pow = add
        (*
    let even number = number mod 2 = 0

    let rec pow' (base, expt, result) = match expt with
        | 0                   -> result
        | expt when even expt -> pow' (base *. base, expt / 2, result)
        | expt                -> pow' (base, expt - 1, base *. result)

    let pow (base, expt) =
        if expt < 0 then pow' (1. /. base, - expt, 1.)
                    else pow' (base, expt, 1.)

                    *)

end


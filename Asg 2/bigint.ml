(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
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

    let concat_list list1 = 
        float_of_string (String.concat "" 
            (List.rev_map string_of_int list1))

    let trimzeros list =
        let rec trimzeros' list' = match list' with 
            | []       -> []
            | [0]      -> []
            | car::cdr -> 
                let cdr' = trimzeros' cdr in match car,cdr' with 
                    | 0, []  -> []
                    | car, cdr' -> car::cdr' in trimzeros' list

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 borrow = match (list1, list2, borrow) with
        | list1, [], 0        -> list1
        | [], list2, 0        -> []
        | list1, [], borrow   -> sub' list1 [borrow] 0
        | [], list2, borrow   -> []
        | car1::cdr1, car2::cdr2, borrow ->
          let diff = car1 - car2 - borrow
          in if diff < 0
          then car1 + radix - car2 - borrow :: sub' cdr1 cdr2 1
          else car1 - car2 - borrow :: sub' cdr1 cdr2 0

    let double number = add' number number 0

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else if concat_list value1 > concat_list value2
           then Bigint(neg1, sub' value1 value2 0)
           else Bigint(neg2, sub' value2 value1 0)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match neg1, neg2 with
        | Pos, Pos    ->
            if concat_list value1 > concat_list value2 
            then Bigint(Pos, sub' value1 value2 0)
            else Bigint(Neg, trimzeros (sub' value2 value1 0) )
        | Neg, Neg    -> 
            if concat_list value1 > concat_list value2
            then Bigint(Neg, sub' value1 value2 0)
            else Bigint(Pos, sub' value2 value1 0)
        | Pos, Neg    -> Bigint (Pos, add' value1 value2 0)
        | Neg, Pos    -> Bigint (Neg, add' value1 value2 0)


    let rec  mul' list1 power list2 = 
        if concat_list power > concat_list list1
        then list1, [0]
        else let remainder, product =
            mul' list1 (double power) (double list2)
        in if concat_list remainder < concat_list power
            then remainder, product
            else (sub' remainder power 0), (add' product list2 0)
        
    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let _, product = 
            mul' value1 [1] value2 in Bigint(Pos, product)
        else let _, product = 
            mul' value1 [1] value2 in Bigint(Neg, product)

    let rec divrem' list1 power list2 =
        if concat_list list2 > concat_list list1
        then [0], list1
        else let quotient, remainder =
            divrem' list1 (double power) (double list2) in
            if concat_list remainder < concat_list list2
            then quotient, remainder
            else (add' quotient power 0), (sub' remainder list2 0)

    let divrem list1 list2 = divrem' list1 [1] list2

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let quotient, _ = 
            divrem value1 value2 in Bigint(Pos, quotient)
        else let quotient, _ = 
            divrem value1 value2 in Bigint(Neg, quotient)

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let _, remainder = 
            divrem value1 value2 in Bigint(Pos, remainder)
        else let _, remainder = 
            divrem value1 value2 in Bigint(Neg, remainder)

    let even number = (let result = 
        mod_float (concat_list number) 2.0 in result = 0.0)

    let rec pow' base expt result = match expt with
    | [0] -> result
    | expt -> if (even expt)
              then pow' (let _,p = mul' base [1] base in p) 
                   (let q,_ = divrem' expt [1] [2] in q) result
              else pow' base (sub' expt [1] 0) (let _,p = 
                   mul' base [1] result in p)

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg2 = Neg
        then Bigint (Pos, (let result,_ = divrem [1]
            (pow' value1 value2 [1]) in result))
        else Bigint (Pos, pow' value1 value2 [1])

end


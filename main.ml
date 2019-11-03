(*
    Domande:
    E' possibile fare una funzione che visiti un albero binario, tale che sia tail recursive?
    E' possibile una versione di nth che sia tail recursive?
*)


(* Es. 1.a *)

(* 
    length: 'a list -> int
    length lst = numero di elementi presenti in lst.
*)
let rec length = function
    [] -> 0
    | a::l -> 1 + (length l);;


let rec length_tr lst =
    (* aux: int -> 'a list -> int
        aux k lst = k + (length lst) 
    *)
    let rec aux result = function
        [] -> result
        | a::l -> aux (result + 1) l
    in aux 0 lst;;


(* --------------------------------------- *)


(* Es. 1.b *)


(* sumof: int list -> int 
   sumof [x1; ....; xn] = x1 + .... + xn
*)
let rec sumof = function
    [] -> 0
    | a::l -> a + sumof l;;


let sumof_tr lst = 
    (*
        aux: int -> int list -> int
        aux k [x1; ....; xn] = k + x1 + ... + xn
    *)
    let rec aux result = function
        [] -> result
        | a::l -> aux (a + result) l
    in aux 0 lst;;


(* --------------------------------------- *)


(* Es. 1.c *)


exception ListaVuota;;


(* 
    maxlist: 'a list -> 'a 
    maxlist [x1; ...; xn] = max (x1, ..., xn)
    La funzione lancia l'eccezione ListaVuota
    se la lista non contiene elementi.
*)
let rec maxlist = function
    [] -> raise ListaVuota
    | a::l -> 
        let m = try maxlist l with
            ListaVuota -> a
        in if a > m
            then a
            else m;;


let maxlist_tr = function
    [] -> raise ListaVuota
    | a::l -> 
        (*
            aux: int -> int list -> int
            aux k [x1; ...; xn] = max(k, x1, ..., xn)
        *)
        let rec aux result = function
            [] -> result
            | a::l -> 
                if a > result 
                then aux a l
                else aux result l
        in aux a l;;


(* --------------------------------------- *)


(* Es. 1.d *)


(*
    drop: int -> 'a list -> 'a list
    drop k [x1; ...; xn] = [xk; ...; xn]
*)
let rec drop k = function
    [] -> []
    | a::l -> 
        match k with
        0 -> a::(drop k l)
        | _ -> drop (k - 1) l;;


let drop_tr k lst =
    (*
        aux: int -> 'a list -> 'a list -> 'a list
        aux k [x1; ...; xn] [y1; ...; ym] = [ym; ... ; yk; x1; ... ; xn]
    *)
    let rec aux k result = function
        [] -> result
        | a::l ->
            match k with
            0 -> aux k (a::result) l
            | _ -> aux (k-1) result l
    in aux k [] lst;;


(* --------------------------------------- *)


(* Es. 1.e *)


(*
    append: 'a list -> 'a list -> 'a list
    append [x1; ... ; xn] [y1; ... ; ym] = [x1; ... ; xn; y1; ... ; ym]
*)
let append l1 l2 =
    (*
        aux: 'a list -> 'a list -> 'a list
        aux [y1; ... ; ym] [x1; ... ; xn] = [xn; ...; x1; y1; ... ; ym]
    *)
    let rec aux result = function
        [] -> result
        | a::l -> aux (a::result) l
    in aux l2 (List.rev l1);;


(* --------------------------------------- *)


(* Es. 1.f *)

(*
    reverse: 'a list -> 'a list
    reverse [x1, ... , xn] = [xn, ..., x1]
*)
let reverse lst =
    (*
        aux: 'a list -> 'a list
        aux [x1, ..., xn] [y1, ... , ym] = [ym, ..., y1, x1, ..., ym]
    *)
    let rec aux result = function
        [] -> result
        | a::l -> aux (a::result) l
    in aux [] lst;;


(* --------------------------------------- *)


(* Es. 1.g *)


exception OutOfBounds;;


(*
    nth: int -> 'a list -> 'a
    nth k [x0, ..., x(n-1)] = xk se n > k + 1, altrimenti viene lanciata l'eccezione OutOfBounds.
*)
let rec nth n = function
    [] -> raise OutOfBounds
    | a::l ->
        if n = 0 
        then a
        else if n < 0
            then raise OutOfBounds
            else nth (n-1) l;;


(* --------------------------------------- *)


(* Es. 1.h *)


(*
    remove: 'a -> 'a list -> 'a list
    remove k list = lista contenente tutti gli elementi di list tranne quelli uguali a k.
*)
let rec remove x = function
    [] -> []
    | a::l ->
        if a = x
        then remove x l
        else a::(remove x l);;


let remove_tr x lst = 
    (*
        aux: 'a list -> 'a -> 'a list -> 'a list
        aux lst k lst2 = lista contenente tutti gli elementi di lst e gli elementi di lst2 diversi da k.
    *)
    let rec aux result x = function
        [] -> result
        | a::l -> 
            if a = x
            then aux result x l
            else aux (a::result) x l
    in aux [] x lst;;


(* --------------------------------------- *)


(* Es. 2.a *)


(*
    copy: int -> 'a -> 'a list
    copy n x = [y1, ..., yn] dove yi = x per ogni i in [0, n]
*)
let rec copy n x = 
    if n = 0
    then []
    else x::(copy (n-1) x);;


let copy_tr n x =
    (*
        aux: 'a list -> 'a -> int -> 'a list
        aux l1 x n = (copy n x)::l1
    *)
    let rec aux result x = function
        0 -> result
        | n -> aux (x::result) x (n-1)
    in aux [] x n;; 


(* --------------------------------------- *)


(* Es. 2.b *)


(* 
    nondec: 'a list -> bool
    Ritorna true se gli elementi nella lista sono in ordine non decrescente.
*)
let rec nondec = function
    [] -> true
    | [x] -> true
    | a::b::l -> (a <= b) && (nondec (b::l) );;


let nondec_tr lst =
    (*
        aux: bool -> 'a lst -> bool
        aux c lst = c && (nondec lst)
    *)
    let rec aux result = function
        [] -> result
        | [x] -> result
        | a::b::l -> aux ((a <= b) && result) (b::l)
    in aux true lst;;


(* --------------------------------------- *)


(* Es. 2.c *)


(*
    pairwith -> 'a -> 'b list -> ('a * 'b) list
    pairwith k [x1, ..., xn] = [(k, x1), ..., (k, xn)]
*)
let rec pairwith y = function
    [] -> []
    | a::l -> (y, a)::(pairwith y l);;


let pairwith_tr y lst =
    (*
        aux: ('a * 'b) list -> 'a -> 'b list -> ('a * 'b) list
        aux [(y1, z1), ..., (ym, zm)] k [x1, ..., xn] = [(k, xn), ..., (k, x1), (y1, z1), ..., (ym, zm)]
    *)
    let rec aux result y = function
        [] -> result
        | a::l -> aux ((y, a)::result) y l
    in aux [] y (List.rev lst);;


(* --------------------------------------- *)


(* Es. 2.d *)


(*
    duplica: 'a list -> 'a list
    duplica [x1, x2, ..., xn] = [x1,x1,x2,x2,x3,x3,...,xn,xn]
*)
let rec duplica = function
    [] -> []
    | a::l -> a::a::(duplica l);;


let duplica_tr lst =
    (*
        aux: 'a list -> 'a list -> 'a list
        aux [x1, ..., xn] [y1, ..., ym] = [ym, ym, ..., y1, y1, x1, ..., xm]
    *)
    let rec aux result = function
        [] -> result
        | a::l -> aux (a::a::result) l
    in aux [] (List.rev lst);;


(* --------------------------------------- *)


(* Es. 2.e *)

(*
    enumera: 'a list -> (int * 'a) list
    enumera [x0, ..., xk] = [(0, x0), ... , (k, xk)]
*)
let enumera lst =
    (*
        aux: int -> 'a list -> (int * 'a) list
        aux i [x0, x1, ..., xk] = [(i, x0), (i+1, x1), ..., (i+k, xk)]
    *)
    let rec aux i = function
        [] -> []
        | a::l -> (i, a)::(aux (i+1) l)
    in aux 0 lst;;


let enumera_tr lst =
    (*
        aux: (int * 'a) list -> int -> 'a list -> (int * 'a) list
        aux [c1, ..., cn] k [x0, ..., xm] = [(m+k, xm), (m+k-1, xm-1), ..., (k, x0), c1, ..., cn] 
    *)
    let rec aux result i = function
        [] -> result
        | a::l -> aux ((i, a)::result) (i+1) l
    in List.rev(aux [] 0 lst);;


(* --------------------------------------- *)

(* Es. 2.f *)


exception NotFound;;


(*
    position: 'a -> 'a list -> int
    position k lst = posizione (partendo da 0) della prima occorrenza di k in lst.
    Se k non è presente in lst, viene lanciata l'eccezione NotFound.
*)
let position x lst =
    (*
        aux: int -> 'a -> 'a list -> int
        aux i k lst = i + (indice della prima occorrenza di k in lst)
        Se k non è presente in lst, viene sollevata l'eccezione NotFound.
    *)
    let rec aux index x = function
        [] -> raise NotFound
        | a::l -> 
            if a = x
            then index
            else aux (index + 1) x l
    in aux 0 x lst;;



    
```haskell
1.
a. let x : Nat = 2 in succ x 
<=> 
let (x : Nat) = 2 in succ x

b. fun (x:Nat) -> x (No se toca)

c. let id (x:Nat) : Nat = x in id 10 
<=> 
let id : Nat -> Nat = fun (x: Nat) -> x in id 10

d. 
let app5 (f : Nat -> Nat) : Nat = f 5 in app5 succ 
<=> 
let app5 (Nat -> Nat) -> Nat = fun (f: Nat -> Nat) -> f 5 in app5 succ


e. fun (x:Nat) (y:Nat) -> ifz x then y else 1 
<=> 
fun (x: Nat) -> fun (y: Nat) -> ifz x then y else 1


2. 
a) 
let rec doble (x:Nat) : Nat = ifz x then 0 else succ (succ (doble (pred x)))

<=>

let doble: Nat -> Nat = 
    fix (doble: Nat -> Nat) (x: Nat) ->
         ifz x 
            then 0 
            else succ (succ (doble (pred x)))

b) 
let rec ack (m:Nat) (n:Nat) : Nat =
ifz m
    then succ n
    else (ifz n
        then ack (pred m) 1
        else ack (pred m) (ack m (pred n)))

<=>

let rec ack (m: Nat) : Nat -> Nat = fun (n: Nat) -> ifz m
    then succ n
    else (ifz n
        then ack (pred m) 1
        else ack (pred m) (ack m (pred n)))

<=>
let ack: Nat -> (Nat -> Nat) = fix (ack: Nat -> (Nat -> Nat)) (m: Nat) ->
    fun (n: Nat) -> ifz m
        then succ n
        else (ifz n
            then ack (pred m) 1
            else ack (pred m) (ack m (pred n)))

```
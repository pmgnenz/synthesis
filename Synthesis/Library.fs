module Synthesis

let abelar v = 
   // failwith "Not implemented"
    v > 12 && v < 3097 && (v % 12) =0
    

let area m n =
    match ((m<0.0)||(n<0.0)) with 
    |true -> failwith "Negative number"
    |_ -> m*n/2.0
    

let zollo m =
    match (m<0) with 
    |true -> m*(-1)
    |_ -> m*2
   

let min m n =
   match (m<n) with 
    |true -> m
    |_ -> n

let max m n =
    match (m>n) with 
    |true -> m
    |_ -> n

let ofTime m n o =
   // failwith "Not implemented"
   (m*3600)+(n*60)+o

let toTime m =
    let rec minsec (a,b) =
        match b%60 =0 with
        |true -> (a,b/60, 0)
        |_ -> (a,b/60,b%60)
    match  m<0 with
    |true -> (0,0,0)
    |_ -> minsec (m/3600,m%3600)

    

let digits m =
    let rec thedigit a =
        match a <10 && a > -10  with
        |true ->1
        |_ -> 1+ thedigit(a/10) 
    thedigit m
   


let minmax (a,b,c,d) =
    let j,k,l,n = (min a b),(min c d),(max a b),(max c d)
    match j,k,l,n with
    |_ -> min j k,max l n
  
let isLeap m =
    match (m < 1582)  with
    |true -> failwith "less than 1582"
    |_ -> match ((m%4=0)&&(m%100 <>0)) || m%400 =0 with
          |true -> true
          |false ->false
   
let month n = 
    match n with
    |1 -> "January", 31
    |2 -> "February", 28
    |3 -> "March", 31
    |4 -> "April", 30
    |5 -> "May", 31
    |6 -> "June", 30
    |7 -> "July", 31
    |8 -> "August", 31
    |9 -> "September", 30
    |10 -> "October", 31
    |11 -> "November", 30
    |12 -> "December", 31
    |_ -> failwith"not a month"
   
   
let toBinary m =
    let rec intBinary i =
       match i with
       | 0 | 1 -> string i
       | _ ->
            let bit = string (i % 2)
            (intBinary (i / 2)) + bit
    match (m<0) with 
    |true -> failwith "negative number"
    |_ -> intBinary m

let bizFuzz n =
    let rec number1 m o u l = 
        match m<=n with
        |false -> o
        |_ -> match (m%u = 0) && (m%l = 0)  with
              |false -> number1 (m+1) o u l
              |_ -> number1 (m+1) (o+1) u l
    let y,p,a = (number1 1 0 3 1),(number1 1 0 5 1),(number1 1 0 3 5)
    match y,p,a with
    |_-> y,p,a
    

let monthDay d y =
    let rec calc a v u =
        match a<=d with
        |true -> match a with
                 |1 -> calc (a+1) "January" u
                 |32 -> calc (a+1+u) "February" u
                 |61 -> calc (a+1) "March" u
                 |92-> calc (a+1) "April" u
                 |122 -> calc (a+1) "May" u
                 |153 -> calc (a+1) "June" u
                 |183 -> calc (a+1) "July" u
                 |214-> calc (a+1) "August" u
                 |245-> calc (a+1) "September" u
                 |275 -> calc (a+1) "October" u
                 |306 -> calc (a+1) "November" u
                 |336 -> calc (a+1) "December" u
                 |_ -> calc (a+1) v u
        |_ -> v
    match (y>=1582 && (d>=1 && d<=366)) with
    |true -> match ((y%400=0)||((y%100 <>0)&&(y%4=0))) with
          |true -> calc 1 "" 0
          |_ -> match d > 365 with 
                |true -> failwith "error"
                |_ -> calc 1 "" 1
    |_ -> failwith "error"
   
           
    
let coord _ =
    failwith "Not implemented"
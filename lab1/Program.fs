open System    

// возведение числа а в степень b
let rec pow a b =
    if b < 1 then 1.0
    else float a * pow a (b-1)

// факториал числа n
let rec fact n =
    if n <= 1 then 1.0
    else float n * fact(n-1)

// вычисление n-ного члена суммы
let chlenSumy x n =
    pow x n / fact n

// вычисление суммы из n элементов с точностью eps
let rec sum x n eps =
    //let curr = fun x n -> ( pow x n / fact n )
    let curr = chlenSumy x n
    if curr >= eps then curr + sum x (n+1) eps
    else curr

// запуск вычисления суммы
let mysum x eps =
    sum x 0 eps

// использование функции вычисления суммы
[<EntryPoint>]
let main argv =
    printf "e^5 = %f" (mysum 5.0 0.0001)
    0
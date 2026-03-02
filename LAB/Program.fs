open System

// Функция для выполнения задания 1
let task1 () =
    printfn "Введите числа через пробел:"
    let input = Console.ReadLine()
    
    let stringArray = input.Split(' ')
    
    let numbers = 
        [ for s in stringArray do
            if s <> "" then
                yield int s ]
    
    let incrementedNumbers = List.map (fun x -> x + 1) numbers
    
    printfn "Введённые значения: %A" numbers
    printfn "Список введённых значений + 1: %A" incrementedNumbers


// Рекурсивная функция для нахождения произведения нечётных цифр
let rec productOfOddDigits number product =
    if number = 0 then
        product
    else
        // Берем последнюю цифру (остаток от деления на 10)
        let lastDigit = number % 10
        
        // Проверяем, нечётная ли цифра
        if lastDigit % 2 <> 0 then
            // Если нечётная - умножаем на текущее произведение
            productOfOddDigits (number / 10) (product * lastDigit)
        else
            // Если чётная - просто переходим к следующим цифрам
            productOfOddDigits (number / 10) product


let task2 () =
    printfn "Введите натуральное число:"
    let input = Console.ReadLine()
    
    // Просто преобразуем строку в число (без проверок)
    let number = int input
    
    if number <= 0 then
        printfn "Ошибка: число должно быть натуральным (положительным)"
    else
        // Вызываем рекурсивную функцию с начальным произведением 1
        let result = productOfOddDigits number 1
        
        if result = 1 then
            printfn "В числе %d нет нечётных цифр" number
        else
            printfn "Число: %d" number
            printfn "Произведение нечётных цифр: %d" result



// Добавление числа в конец списка
let addNumber number list = list @ [number]

// Удаление числа из списка
let removeNumber number list =
    [ for x in list do
        if x <> number then
            yield x ]

// Поиск числа в списке
let findNumber number list =
    let rec search remainingList =
        if remainingList = [] then
            false
        elif remainingList.Head = number then
            true
        else
            search remainingList.Tail
    search list

// Получение числа по индексу
let getNumberByIndex index list =
    let rec get remainingList currentIndex =
        if remainingList = [] then
            -1
        elif currentIndex = index then
            remainingList.Head
        else
            get remainingList.Tail (currentIndex + 1)
    get list 0

// Соединение двух списков
let joinLists list1 list2 = list1 @ list2

// Ввод списка чисел
let readList prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    let parts = input.Split(' ')
    [ for s in parts do
        if s <> "" then
            yield int s ]

// Ввод числа
let readNumber prompt =
    printf "%s" prompt
    int (Console.ReadLine())

// Меню программы
let rec menu currentList =
    printfn "\n-----------------------------"
    printfn "1 - Добавить число"
    printfn "2 - Удалить число"
    printfn "3 - Найти число"
    printfn "4 - Получить число по индексу"
    printfn "5 - Соединить с другим списком"
    printfn "6 - Показать список"
    printfn "0 - Выход"
    printf "Выберите действие: "
    
    let choice = Console.ReadLine()
    printfn ""
    
    if choice = "0" then
        printfn "До свидания!"
        currentList
        
    elif choice = "1" then
        let x = readNumber "Введите число: "
        let newList = addNumber x currentList
        printfn "Число %d добавлено" x
        printfn "Список: %A" newList
        menu newList
        
    elif choice = "2" then
        let x = readNumber "Введите число для удаления: "
        let newList = removeNumber x currentList
        if List.length newList < List.length currentList then
            printfn "Число %d удалено" x
        else
            printfn "Число %d не найдено" x
        printfn "Список: %A" newList
        menu newList
        
    elif choice = "3" then
        let x = readNumber "Введите число для поиска: "
        if findNumber x currentList then
            printfn "Число %d есть в списке" x
        else
            printfn "Числа %d нет в списке" x
        menu currentList
        
    elif choice = "4" then
        let i = readNumber "Введите индекс: "
        let value = getNumberByIndex i currentList
        if value = -1 then
            printfn "Нет элемента с индексом %d" i
        else
            printfn "Элемент[%d] = %d" i value
        menu currentList
        
    elif choice = "5" then
        let list2 = readList "Введите второй список через пробел: "
        let newList = joinLists currentList list2
        printfn "Списки соединены"
        printfn "Результат: %A" newList
        menu newList
        
    elif choice = "6" then
        printfn "Текущий список: %A" currentList
        menu currentList
        
    else
        printfn "Неправильный ввод, попробуйте еще раз"
        menu currentList

// Задание 3
let task3 () =
    
    let initialList = readList "Введите числа через пробел: "
    printfn "Начальный список: %A" initialList
    
    menu initialList


[<EntryPoint>]
let main args =
    printfn "Задание 1"
    printfn"Сформировать список из чисел, на 1 больших, чем вводимые значения"
    task1()  // Вызов функции задания 1
    printfn "Задание 2"
    printfn"Найти произведение нечётных цифр натурального числа."
    task2()
    printfn "Задание 3"
    task3()

    0
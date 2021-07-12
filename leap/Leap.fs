module Leap

let leapYear (year: int): bool = 
    let divisibleBy4 = year % 4 = 0
    let notDivisibleBy100 = year % 100 <> 0
    let divisibleBy400 = year % 400 = 0
    if (divisibleBy4 && (notDivisibleBy100 || divisibleBy400)) then true
    else false
module online
open System

let message (logLine: string): string = logLine.Split([| ':' |]).[1].Trim()

let logLevel(logLine: string): string = logLine.Split([| '['; ']' |]).[1].Trim().ToLower()

let reformat(logLine: string): string = 
    let message = message logLine
    let logLevel = logLevel logLine
    sprintf "%s (%s)" message logLevel

reformat "[INFO]: Operation completed"

let twoFer (input: string option): string = 
    sprintf "One for %s, one for me." <|
        match input with
        |Some name -> name
        |None -> "you"

type Approval =
    |Yes 
    |No
    |Maybe

type Cuisine =
    |Korean
    |Turkish

type Genre =
    |Crime 
    |Horror
    |Romance
    |Thriller

type Activity =
    |BoardGame
    |Chill
    |Movie of Genre
    |Restaurant of Cuisine
    |Walk of int

let rateActivity activity = 
    match activity with 
    |BoardGame -> No
    |Chill -> No
    |Movie genre -> if genre = Romance then Yes else No
    |Restaurant cuisine -> if cuisine = Korean then Yes else Maybe
    |Walk length -> if length < 3 then Yes elif length < 5 then Maybe else No

let succesRate speed : float = if speed = 0 then 0.0 elif speed > 0 && speed < 5 then 100.0 elif speed > 4 && speed < 9 then 0.9 elif speed = 9 then 0.8 else 0.77

let productionRatePerHour speed : float = succesRate speed * (float speed) * 221.0

let workingItemsPerMinute speed : int =  int (productionRatePerHour(speed)/60.0)
workingItemsPerMinute 6

let response (input: string): string =
    match input with
    |input when input.EndsWith("?") -> "Sure."
    |input when input |> String.forall Char.IsUpper = true -> "Whoa, chill out!"
    |input when input.[input.Length - 1] = '!' && input.[input.Length - 2] = '?' -> "Calm down, I know what I'm doing!"
    |input when input.Length = 0 -> "Fine. Be that way!"
    |_ -> "Whatever."

let leapYear (year: int): bool =
    let divisibleBy x = (year % x) = 0
    divisibleBy 400 || divisibleBy 100 <> divisibleBy 4

leapYear 2015

let reply guess =
    match guess with
    |42 -> "Correct"
    |41|43 -> "So close"
    |x when x < 41 -> "Too low"
    |_ -> "Too high"

reply 0

type Pizza =
    |Margherita
    |Caprese 
    |Formaggio
    |ExtraSauce of Pizza
    |ExtraToppings of Pizza

let rec pizzaPrice pizza =
    match pizza with
    |Margherita -> 7
    |Caprese -> 9
    |Formaggio -> 10

let newList: string list = []

let existingList: string list = ["F#"; "Clojure"; "Haskell"]

let addLanguage (language: string) (languages: string list): string list = language::languages

let countLanguages (languages: string list): int = List.length languages

let reverseList(languages: string list): string list = List.rev languages

let excitingList (languages: string list): bool = 
    match languages with
    |"F#"::_ -> true
    |[_;"F#"] -> true
    |[_;"F#";_] -> true
    |_ -> false

let lastWeek: int[] = [|0; 2; 5; 3; 7; 8; 4|]

let yesterday(counts: int[]): int = counts[5]

let total(counts: int[]): int = Array.sum counts

let dayWithoutBirds(counts: int[]): bool = if (Array.contains 0 counts) then true else false

let incrementTodaysCount(counts: int[]): int[] = Array.updateAt 6 (counts[6] + 1) counts

let unusualWeek(counts: int[]): bool = 
    match counts with
    |[|day1;day2;day3;day4;day5;day6;day7|] when day2 = 0 && day4 = 0 && day6 = 0 -> true
    |[|day1;day2;day3;day4;day5;day6;day7|] when day2 = 10 && day4 = 10 && day6 = 10 -> true
    |[|day1;day2;day3;day4;day5;day6;day7|] when day1 = 5 && day3 = 5 && day5 = 5 && day7 = 5 -> true
    |_ -> false

type Coach =
    {   Name : string
        FormerPlayer : bool }

type Stats =
    {   Wins : int
        Losses : int    }

type Team = 
    {   Name : string
        Coach : Coach
        Stats : Stats}

let createCoach (name: string) (formerPlayer: bool): Coach = {Name = name; FormerPlayer = formerPlayer}

let createStats(wins: int) (losses: int): Stats = {Wins = wins; Losses = losses}

let createTeam(name: string) (coach: Coach)(stats: Stats): Team = {Name = name; Coach = coach; Stats = stats }

let replaceCoach(team: Team) (coach: Coach): Team = {Name = team.Name; Coach = coach; Stats = team.Stats}

let isSameTeam(homeTeam: Team) (awayTeam: Team): bool = if homeTeam = awayTeam then true else false

let rootForTeam(team: Team): bool = if team.Coach.Name = "Gregg Popovich" || team.Coach.FormerPlayer || team.Name = "Chicago Bulls" || team.Stats.Wins >= 60 || team.Stats.Wins < team.Stats.Losses then true else false

let getCoordinate (line: string * string): string = snd line

let convertCoordinate (coordinate: string): int * char = (int (string coordinate[0]),coordinate[1])

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool =
    let _,data,_ = ruisData
    if convertCoordinate (snd azarasData) = data then true else false

let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) = 
    let location,_, quadrant = ruisData
    if compareRecords azarasData ruisData then (snd azarasData, location, quadrant, fst azarasData) else ("", "", "", "")

type Direction = North | East | South | West
type Position = int * int

let reverse (input: string): string = input.ToCharArray() |> Array.rev |> System.String

// The following line is needed to use the DateTime type
open System

let schedule (appointmentDateDescription: string): DateTime = DateTime.Parse appointmentDateDescription

let hasPassed (appointmentDate: DateTime): bool = if DateTime.Now > appointmentDate then true else false

let isAfternoonAppointment (appointmentDate: DateTime): bool = if appointmentDate.Hour >= 12 && appointmentDate.Hour < 18 then true else false

let description (appointmentDate: DateTime): string = "You have an appointment on " + appointmentDate.ToString() + "."

let anniversaryDate(): DateTime = DateTime(DateTime.Now.Year,9,15,0,0,0)
let rec convert (number: int): string = 
    let result = (if number % 3 = 0 then "Pling" else "") +
                 (if number % 5 = 0 then "Plang" else "") +
                 (if number % 7 = 0 then "Plong" else "")
    match result with
    |"" -> string number
    |_ -> result

let isPangram (input: string): bool = set ['a'..'z'] - set (input.ToLower()) |> Set.isEmpty

let scores (values: int list): int list = values

let latest (values: int list): int = values[values.Length-1]

let rec personalBest (values: int list): int = List.max values

let personalTopThree (values: int list): int list = values |> List.sortDescending |> List.truncate 3


let isArmstrongNumber number =
    let digits = number.ToString() |> Seq.map int
    let numDigits = Seq.length digits
    let sum = Seq.fold (fun acc digit -> acc + pown digit numDigits) 0 digits
    sum = number

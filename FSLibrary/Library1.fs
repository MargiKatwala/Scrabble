//Margi Katwala
//mkatwa3
module MyLibrary

open System.Collections.Specialized
open System.Runtime.InteropServices

#light


//
// explode a string into a list of characters.
// Example: "cat" -> ['c'; 'a'; 't']
//
let explode(s:string) =
  [ for c in s -> c ]


//
// implode a list L of characters back into a string.
// Example: implode ['c'; 'a'; 't'] -> "cat"
//
let implode L =
  let sb = System.Text.StringBuilder()
  let ignore = List.map (fun c -> sb.Append (c:char)) L
  sb.ToString()


//
// Initialize:
//
// This function is called ONCE at program startup to initialize any
// data structures in the library.  We use this function to input the
// Scrabble dictionary and build a list of legal Scrabble words.
//
let mutable WordList = []

let Initialize folderPath =
  let alphabetical = System.IO.Path.Combine(folderPath, "alphabetical.txt")
  WordList <- [ for line in System.IO.File.ReadAllLines(alphabetical) -> line ]
  printfn "%A" (List.length WordList)


//
// possibleWords:
//
// Finds all Scrabble words in the Scrabble dictionary that can be 
// spelled with the given letters.  The words are returned as a list
// in alphabetical order.
//
// Example:  letters = "tca" returns the list
//   ["act"; "at"; "cat"; "ta"]
//

//
//functions from week 6 lecture slides
let rec contains x L = 
  match L with
  | []     -> false
  | hd::tl when hd = x -> true
  | hd::tl -> contains x tl

//functions from week 6 lecture slides
//emplmenting to see if the word is one of the letters 
let rec equivalent char word = 
  match word with
  | [] -> true
  | hd::tl -> if hd = char 
              then equivalent char tl
              else 
                false

//delete function 
let rec delete char word =
    match word with
    |[]->[]
    |hd::tl ->     
                if hd.Equals(char)
                then tl
                else [hd]@(delete char tl)
    
//Sudo Code given on Piazza
//match word with
//|
//hd::tl-> 
//if hd is on of the letters then 
//pf tl ( delete hd letters)
//else 
//false
let rec PF word letters =
   match explode(word) with
   | [] -> true
   | hd::tl -> 
               let tl2 = implode tl
               if contains hd letters 
               then PF tl2 (delete hd letters)
               else 
               false
//possible words function
let possibleWords letters = 
    List.filter(fun word ->PF word (explode(letters))) WordList
    //[ "?"; "?"; "?" ]


// wordsWithScores:
//
// Finds all Scrabble words in the Scrabble dictionary that can be 
// spelled with the given letters.  The words are then "scored"
// based on the value of each letter, and the results returned as
// a list of tuples in the form (word, score).  The list is ordered
// in descending order by score; if 2 words have the same score,
// they are ordered in alphabetical order.
//
// Example:  letters = "tca" returns the list
//   [("act",5); ("cat",5); ("at",2); ("ta",2)]
//
//valueforChar function matches with each character and assings the value 
//similar to shift fucntion done in week 8
let ValueForChar char = 
    match char with
    |'a' -> 1
    |'e' -> 1
    |'i' -> 1
    |'l' -> 1
    |'n' -> 1
    |'o' -> 1
    |'r' -> 1
    |'s' -> 1
    |'t' -> 1
    |'u' -> 1
    |'d' -> 2
    |'g' -> 2
    |'b' -> 3
    |'c' -> 3
    |'m' -> 3
    |'p' -> 3
    |'f' -> 4
    |'h' -> 4
    |'v' -> 4
    |'w' -> 4
    |'y' -> 4
    |'k' -> 5
    |'j' -> 8
    |'x' -> 8
    |'q' -> 10
    |'z' -> 10
    |_ -> -1000 //large neg score means we have a bug

//finds the value of the word 
//countumutates the value of all the words 
//this function returns the sum of the char values in the list 
let rec FindValue word count = 
    match word with
    |[] -> List.sum(count)
    |hd::tl -> FindValue tl (count@[ValueForChar hd])

//implementing tuple for the word x,y 
let rec TupleForWords words count=
    match words with 
    | [] -> List.rev(count)
    | hd::tl -> 
                let l1 = explode hd 
                TupleForWords tl [(hd, FindValue (l1) [])]@count
//need to sort the elements in the list 
let customSort elem =
    let level1sort = List.sortBy(fun (x,y) -> x) elem
    let level2sort = List.sortBy(fun (x,y) -> -y) level1sort
    level2sort

let wordsWithScores letters =
    customSort(TupleForWords (possibleWords letters) [])

// wordsThatFitPattern:
// Finds all Scrabble words in the Scrabble dictionary that can be 
// spelled with the given letters + the letters in the pattern, such
// that those words all fit into the pattern.  The results are 
// returned as a list of tuples (word, score), in descending order by
// score (with secondary sort on the word in alphabetical order).
//
// Example:  letters = "tca" and pattern = "e**h" returns the list
//   [("each",9); ("etch",9); ("eath",7)]
//
let wordsThatFitPattern letters pattern = 
  [ ("?", -1); ("?", -2); ("?", -3) ]

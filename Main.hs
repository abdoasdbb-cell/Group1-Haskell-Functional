module Main where

import Data.List (intercalate)

-- Define a type alias for a Grade, making the code easier to read
type Grade = Float

-- ===============================================
-- 1. DATA STRUCTURE (Algebraic Data Type - ADT)
--    This is the functional equivalent of a 'struct' in C.
-- ===============================================

data Student = Student
  { studentName  :: String
  , studentAge   :: Int
  , studentGrade :: Grade
  } deriving (Show)

-- The Roster is a simple list of Students
type Roster = [Student]


-- ===============================================
-- 2. CORE FUNCTIONS (PURE FUNCTIONS)
--    Note: These functions do *not* modify the list. They take an old list
--    and return a brand new list (immutability).
-- ===============================================

-- | Function to add a student to the roster.
-- It takes a Student and a Roster, and returns a new Roster.
addStudent :: Student -> Roster -> Roster
addStudent newStudent currentRoster = newStudent : currentRoster
-- The ':' operator (cons) efficiently adds an element to the front of a list.

-- | Function to convert a Student into a formatted String for display.
formatStudent :: Student -> String
formatStudent s =
  "| " ++ take 15 (studentName s ++ replicate 15 ' ') ++
  " | " ++ show (studentAge s) ++
  " | " ++ show (studentGrade s) ++ " |"

-- | Function to display the entire Roster.
displayRoster :: Roster -> String
displayRoster [] = "Roster is empty."
displayRoster roster = header ++ content
  where
    headerLine = replicate 35 '-'
    header = "\n" ++ headerLine ++ "\n" ++ "| Name            | Age | Grade |\n" ++ headerLine
    -- map applies 'formatStudent' to every Student in the list
    -- intercalate joins the resulting strings with a newline character
    content = intercalate "\n" (map formatStudent roster) ++ "\n" ++ headerLine


-- ===============================================
-- 3. MAIN IO LOOP (THE IMPURE PART)
--    Haskell separates pure logic from side-effects (like printing/input).
-- ===============================================

main :: IO ()
main = do
  putStrLn "--- Haskell Student Manager (Functional Paradigm) ---"

  -- Initialize the roster (immutable data)
  let initialRoster :: Roster
      initialRoster = []

  putStrLn "\n--- Initial Roster ---"
  putStrLn (displayRoster initialRoster)

  -- Create two new Student records
  let alice = Student {studentName = "Alice", studentAge = 20, studentGrade = 4.5}
  let bob   = Student {studentName = "Bob", studentAge = 19, studentGrade = 3.0}

  -- Add Alice (results in a new roster: rosterA)
  let rosterA = addStudent alice initialRoster
  putStrLn "\n--- Roster after adding Alice ---"
  putStrLn (displayRoster rosterA)

  -- Add Bob (results in a new roster: finalRoster)
  let finalRoster = addStudent bob rosterA
  putStrLn "\n--- Roster after adding Bob ---"
  putStrLn (displayRoster finalRoster)

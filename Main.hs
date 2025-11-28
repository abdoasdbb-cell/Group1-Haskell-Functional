module Main where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- Define a type alias for a Grade, making the code easier to read
type Grade = Float

-- ===============================================
-- 1. DATA STRUCTURE (Algebraic Data Type - ADT)
-- ===============================================

data Student = Student
  { studentName  :: String
  , studentAge   :: Int
  , studentGrade :: Grade
  } deriving (Show, Eq) -- Added 'Eq' to allow comparison (needed for Delete)

-- The Roster is a simple list of Students
type Roster = [Student]


-- ===============================================
-- 2. CORE FUNCTIONS (PURE FUNCTIONS)
-- ===============================================

-- | Function to add a student to the roster (returns a *new* roster).
addStudent :: Student -> Roster -> Roster
addStudent newStudent currentRoster = newStudent : currentRoster

-- | Function to remove a student by name (uses recursion/filter, returns a *new* roster).
deleteStudent :: String -> Roster -> Roster
deleteStudent nameToDelete currentRoster = filter (\s -> studentName s /= nameToDelete) currentRoster
-- 'filter' is a pure function that returns a new list containing only the elements
-- for which the condition (studentName /= nameToDelete) is True.

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
-- 3. STATISTICS FUNCTIONS (PURE FUNCTIONS)
-- ===============================================

-- | Function to calculate the average grade of the roster.
calculateAverageGrade :: Roster -> Grade
calculateAverageGrade [] = 0.0 -- Handles the empty list case
calculateAverageGrade roster = totalGrade / count
  where
    -- 'map' pulls all grades; 'sum' aggregates them
    grades = map studentGrade roster
    totalGrade = sum grades
    -- 'fromIntegral' converts the integer length to a float for division
    count = fromIntegral (length grades)

-- | Function to calculate the average age of the roster.
calculateAverageAge :: Roster -> Float
calculateAverageAge [] = 0.0
calculateAverageAge roster = totalAge / count
  where
    ages = map studentAge roster
    totalAge = fromIntegral (sum ages)
    count = fromIntegral (length ages)


-- ===============================================
-- 4. MAIN IO LOOP (THE IMPURE PART - DEMONSTRATION)
-- ===============================================

main :: IO ()
main = do
  putStrLn "--- Haskell Student Manager (Functional Paradigm) ---"

  let initialRoster :: Roster
      initialRoster = []

  -- 1. Create three new Student records (immutable data)
  let alice   = Student {studentName = "Alice", studentAge = 20, studentGrade = 4.5}
  let bob     = Student {studentName = "Bob", studentAge = 19, studentGrade = 3.0}
  let charlie = Student {studentName = "Charlie", studentAge = 21, studentGrade = 4.0}

  -- 2. Build the final roster through sequential, immutable transformations:
  let rosterA = addStudent alice initialRoster
  let rosterB = addStudent bob rosterA
  let rosterC = addStudent charlie rosterB

  putStrLn "\n--- Current Roster (3 Students) ---"
  putStrLn (displayRoster rosterC)

  -- 3. Demonstrate statistics
  putStrLn "\n--- Statistics Calculation ---"
  let avgG = calculateAverageGrade rosterC
  let avgA = calculateAverageAge rosterC
  putStrLn $ "Total Students: " ++ show (length rosterC)
  putStrLn $ "Average Grade: " ++ show avgG
  putStrLn $ "Average Age: " ++ show avgA ++ " years"
  putStrLn "----------------------------"

  -- 4. Demonstrate deletion (creates a NEW roster: rosterD)
  putStrLn "\n--- Demonstrating Deletion (Removing Bob) ---"
  let rosterD = deleteStudent "Bob" rosterC
  putStrLn "Bob has been removed (functionally)."

  putStrLn "\n--- Roster After Deletion ---"
  putStrLn (displayRoster rosterD)
  
  -- Final check of statistics on the smaller roster
  putStrLn "\n--- Statistics Check on Roster D (Alice & Charlie) ---"
  putStrLn $ "New Total Students: " ++ show (length rosterD)

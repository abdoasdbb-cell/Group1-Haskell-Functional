module Main where

import Data.List (intercalate)
import Data.Char (toLower)

-- Define a type alias for a Grade, making the code easier to read
type Grade = Float

-- ===============================================
-- 1. DATA STRUCTURE (Algebraic Data Type - ADT)
-- ===============================================

data Student = Student
  { studentName  :: String
  , studentAge   :: Int
  , studentGrade :: Grade
  } deriving (Show, Eq)

-- The Roster is a simple list of Students
type Roster = [Student]


-- ===============================================
-- 2. CORE FUNCTIONS (PURE FUNCTIONS)
-- ===============================================

-- | Function to add a student to the roster (returns a *new* roster).
addStudent :: Student -> Roster -> Roster
addStudent newStudent currentRoster = newStudent : currentRoster

-- | Function to remove a student by name (returns a *new* roster).
deleteStudent :: String -> Roster -> Roster
deleteStudent nameToDelete currentRoster = filter (\s -> studentName s /= nameToDelete) currentRoster

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
    content = intercalate "\n" (map formatStudent roster) ++ "\n" ++ headerLine

-- ===============================================
-- 3. SEARCH/FILTER FUNCTION (PURE FUNCTION)
-- ===============================================

-- | Searches the roster by name (case-insensitive and partial match).
-- Returns a *new* list containing only matching students.
searchStudentsByName :: String -> Roster -> Roster
searchStudentsByName query roster = filter matchesQuery roster
  where
    -- Convert query and student name to lowercase for case-insensitive matching
    lowerQuery = map toLower query
    matchesQuery student = lowerQuery `elem` map toLower (studentName student)

-- ===============================================
-- 4. STATISTICS FUNCTIONS (PURE FUNCTIONS)
-- ===============================================

-- | Function to calculate the average grade of the roster.
calculateAverageGrade :: Roster -> Grade
calculateAverageGrade [] = 0.0
calculateAverageGrade roster = totalGrade / count
  where
    grades = map studentGrade roster
    totalGrade = sum grades
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
-- 5. MAIN IO LOOP (THE IMPURE PART - FINAL DEMONSTRATION)
-- ===============================================

main :: IO ()
main = do
  putStrLn "--- Haskell Student Manager (Functional Paradigm) ---"

  let initialRoster :: Roster
      initialRoster = []

  -- 1. Create four new Student records
  let alice   = Student {studentName = "Alice", studentAge = 20, studentGrade = 4.5}
  let bob     = Student {studentName = "Bob", studentAge = 19, studentGrade = 3.0}
  let charlie = Student {studentName = "Charlie", studentAge = 21, studentGrade = 4.0}
  let alison  = Student {studentName = "Alison", studentAge = 20, studentGrade = 3.5}

  -- 2. Build the final roster
  let rosterA = addStudent alice initialRoster
  let rosterB = addStudent bob rosterA
  let rosterC = addStudent charlie rosterB
  let finalRoster = addStudent alison rosterC

  putStrLn "\n--- Full Roster (4 Students) ---"
  putStrLn (displayRoster finalRoster)

  -- 3. Demonstrate the new search function
  let searchQ = "ali"
  let searchResults = searchStudentsByName searchQ finalRoster

  putStrLn $ "\n--- Demonstrating Search: Query '" ++ searchQ ++ "' ---"
  putStrLn $ "Found " ++ show (length searchResults) ++ " student(s):"
  putStrLn (displayRoster searchResults)
  
  -- 4. Demonstrate Deletion and Stats
  putStrLn "\n--- Demonstrating Deletion (Removing Bob) & Stats ---"
  let rosterWithoutBob = deleteStudent "Bob" finalRoster
  
  putStrLn $ "New Total Students: " ++ show (length rosterWithoutBob)
  putStrLn $ "Average Grade (No Bob): " ++ show (calculateAverageGrade rosterWithoutBob)
  putStrLn "----------------------------"

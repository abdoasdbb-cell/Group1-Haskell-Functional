# Functional Data Processor (Haskell) 💡

## Project Overview
This is **Project 2** for Group 1, designed to demonstrate the **Functional Programming Paradigm** using the Haskell language.

The application manages basic student records (`Name`, `Age`, `Grade`) using purely functional concepts:
* **Immutability:** Data structures (`Roster`) are never changed. Operations like adding or deleting students result in a **new** list being created.
* **Pure Functions:** All core logic (`addStudent`, `calculateAverageGrade`, `deleteStudent`, `searchStudentsByName`) is side-effect free, ensuring reliability.
* **List Transformation:** Core features are implemented using higher-order functions like `map`, `filter`, and `sum` instead of imperative loops or mutable variables.

## Getting Started: Installation & Setup 🛠️

To compile and run this program, you need the **Haskell Platform** installed, which includes the **GHC (Glasgow Haskell Compiler)**.

1.  **Save the Code:** Ensure the source code is saved as `Main.hs` in your project folder.
2.  **Compile:** Open your terminal and use the GHC command to compile the executable:
    ```bash
    ghc Main.hs
    ```
3.  **Run:** Execute the compiled file:
    ```bash
    ./Main
    ```
    *(Alternatively, you can run it immediately using the interpreter: `runghc Main.hs`)*

## Examples of Execution

The demonstration below shows adding four students, calculating statistics, running a search query, and demonstrating deletion by creating a new, smaller roster.

### Scenario: Full Feature Demonstration

```bash
$ ./Main
--- Haskell Student Manager (Functional Paradigm) ---

--- Full Roster (4 Students) ---
-----------------------------------
| Name            | Age | Grade |
-----------------------------------
| Alison          | 20 | 3.5 |
| Charlie         | 21 | 4.0 |
| Bob             | 19 | 3.0 |
| Alice           | 20 | 4.5 |
-----------------------------------

--- Demonstrating Search: Query 'ali' ---
Search uses a case-insensitive, partial match.
Found 2 student(s):

-----------------------------------
| Name            | Age | Grade |
-----------------------------------
| Alison          | 20 | 3.5 |
| Alice           | 20 | 4.5 |
-----------------------------------

--- Demonstrating Deletion (Removing Bob) & Stats ---
Bob has been removed (functionally, by creating a new list).

New Total Students: 3
Average Grade (No Bob): 4.0 years
----------------------------

--- Roster After Deletion ---
-----------------------------------
| Name            | Age | Grade |
-----------------------------------
| Alison          | 20 | 3.5 |
| Charlie         | 21 | 4.0 |
| Alice           | 20 | 4.5 |
-----------------------------------

# Functional Data Processor (Haskell) 💡

## Project Overview
This is **Project 2** for Group 1, designed to demonstrate the **Functional Programming Paradigm** using the Haskell language.

Unlike the imperative C project, this application focuses on **immutability** (data structures are never changed) and **pure functions** (functions rely only on their inputs and produce no side effects). State changes, like adding a student, are performed by creating and returning a *new* data structure.

The project currently implements the fundamental data structures and the functional equivalent of adding records to the roster.

## Core Functional Concepts
* **Algebraic Data Types (ADTs):** Used to define the `Student` record, similar to `structs` in C.
* **Pure Functions:** All logic (like `addStudent` and `displayRoster`) is side-effect free, meaning they only perform calculations and never handle I/O (printing, saving, input).
* **Recursion & List Comprehension:** Future functions (like calculating statistics) will be implemented using recursive logic or built-in list functions (`map`, `filter`, `foldr`) instead of traditional loops.

## Getting Started: Installation & Setup 🛠️

To compile and run this program, you need the **Haskell Platform** installed, which includes the **GHC (Glasgow Haskell Compiler)**.

1.  **Save the Code:** Ensure the source code is saved as `Main.hs` in your project folder.
2.  **Compile:** Open your terminal and use the GHC command to compile the executable:
    ```bash
    ghc Main.hs
    ```
3.  **Run:** Execute the compiled file (the executable will be named `Main`):
    ```bash
    ./Main
    ```
    *(Alternatively, you can run it immediately using the interpreter: `runghc Main.hs`)*

## Examples of Execution

This demonstration shows how the immutable Roster is updated by creating a new list (`rosterA` and `finalRoster`) at each step.

```bash
$ ./Main
--- Haskell Student Manager (Functional Paradigm) ---

--- Initial Roster ---
Roster is empty.

--- Roster after adding Alice ---
-----------------------------------
| Name            | Age | Grade |
-----------------------------------
| Alice           | 20 | 4.5 |
-----------------------------------

--- Roster after adding Bob ---
-----------------------------------
| Name            | Age | Grade |
-----------------------------------
| Bob             | 19 | 3.0 |
| Alice           | 20 | 4.5 |
-----------------------------------

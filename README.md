Copyright Szabo Cristina-Andreea 2023-2024

# **Suffix Tree**

## **Overview**
This project implements a **Suffix Tree** in **Racket**, focusing on efficient string manipulation, searching, and pattern matching. The **Suffix Tree** is a data structure widely used in fields such as **bioinformatics, text indexing, data compression, and natural language processing**. It also includes a **stream-based suffix tree**, optimized for handling large-scale datasets by processing input incrementally rather than loading everything into memory at once.

## **Project Scope**
The main goals of this project are:
- **Efficient Suffix Tree Construction** – Constructing a suffix tree for a given text with optimized space and time complexity.
- **Fast Pattern Matching** – Enabling quick substring searches within large datasets.
- **Memory Optimization** – Implementing a stream-based suffix tree to handle large inputs incrementally.
- **Performance Improvements** – Iterative enhancements across multiple development stages.

## **Features**
### **1. Suffix Tree Construction**
The **Suffix Tree** is built using an optimized algorithm that compresses repeated substrings, reducing redundancy and improving lookup times. Each node represents a suffix of the input string, facilitating **O(n) complexity** for key operations such as searching, substring matching, and finding the longest repeated substring.

### **2. Pattern Matching & String Processing**
- **Fast Substring Search** – Determines whether a given pattern exists in the text efficiently.
- **Longest Common Substring** – Finds the longest repeated sequence across multiple texts.
- **Suffix Array Integration** – Provides an optimized representation for large-scale text indexing.

### **3. Stream-Based Optimization**
Instead of processing the entire text at once, the **stream-based suffix tree** processes data incrementally, making it efficient for large texts that cannot fit into memory. This approach is particularly useful for **bioinformatics applications, web crawling, and real-time data analysis**.

### **Applications**
- **Text Processing & Search Engines** – Enables fast text searching, substring identification, and autocomplete functionality.
- **Data Compression** – Detects repeated patterns to optimize data storage.
- **Plagiarism Detection** – Efficiently compares multiple documents to identify similarities.

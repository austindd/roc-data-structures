# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About This Repository

This is a Roc package that provides useful data structures including:
- Red-Black Trees (self-balancing binary search trees)
- AVL Trees (self-balancing binary search trees)
- Linked Lists (singly-linked lists)
- ListMap (map backed by a list with compaction)

The package is exported as `package/main.roc` which exposes: `AvlTree`, `AvlTreeNum`, `ListMap`, `RbTree`, `LinkedList`.

## Key Architecture

### Module Structure Pattern

The codebase follows a consistent three-layer architecture for tree-based data structures:

1. **Base Module** (`*Base.roc`): Contains the core recursive type definition and implementation logic
   - `AvlTreeBase`: Defines the recursive AVL tree structure with `Empty`, `Node`, and `Leaf` variants
   - `RbTreeBase`: Defines the recursive Red-Black tree structure with color-tagged nodes
   - `LinkedListBase`: Defines the recursive linked list structure with `Cons` and `Nil`
   - These modules implement all the algorithms (insertion, balancing, traversal, etc.)

2. **Generic Wrapper** (`AvlTree.roc`, `RbTree.roc`, `LinkedList.roc`): User-facing module requiring keys to implement `Ord`
   - Wraps the base implementation in an opaque type
   - Keys must implement the `Ord` ability (custom ordering via `compare`)
   - Provides standard map operations: `empty`, `insert`, `get`, `map`, `walk`, `to_list`, `from_list`

3. **Numeric Specialization** (`*Num.roc`): Convenience wrapper for numeric keys
   - `AvlTreeNum` and `RbTreeNum` accept raw numeric keys directly
   - Internally wraps numbers in a `NumKey` opaque type that implements `Ord` using `Num.compare`
   - Simplifies usage when keys are always numbers (no need to manually implement `Ord`)

### The Ord Ability

`package/Ord.roc` defines the `Ord` ability used for ordering tree keys:
- `Ordering : [EQ, LT, GT]` - comparison result type
- `Ord` ability requires implementing `compare : a, a -> Ordering`
- All tree-based structures require keys to implement `Ord` for ordering/searching

### ListMap Implementation

`ListMap` is a simple map backed by a list with automatic compaction:
- Stores entries as `List (Result (a, b) {})`
- Uses tombstones (`Err {}`) for deletions
- Automatically compacts when tombstone count exceeds threshold (255)
- Simpler than tree structures but with O(n) lookup time

## Development Commands

### Testing
Roc uses inline `expect` blocks for testing. Tests are embedded within the module files themselves.

To test a specific module (if you have `roc` installed):
```bash
roc test package/AvlTree.roc
roc test package/RbTree.roc
roc test package/LinkedList.roc
```

To test all modules:
```bash
roc test package/*.roc
```

### Building/Type Checking
To check the package builds correctly:
```bash
roc check package/main.roc
```

## Coding Conventions

- Use opaque types (types prefixed with `@`) for wrapping internal implementations
- Implement abilities (`Eq`, `Ord`, `Inspect`, etc.) for opaque types as needed
- Use pattern matching (`when ... is`) for control flow
- Prefer explicit type annotations for public functions
- Use `Result` types for fallible operations (e.g., `get` returns `Result v {}`)
- Tree structures maintain their invariants automatically through rotation/balancing logic in Base modules

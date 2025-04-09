# Test Plan for Malgo

## Error Case Testing

### Parser Errors
- Add tests to verify that the parser correctly identifies and reports syntax errors.
- Include cases such as:
  - Missing parentheses.
  - Missing keywords or symbols.
  - Invalid identifiers or operators.

### Rename Errors
- Add tests to ensure the renaming phase catches and reports:
  - Undefined variables.
  - Undefined types.
  - Duplicate definitions.
  - Misuse of constructors as variables or vice versa.
  - Incorrectly qualified names (e.g., using a module that is not imported).
  - Missing type variables in type definitions.

### Type Inference Errors
- Add tests to validate that type inference fails gracefully for:
  - Type mismatches.
  - Ambiguous types (e.g., unresolved type variables).
  - Invalid type applications (e.g., applying a non-function type).
  - Incorrect arity in type constructors.
  - Conflicting type annotations and inferred types.

### Refinement Errors
- Add tests to confirm that refinement errors are handled, such as:
  - Invalid transformations.
  - Missing dependencies.
  - Non-exhaustive pattern matching.
  - Overlapping patterns in clauses.
  - Invalid type instantiations (e.g., instantiating a type variable with an incompatible type).
  - Incorrect record field access or updates.
  - Misuse of tuple or record patterns (e.g., mismatched arity or missing fields).

### Evaluation Errors
- Add tests to check that runtime errors are reported, including:
  - Division by zero.
  - Undefined function calls.
  - Invalid input/output operations.
  - Type mismatches in function applications.
  - Accessing non-existent fields in records.
  - Pattern match failures.
  - Invalid primitive operations (e.g., unsupported types or arity mismatches).
  - Infinite recursion or stack overflow.

### Golden Tests for Errors
- Create golden files for each error case to ensure consistent error messages across changes.

### Error Case File Organization
- Store error case test files in `test/${Module path}/errors/`.
- Store corresponding golden files in `.golden/${Module name}/errors/`.
- Use descriptive filenames for test to indicate the type of error being tested.

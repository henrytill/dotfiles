; Custom C fold queries - fold bodies only, keeping declarations/conditions visible
; This file should take precedence over nvim-treesitter's default

; Function bodies (not the signature)
(function_definition
  body: (compound_statement) @fold)

; Control flow statement bodies (not the condition/header)
(if_statement
  consequence: (compound_statement) @fold)

(else_clause
  (compound_statement) @fold)

(for_statement
  body: (compound_statement) @fold)

(while_statement
  body: (compound_statement) @fold)

(do_statement
  body: (compound_statement) @fold)

(switch_statement
  body: (compound_statement) @fold)

; Struct and enum bodies (not the declaration)
(struct_specifier
  body: (field_declaration_list) @fold)

(enum_specifier
  body: (enumerator_list) @fold)

; These fold the entire construct (no separate body to target)
[
  (case_statement)
  (comment)
  (preproc_if)
  (preproc_elif)
  (preproc_else)
  (preproc_ifdef)
  (preproc_function_def)
  (initializer_list)
  (gnu_asm_expression)
  (preproc_include)+
] @fold

; Nested compound statements
(compound_statement
  (compound_statement) @fold)

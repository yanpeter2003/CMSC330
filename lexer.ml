open TokenTypes
open Str

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let length = String.length input in 
  let rec tok pos =
    if pos >= length then
      []
    else
      (*Put regex for negative int first to avoid conflict with parentheses and subtraction*)
      (*Tok_Int*)
      if Str.string_match (Str.regexp "(-[0-9]+)") input pos then
        let value = Str.matched_string input in 
        Tok_Int(int_of_string (String.sub value 1 ((String.length value) - 2)))::(tok (pos + String.length value))

      else if Str.string_match (Str.regexp "[0-9]+") input pos then
        let value = Str.matched_string input in 
        Tok_Int(int_of_string value)::(tok (pos + String.length value))

      (*Tok_Arrow*)
      else if Str.string_match (Str.regexp "->") input pos then
        Tok_Arrow::(tok (pos + 2))

      (*Tok_LParen*)
      else if Str.string_match (Str.regexp "(") input pos then
        Tok_LParen::(tok (pos + 1))

      (*Tok_RParen*)
      else if Str.string_match (Str.regexp ")") input pos then
        Tok_RParen::(tok (pos + 1))  

      (*Tok_Equal*)
      else if Str.string_match (Str.regexp "=") input pos then
        Tok_Equal::(tok (pos + 1))

      (*Tok_NotEqual*)
      else if Str.string_match (Str.regexp "<>") input pos then 
        Tok_NotEqual::(tok (pos + 2)) 

      (*Tok_Greater*)
      else if Str.string_match (Str.regexp ">") input pos then
        Tok_Greater::(tok (pos + 1))

      (*Tok_Less*)
      else if Str.string_match (Str.regexp "<") input pos then 
        Tok_Less::(tok (pos + 1)) 

      (*Tok_GreaterEqual*)
      else if Str.string_match (Str.regexp ">=") input pos then 
        Tok_GreaterEqual::(tok (pos + 2)) 

      (*Tok_LessEqual*)
      else if Str.string_match (Str.regexp "<=") input pos then 
        Tok_LessEqual::(tok (pos + 2)) 

      (*Tok_Or*)
      else if Str.string_match (Str.regexp "||") input pos then 
        Tok_Or::(tok (pos + 2)) 

      (*Tok_And*)
      else if Str.string_match (Str.regexp "&&") input pos then 
        Tok_And::(tok (pos + 2)) 

      (*Tok_Add*)
      else if Str.string_match (Str.regexp "\\+") input pos then
        Tok_Add::(tok (pos + 1)) 

      (*Tok_Mult*)
      else if Str.string_match (Str.regexp "\\*") input pos then
        Tok_Mult::(tok (pos + 1))

      (*Tok_Sub*)
      else if Str.string_match (Str.regexp "\\-") input pos then
        Tok_Sub::(tok (pos + 1))

      (*Tok_Div*)
      else if Str.string_match (Str.regexp "\\/") input pos then
        Tok_Div::(tok (pos + 1)) 

      (*Tok_Concat*)
      else if Str.string_match (Str.regexp "\\^") input pos then
        Tok_Concat::(tok (pos + 1)) 

      (*Tok_ID*)
      else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
        let value = Str.matched_string input in
        (*Tok_Not*)
        if String.equal value "not" then 
          Tok_Not::(tok (pos + 3))
        (*Tok_If*)
        else if String.equal value "if" then 
          Tok_If::(tok (pos + 2))
        (*Tok_Then*)  
        else if String.equal value "then" then 
          Tok_Then::(tok (pos + 4))
        (*Tok_Else*)
        else if String.equal value "else" then 
          Tok_Else::(tok (pos + 4))
        (*Tok_Let*)
        else if String.equal value "let" then
          Tok_Let::(tok (pos + 3))
        (*Tok_Rec*)
        else if String.equal value "rec" then
          Tok_Rec::(tok (pos + 3))
        (*Tok_In*)
        else if String.equal value "in" then
          Tok_In::(tok (pos + 2))
        (*Tok_Def*)
        else if String.equal value "def" then
          Tok_Def::(tok (pos + 3))
        (*Tok_Fun*)
        else if String.equal value "fun" then
          Tok_Fun::(tok (pos + 3))
        (*Tok_Bool*)
        else if String.equal value "true" then
          Tok_Bool(true)::(tok (pos + 4))
        else if String.equal value "false" then
          Tok_Bool(false)::(tok (pos + 5))
        (*Check if actually is Tok_ID*)
        else
          Tok_ID(value)::(tok (pos + String.length value))

      (*Tok_DoubleSemi*)
      else if Str.string_match (Str.regexp ";;") input pos then
        Tok_DoubleSemi::(tok (pos + 2))

      (*Tok_String*)
      else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then
        let value = Str.matched_string input in 
        Tok_String(String.sub value 1 ((String.length value) - 2))::(tok (pos + String.length value)) 
      
      else
        (*Check for whitespace*)
        if Str.string_match (Str.regexp " ") input pos then
          tok (pos + 1)
        (*Error case if it's not whitespace*)
        else 
          raise (InvalidInputException("Invalid character"))
  in tok 0;;

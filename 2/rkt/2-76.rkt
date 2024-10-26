; Explicit Dispatch --
; Adding new types requires creating new selectors and a constructor 
;   which have to purposely avoid namespace collisions with other selectors/constructors
; Creating new operations involves updating the conditionals that dispatch on type
;
; Data-Directed Dispatch--
; Adding new types and operations requires defining new selectors and a constructor
;   within an installer function and binding them with (get) to an
;   operation symbol and list of type symbols
;
; Message-Passing Style--
; Adding new types requires creating a constructor which returns a data object

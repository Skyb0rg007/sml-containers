
structure Test: TEST =
struct

structure Span =
   struct
      datatype t = T of {
            file: string,
            startLine: int,
            startColumn: int,
            endLine: int,
            endColumn: int
         }
   end

datatype cover = Cover | NoCover

datatype 'a label = Label of {
      name: string,
      loc: Span.t option,
      minimum: real,
      annot: 'a
   }

datatype log =
   Annotation of Span.t option * string
 | Footnote of string
 | Label of cover label

type journal = log list

datatype diff = Diff of {(* TODO *)}

exception TestFailure of Span.t option * string * diff option

end

(* vim: set tw=0 ts=3 sw=3: *)

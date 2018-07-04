#lang scribble/manual

@title{csv-writing}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket "main.rkt"))

This collection provides a simple set of routines for writing
comma-separated-values (CSV) files.

There is no well-specified generally-agreed-upon format for
CSV files, but there are a set of conventions, and this library
tries to cleave to those conventions.

At the same time, it does provide a number of simple parameters that
you can adjust in order to customize the less-well-agreed-upon
aspects of serialization. One obvious design question is how to
allow you to specify values for these parameters without decorating
every function with a raft of optional parameters. I haven't found
a completely satisfactory answer, but—in somewhat the style of
Neil Van Dyke's csv-reading library—you get a reasonable default,
and a way to specify a printing-parameters structure that customizes
printing if you need it.

Also, users should note that in general, the CSV representation
loses information; the string @racket["TRUE"], the symbol @racket['TRUE],
and the boolean value @racket[#t] all have (by default) the same
printed representation, the string @racket["TRUE"]. Users may certainly
specify custom procedures for the printing of booleans, symbols, strings,
or all values, in order to address this in whatever style makes sense
for their particular application.

@defmodule[csv-writing]{
  The @racket[csv-writing] module provides all of the functions
      defined in the library.

}

@defproc[(table? [val any?]) boolean?]{
 Returns @racket[true] if the argument is a table.
         
 The representation of a table is simply a list of lists of values.

 The rows do not all need to be the same length.
}

@defproc[(display-table [table table?]
                        [port output-port? (current-output-port)]
                        [#:printing-params printing-params
                         csv-printing-params?
                         default-csv-printing-params])
         void?]{
  Given a table, an optional port, and an optional @racket[csv-printing-params?],
 display the table on the given port, using the printing-params to determine how
 to print.

 For instance,

 @codeblock|{
(display-table '((name title)
                 ("joey" bottle-washer)
                 ("margo" sign-painter 34)))}|

 Produces

 @verbatim{
name,title
joey,bottle-washer
margo,sign-painter,34}

 as output.

 The documentation of the @racket[csv-printing-params] structure
 provides information on how to customize the printing.
}

@defproc[(table->string [table table?]
                        [#:printing-params printing-params
                         csv-printing-params?
                         default-csv-printing-params])
         string?]{
 Similar to @racket[display-table], but returns a string rather
 than displaying anything to an output port.
}

@defproc[(table-row->string [row list?]
                            [#:printing-params printing-params
                         csv-printing-params?
                         default-csv-printing-params])
         string?]{
  Given a list of values, return its representation as a CSV line (string).

  So, for instance:

  @code|{(table-row->string '(342 bc "def" #t))}|

  ... produces the string @racket["342,bc,def,TRUE"].

  As before, the @racket[printing-params] can be used to customize the printing of values.
}

@defproc[(table-cell->string [cell any?]
                             [printing-params csv-printing-params? default-csv-printing-params])
         string?]{
 given a value, return its representation as a CSV cell (string).
}

@defproc[(make-csv-printing-params
          [#:table-cell->string table-cell->string procedure? default-table-cell->string]
          [#:string-cell->string string-cell->string procedure? default-string-cell->string]
          [#:number-cell->string number-cell->string procedure? default-number-cell->string]
          [#:boolean-cell->string boolean-cell->string procedure? default-boolean-cell->string]
          [#:symbol-cell->string symbol-cell->string procedure? default-symbol-cell->string]
          [#:quotes-only-when-needed? quotes-only-when-needed? boolean? #t]
          [#:quoted-double-quote quoted-double-quote string? "\"\""]
          )
         csv-printing-params?]{
 This function is a convenience function to simplify the specification of
 custom printing parameters. All of its keyword arguments are set by default
 to the default values, so overriding just one function can be done without
 specifying all of the other parameters.

 The @racket[table-cell->string] procedure controls the translation of
 cell values to strings. Here's a simple (and mostly useless) example:

 @codeblock|{
  (display-table '((a b) (c d))
                 #:printing-params
                 (make-csv-printing-params
                  #:table-cell->string (λ (str) "X")))}|

 This produces the output

 @verbatim{
X,X
X,X}

 The default @racket[table-cell->string] procedure dispatches to
 customizable printing procedures for strings, numbers, symbols,
 and booleans, and signals an error for all other kinds of data.
 If the user provides a different procedure for
 @racket[table-cell->string], then the values of procedures such
 as @racket[boolean-cell->string] will be irrelevant, since they
 won't be called.

 Put differently, overriding @racket[table-cell->string] is the
 ``nuclear option'', indicating that you just want all of the
 default procedures to get the heck out of the way.

 The @racket[string-cell->string] procedure is called by the default
 @racket[table-cell->string] function to map strings
 to CSV values. So, for instance:

  @codeblock|{
  (display-table '(("ab" 34) ("cd" 2))
                 #:printing-params
                 (make-csv-printing-params
                 #:string-cell->string string-upcase))}|

  ...would produce the output:

  @verbatim{
AB,34
DC,2
 }

 The default @racket[string-cell->string] procedure maps strings
 to themselves, unless they contain newlines, commas, or double-quotes,
 in which case it wraps them in double-quotes, and quotes
 double-quotes using the default quoted-double-quote.
 

 The @racket[quotes-only-when-needed?] parameter is true by
 default; if set to false, then all strings are wrapped in
 double-quotes, regardless of whether they contain dangerous
 characters.

 The @racket[quoted-double-quote] parameter is the string that
 is used in place of a double-quote that appears in a table
 cell.

 The @racket[number-cell->string] procedure is called by the
 default @racket[table-cell->string] procedure to effect the translation
 of numbers to CSV cells.

 The default @racket[number-cell->string] procedure uses @racket[~r]
 to produce strings for rational numbers, and signals an error
 otherwise.

 The @racket[symbol-cell->string] procedure
 is called by the
 default @racket[table-cell->string] procedure
 to effect the translation
 of symbols to CSV cells. By default, it simply maps symbols to
 strings using @racket[symbol->string] and then calls the
 default @racket[string-cell->string] procedure. (Specifying a
 custom @racket[string-cell->string] procedure will not affect the
 behavior of the default @racket[symbol-cell->string] procedure.)

 The @racket[boolean-cell->string] procedure
 is called by the
 default @racket[table-cell->string] procedure
 to effect the translation of booleans to CSV cells.
 By default, it produces @racket["TRUE"]
 and @racket["FALSE"].

}

@section{Suggestions Etc.}

The goal is for this library to be useful to other people.
Please, by all means report problems. Pull requests are especially
welcome.


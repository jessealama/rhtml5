html5
=====

This library is intended to provide a Racket implementation of the [official HTML5 parsing algorithm][html5-parsing].

[html5-parsing]: https://html.spec.whatwg.org/multipage/syntax.html#parsing "Parsing HTML documents"

# Current status (2017-02-24)

The current focus is encoding detection from byte strings, which is an essential preliminary step toward a full implementation of the algorithm.

Construction of parse trees is not yet supported.

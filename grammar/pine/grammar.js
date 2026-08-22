/// pine: Common Lisp as pine's reader sees it.
///
/// pine installs reader macros for `{}` maps, `[]` seqs, `#{}` sets, `/a/b`
/// paths and `${form}` interpolation, so a file that declares
/// `(in-readtable pine.path:syntax)` is not Common Lisp any more. It is a
/// superset in everything but one character: `/` is a terminating macro
/// character here, so `(/ a b)` is a path where plain CL reads division. That
/// is why this is its own grammar and not a widening of the one it extends.
///
/// Structure stops where pine's own `%read-token` stops (src/path.lisp). A
/// segment is one token the highlighter classifies, because a half-typed
/// `/buf/?` has to degrade to a segment rather than an ERROR that poisons the
/// form around it. `${form}` and `[k = v]` hold arbitrary lisp, so they are
/// real rules.
///
/// No external scanner: the guix package compiles parser.c alone.

const commonlisp = require('../grammar');

// the symbol characters of the grammar this extends, less `/`
const SYMBOL_HEAD =
    /[^:\f\n\r\t ()\[\]{}"^;/`\\,#'\u000B\u001C\u001D\u001E\u001F\u2028\u2029\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2008\u2009\u200a\u205f\u3000]/;

const SYMBOL_BODY = choice(SYMBOL_HEAD, /[#']/);

const SYMBOL = token(seq(SYMBOL_HEAD, repeat(SYMBOL_BODY)));

// what %read-token keeps: it stops at / [ ] and the terminators. A {a,b} group
// or a {1..9} range is part of the segment, which is how %token-segment reads
// it, so it is inside the token rather than a rule of its own.
const SEG_GROUP = seq('{', repeat(/[^{}\n]/), '}');
const SEG_CHAR = choice(/[^/\[\]{}()"'`,;\f\n\r\t ]/, SEG_GROUP);
// $ and # dispatch at the start of a segment -- ${form} is an interpolation and
// #{a b} an alternation -- so a plain segment may not begin with either, or the
// lexer takes the whole of one as a segment and neither rule ever fires
const SEG_HEAD = /[^/\[\]{}()"'`,;$#\f\n\r\t ]/;
const PATH_SEGMENT = token.immediate(seq(SEG_HEAD, repeat(SEG_CHAR)));

module.exports = grammar(commonlisp, {
    name: 'pine',

    rules: {
        // `/` terminates a token under pine's readtable, so a symbol cannot
        // hold one and `:pine/wayland` really is a keyword then a path
        sym_lit: _ => seq(SYMBOL),

        _form: ($, original) =>
            choice(original, $.map_lit, $.seq_lit, $.ns_path, $.unquote_lit),

        map_lit: $ =>
            seq(field('open', '{'),
                repeat(choice(field('value', $._form), $._gap)),
                field('close', '}')),

        // vec_lit is taken: the grammar this extends redefines it as #(...)
        seq_lit: $ =>
            seq(field('open', '['),
                repeat(choice(field('value', $._form), $._gap)),
                field('close', ']')),

        // ${form} and $@{form} where a value is wanted. The marker is one token
        // so that `$` on its own stays an ordinary symbol character.
        unquote_lit: $ =>
            seq(field('marker', choice(token('$@{'), token('${'))),
                repeat($._gap),
                field('value', $._form),
                repeat($._gap),
                field('close', '}')),

        ns_path: $ =>
            prec.right(seq('/',
                optional($._path_part),
                repeat(seq(token.immediate('/'), optional($._path_part))))),

        // token.immediate is what forbids whitespace between segments, so
        // `/a /b` is two paths and `/a\n/b` is two paths, as the reader has it
        _path_part: $ =>
            seq(choice($.path_interpolation, $.path_alternation, $.path_segment),
                repeat($.path_constraint)),

        path_segment: _ => PATH_SEGMENT,

        path_interpolation: $ =>
            seq(field('marker', choice(token.immediate('$@{'), token.immediate('${'))),
                repeat($._gap),
                field('value', $._form),
                repeat($._gap),
                field('close', '}')),

        // #{a b} inside a path is alternation, a set of segment names, which is
        // a different meaning from #{} in a value position
        path_alternation: $ =>
            seq(token.immediate('#{'),
                repeat(choice(alias(token.immediate(/[^{},\f\n\r\t ]+/),
                                    $.path_segment),
                              ',',
                              $._ws)),
                field('close', '}')),

        // [pred] and [name = pattern]: the right-hand side is read as data.
        // extras is empty in this grammar family, so every gap is explicit
        path_constraint: $ =>
            seq(token.immediate('['),
                optional(field('key', alias(token.immediate(/[^=\]\f\n\r\t ]+/),
                                            $.path_key))),
                repeat($._gap),
                optional(seq('=',
                             repeat($._gap),
                             field('value', $._form),
                             repeat($._gap))),
                field('close', ']')),
    },
});
